# Plot prediction maps
# 9/22/2020
#JGM

library (tidyverse)
library (raster)
library (viridis)
library (maps)
library (mapdata)
library (gridExtra)

library (beepr)

# Note--maps breaks when mgcv is loaded (I think)


# centroids df, so I can look at centroid, warm edge, cool edge?
load ("Data/centroids_Borm14_alltemp_allscenarios.RData")


# Function to plot historical, 245, and 585 in a panel ----
plot_pred_maps_fun <- function (sci_name) {
  
  # load bricks, which have a layer for each month and year
  br_hist <- brick (paste0("Models/Prediction_bricks/", sci_name, "_Borm_14_alltemp_2000_2018.grd"))
  
  # load predictions and stack to get mean and sd
  files_245 <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name, ".*245_2061_2080.grd"), full.names = TRUE)
  files_585 <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name, ".*585_2061_2080.grd"), full.names = TRUE)
  
  br_245 <- stack (files_245)
  # does it make sense to take an overall mean and sd of all the models, all the months? this captures more of the variation than taking a mean for each model and sd btw models, but the standard deviation could be within model, not among models. 
  br_585 <- stack (files_585) # 23s
  
  # calculate a mean, so just one layer
  mn_hist <- calc (br_hist, mean)
  
  mn_245 <- calc (br_245, mean, na.rm = TRUE) # 36s
 # med_245 <- calc (br_245, median) # 36s
 # sd_245 <- calc (br_245, sd) # 20s
  mn_585 <- calc (br_585, mean, na.rm = TRUE)
  
  
  # make one standard color bar based on full range of values
  overall_vals <- stack (mn_hist, mn_245, mn_585)
  col_lims <- c (min (log(getValues (overall_vals)), na.rm = TRUE), max (log(getValues (overall_vals)), na.rm = TRUE))
  

  # add centroid?
  centroid <- centroid_change %>%
    filter (species == sci_name) %>%
    group_by (scenario) %>%
    summarize_at (vars(-model), mean)
    
  
  png (paste0("Figures/Thermpred_map_", sci_name, "_Borm_14_alltemp.png"), width = 16, height = 9, units = "in", res = 300)
  
  par (mfrow = c (1, 3))
  plot (log(mn_hist), 
        col = viridis(25),
        xlim = c (-32, -3), ylim = c (60, 69),
        zlim = col_lims,
        cex.main = 2,
        legend = FALSE,
        main = paste0(sci_name, " historical"))
  maps::map('worldHires', add=TRUE, col='grey90', fill=TRUE)

  
  # add centroid of distribution and triangles for warm and cold edges
  points (x = centroid$hist_lon[1], y = centroid$hist_lat[1], pch = 21, bg = "white", col = "black", cex = 2)
  points (x = centroid$hist_lon[1], y = centroid$hist_warm[1], pch = 2, col = "white", cex = 2)
  points (x = centroid$hist_lon[1], y = centroid$hist_cold[1], pch = 6, col = "white", cex = 2)
  
  
  
  plot (log(mn_245), 
        col = viridis(25),
        xlim = c (-32, -3), ylim = c (60, 69),
       # zlim = col_lims,
        cex.main = 2,
        legend = FALSE,
        main = paste0(sci_name, " 245"))
  map('worldHires',add=TRUE, col='grey90', fill=TRUE)
  points (x = centroid$pred_lon[1], y = centroid$pred_lat[1], pch = 21, bg = "white", col = "black", cex = 2)
  points (x = centroid$pred_lon[1], y = centroid$pred_warm[1], pch = 2, col = "white", cex = 2)
  points (x = centroid$pred_lon[1], y = centroid$pred_cold[1], pch = 6, col = "white", cex = 2)
  
  plot (log(mn_585), 
        col = viridis(25),
        xlim = c (-32, -3), ylim = c (60, 69),
        zlim = col_lims,
        cex.main = 2,
        main = paste0(sci_name, " 585"))
  map('worldHires',add=TRUE, col='grey90', fill=TRUE)
  points (x = centroid$pred_lon[2], y = centroid$pred_lat[2], pch = 21, bg = "white", col = "black", cex = 2)
  points (x = centroid$pred_lon[2], y = centroid$pred_warm[2], pch = 2, col = "white", cex = 2)
  points (x = centroid$pred_lon[2], y = centroid$pred_cold[2], pch = 6, col = "white", cex = 2)
  
  dev.off()
}

# takes a bit over a minute, 68s
system.time(plot_pred_maps_fun (sci_name = "Gadus_morhua", 
                    model_name = "Borm_14_alltemp")); beep()

plot_pred_maps_fun (sci_name = "Cyclopterus_lumpus", 
                    model_name = "Borm_14_alltemp")

plot_pred_maps_fun (sci_name = "Pleuronectes_platessa", 
                    model_name = "Borm_14_alltemp"); beep()

comm_spp <- c("Brosme_brosme", "Microstomus_kitt", "Merlangius_merlangus", "Limanda_limanda", "Glyptocephalus_cynoglossus", "Sebastes_marinus", "Anarchicus_minor", "Clupea_harengus", "Scomber_scombrus")

purrr::map (comm_spp, plot_pred_maps_fun); beep()

# plot habitat difference ----



# for the difference maps, I would want to make sure the colors are centered around zero. Looks like this is possible with ggplot but not sure about base R. 

# Going with RasterVis for now: 
#https://stackoverflow.com/questions/33750235/plotting-a-raster-with-the-color-ramp-diverging-around-zero

# also would be possible with ggplot:
# https://stackoverflow.com/questions/50805606/use-viridis-and-map-values-to-colour-in-a-histogram-plot
# https://stackoverflow.com/questions/33748871/raster-map-with-discrete-color-scale-for-negative-and-positive-values-r

library (rasterVis)

library(maps) # for plotting country shapefile
library(mapdata)
library(maptools)

library (gridExtra)
library (RColorBrewer)

plot_diff_maps_fun <- function (sci_name) {
  
  # load bricks, which have a layer for each month and year
  br_hist <- brick (paste0("Models/Prediction_bricks/", sci_name, "_Borm_14_alltemp_2000_2018.grd"))
  
  # load predictions and stack to get mean and sd
  files_245 <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name, ".*245_2061_2080.grd"), full.names = TRUE)
  files_585 <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name, ".*585_2061_2080.grd"), full.names = TRUE)
  
  br_245 <- stack (files_245)
  # does it make sense to take an overall mean and sd of all the models, all the months? this captures more of the variation than taking a mean for each model and sd btw models, but the standard deviation could be within model, not among models. 
  br_585 <- stack (files_585) # 23s
  
  # calculate a mean, so just one layer
  mn_hist <- calc (br_hist, mean)
  
  mn_245 <- calc (br_245, mean, na.rm = TRUE) # 36s

  mn_585 <- calc (br_585, mean, na.rm = TRUE)
  
  # calculate difference
  diff_245 <- overlay (mn_245, mn_hist, 
                       fun = function (r1, r2) {return (r1 - r2)}
                       )
  
  diff_585 <- overlay (mn_585, mn_hist, 
                       fun = function (r1, r2) {return (r1 - r2)}
  )
  
 
  # create levelplot objects for hist map and differences, with country polygon
  # add country polygon:
  # https://stackoverflow.com/questions/17582532/r-overlay-plot-on-levelplot

  p_245 <- levelplot (diff_245, margin = F,  
                      xlim = c (-32, -3), ylim = c (60, 69),
                      xlab = NULL, ylab = NULL,
                      main = "Habitat difference, SSP 2-4.5") + 
    layer(sp.polygons(bPols, fill = "white"))
  
  p_585 <- levelplot (diff_585, margin = F,  
                      xlim = c (-32, -3), ylim = c (60, 69),
                      xlab = NULL, ylab = NULL,
                      main = "Habitat difference, SSP 5-8.5") + 
    layer(sp.polygons(bPols, fill = "white"))
  
  # show log habitat for hist
  # for color palettes for rasterViw: https://www.r-graph-gallery.com/27-levelplot-with-lattice.html
  p_hist <- levelplot (log (mn_hist), margin = F, 
                       xlim = c (-32, -3), ylim = c (60, 69), 
                       xlab = NULL, ylab = NULL,
                       col.regions = colorRampPalette (brewer.pal (9, "PuBuGn")),
                       main = "Log (habitat suitability), historical") + 
    layer(sp.polygons(bPols, fill = "white"))
  
  
  
  png (paste0("Figures/Thermpred_Diff_map_", sci_name, "_Borm_14_alltemp.png"), width = 16, height = 9, units = "in", res = 300)
  
  grid.arrange (p_hist, diverge0(p_245, ramp = "BrBG"), 
                diverge0(p_585, ramp = "BrBG"), ncol = 3,
                top = textGrob(str_replace (sci_name, "_", " "), gp = gpar(fontface="italic", fontsize = 20), vjust = 0.7)
  )

  
  dev.off()
}

plot_diff_maps_fun("Pleuronectes_platessa")

# look at differences among CM predictions----
plot_cm_maps_fun <- function (sci_name) {
  
  # find filenames
  files_245 <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name, ".*245_2061_2080.grd"), full.names = TRUE)
  files_585 <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name, ".*585_2061_2080.grd"), full.names = TRUE)
  
  # reorder 585 files so CM26 is at the end
  files_585 <- files_585[c(2:5, 1)]
  
  png (paste0("Figures/Thermpred_map_allCM_", sci_name, ".png"), width = 16, height = 9, res = 300, unit = "in")
  par (mfrow = c (2, 5))
  
  for (file in files_585) {
    br <- brick (file)
    br_mean <- calc (br, mean)
    
    model <- strsplit (file, "_")[[1]][7]
    
    plot (br_mean, 
          #breaks = spp_breaks, col = viridis(length(spp_breaks) -1),
          xlim = c (-32, -3), ylim = c (60, 69),
          cex.main = 2,
          #legend = FALSE,
          main = paste0(model, " 585")
    )
  }
  
  for (file in files_245) {
    br <- brick (file)
    br_mean <- calc (br, mean)
    
    model <- strsplit (file, "_")[[1]][7]
    
    plot (br_mean, 
          #breaks = spp_breaks, col = viridis(length(spp_breaks) -1),
          xlim = c (-32, -3), ylim = c (60, 69),
          cex.main = 2,
          #legend = FALSE,
          main = paste0(model, " 245")
    )
    
  }
  
  dev.off()
  
}

plot_cm_maps_fun("Cyclopterus_lumpus")

# # plot CM variance ----

# use a similar helper function approach to what I did for predict_ensemble_rasters. make one function that calculates a temporal mean for a model and scenario. Then lapply that for a species and calculate SD.
calc_CM_mean <- function (sci_name, CM, scenario) {
  
  br <- brick (list.files(path = "Models/Prediction_bricks/", pattern = paste0(sci_name, ".*", CM, "_", scenario, "_2061_2080.grd"), full.names = TRUE))
  
  br_mn <- calc (br, mean, na.rm = TRUE)
}

plot_cm_varmap_fun <- function (sci_name) {

  # just copy/pasting the scenarios
  
  # model mean input list to feed into calc_CM_mean
  model_mean_ls_245 <- expand.grid (sci_name = sci_name,
                                CM = c("gfdl", "cnrm", "ipsl", "mohc"),
                                scenario = 245)
  
  # apply calc_CM function
  model_means_245 <- pmap (model_mean_ls_245, calc_CM_mean)
  
  # convert list to raster brick
  model_br_245 <- brick (model_means_245)
  
  # calculate SD
  model_sd_245 <- calc (model_br_245, sd, na.rm = TRUE)
  
  # 585
  model_mean_ls_585 <- expand.grid (sci_name = sci_name,
                                    CM = c("gfdl", "cnrm", "ipsl", "mohc", "CM26"),
                                    scenario = 585)
  
  # apply calc_CM function
  model_means_585 <- pmap (model_mean_ls_585, calc_CM_mean)
  
  # convert list to raster brick
  model_br_585 <- brick (model_means_585)
  
  # calculate SD
  model_sd_585 <- calc (model_br_585, sd, na.rm = TRUE)
  
 ## plot
  
  # make one standard color bar based on full range of values
  overall_vals <- stack (model_sd_245, model_sd_585)
  col_lims <- c (min (getValues (overall_vals), na.rm = TRUE), max (getValues (overall_vals), na.rm = TRUE))
  
  png (paste0("Figures/Thermpred_map_CM_SD_", sci_name, "_Borm_14_alltemp.png"), width = 16, height = 9, units = "in", res = 300)
  
  par (mfrow = c (1, 2))
  plot (model_sd_245, 
        col = viridis(25),
        xlim = c (-32, -3), ylim = c (60, 69), zlim = col_lims,
        legend = FALSE,
        cex.main = 2,

        main = paste0(sci_name, " SD, SSP 2-4.5"))
  maps::map('worldHires', add=TRUE, col='grey90', fill=TRUE)
  
  plot (model_sd_585, 
        col = viridis(25),
        xlim = c (-32, -3), ylim = c (60, 69), zlim = col_lims,
        cex.main = 2,

        main = paste0(sci_name, " SD, SSP 5-8.5"))
  maps::map('worldHires', add=TRUE, col='grey90', fill=TRUE)
  
  dev.off()
  
}

plot_cm_varmap_fun ("Gadus_morhua")

# look at monthly differences for predictions----
plot_monthly_maps_fun <- function (sci_name, model) {
  # use "hist" for historical or 4-letter cm abbreviation 
  if (model == "hist") {
    br <- brick (paste0("Models/Prediction_bricks/", sci_name, "Borm_14_alltemp_2000_2018.grd"))
    month_index <- seq ()
    stackApply....
  }
  
}



# test maps ----

# cod and pollock for science deep dive

plot_2_pred_maps_fun <- function (sci_name1, sci_name2) {
  
  # load bricks, which have a layer for each month and year
  br_hist1 <- brick (paste0("Models/Prediction_bricks/", sci_name1, "_Borm_14_alltemp_2000_2018.grd"))
  
  # load predictions and stack to get mean and sd
  files_2451 <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name1, ".*245_2061_2080.grd"), full.names = TRUE)
  files_5851 <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name1, ".*585_2061_2080.grd"), full.names = TRUE)
  
  br_2451 <- stack (files_2451)
  br_5851 <- stack (files_5851) 
  
  # calculate a mean, so just one layer
  mn_hist1 <- calc (br_hist1, mean)
  mn_2451 <- calc (br_2451, mean, na.rm = TRUE) # 36s
  mn_5851 <- calc (br_5851, mean, na.rm = TRUE)
  
  
  # make one standard color bar based on full range of values
  overall_vals1 <- stack (mn_hist1, mn_2451, mn_5851)
  col_lims1 <- c (min (log(getValues (overall_vals1)), na.rm = TRUE), max (log(getValues (overall_vals1)), na.rm = TRUE))
  
  # second species 
  
  # load bricks, which have a layer for each month and year
  br_hist2 <- brick (paste0("Models/Prediction_bricks/", sci_name2, "_Borm_14_alltemp_2000_2018.grd"))
  
  # load predictions and stack to get mean and sd
  files_2452 <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name2, ".*245_2061_2080.grd"), full.names = TRUE)
  files_5852 <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name2, ".*585_2061_2080.grd"), full.names = TRUE)
  
  br_2452 <- stack (files_2452)
  br_5852 <- stack (files_5852) 
  
  # calculate a mean, so just one layer
  mn_hist2 <- calc (br_hist2, mean)
  mn_2452 <- calc (br_2452, mean, na.rm = TRUE) # 36s
  mn_5852 <- calc (br_5852, mean, na.rm = TRUE)
  
  
  # make one standard color bar based on full range of values
  overall_vals2 <- stack (mn_hist2, mn_2452, mn_5852)
  col_lims2 <- c (min (log(getValues (overall_vals2)), na.rm = TRUE), max (log(getValues (overall_vals2)), na.rm = TRUE))
  
  # switch to common name
  comm_name1 <- spp_list$Common_name[which (spp_list$species == sci_name1)]
  comm_name2 <- spp_list$Common_name[which (spp_list$species == sci_name2)]

png (paste("Figures/Thermpred_map", sci_name1, sci_name2, "Borm_14.png", sep = "_"), width = 16, height = 9, units = "in", res = 300)
par (mfrow = c (2, 3))

plot (log(mn_hist1), 
      col = viridis(25),
      xlim = c (-32, -3), ylim = c (60, 69),
      zlim = col_lims1,
      cex.main = 2,
      legend = FALSE,
      main = paste0(comm_name1, " historical"))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (log(mn_2451), 
      col = viridis(25),
      xlim = c (-32, -3), ylim = c (60, 69),
      zlim = col_lims1,
      cex.main = 2,
      legend = FALSE,
      main = paste0(comm_name1, " 245"))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (log(mn_5851), 
      col = viridis(25),
      xlim = c (-32, -3), ylim = c (60, 69),
      zlim = col_lims1,
      cex.main = 2,
      legend = FALSE,
      main = paste0(comm_name1, " 585"))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (log(mn_hist2), 
      col = viridis(25),
      xlim = c (-32, -3), ylim = c (60, 69),
      zlim = col_lims2,
      cex.main = 2,
      legend = FALSE,
      main = paste0(comm_name2, " historical"))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (log(mn_2452), 
      col = viridis(25),
      xlim = c (-32, -3), ylim = c (60, 69),
      zlim = col_lims2,
      cex.main = 2,
      legend = FALSE,
      main = paste0(comm_name2, " 245"))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (log(mn_5852), 
      col = viridis(25),
      xlim = c (-32, -3), ylim = c (60, 69),
      zlim = col_lims2,
      cex.main = 2,
      legend = FALSE,
      main = paste0(comm_name2, " 585"))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)


dev.off()

} # end function 


plot_2_pred_maps_fun("Gadus_morhua", "Hippoglossoides_platessoides")

system.time(plot_2_pred_maps_fun("Gadus_morhua", "Boreogadus_saida"));beep()
 # 201s
 

###
# https://stackoverflow.com/questions/33227182/how-to-set-use-ggplot2-to-map-a-raster
library (rasterVis)
levelplot(mn_hist, 
          margin=FALSE,                       
          colorkey=list(
            space='bottom',                   
            labels=list(at=-5:5, font=4),
            axis.line=list(col='black')       
          ),    
          par.settings=list(
            axis.line=list(col='transparent') 
          ),
          scales=list(draw=FALSE),            
          col.regions=viridis,                   
          at=seq(-5, 5, len=101)) +           
  layer(sp.polygons(iceland, lwd=3))
