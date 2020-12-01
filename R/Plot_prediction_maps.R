# Plot prediction maps
# 9/22/2020
#JGM

library (raster)
library (viridis)
library (maps)
library (mapdata)
library (gridExtra)


# centroids df, so I can look at centroid, warm edge, cool edge?
load ("Data/centroids_Borm14_alltemp_allscenarios.RData")


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

plot_cm_maps_fun("Gadus_morhua")

# look at monthly differences for predictions----
plot_monthly_maps_fun <- function (sci_name, model) {
  # use "hist" for historical or 4-letter cm abbreviation 
  if (model == "hist") {
    br <- brick (paste0("Models/Prediction_bricks/", sci_name, "Borm_14_alltemp_2000_2018.grd"))
    month_index <- seq ()
    stackApply....
  }
  
}



# Function to plot historical, 245, and 585 in a panel
plot_pred_maps_fun <- function (sci_name, model_name) {
  
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
  
  #spp_breaks <- quantile (getValues(overall_vals), probs = c(seq (0, .9, 0.1), 0.95, 0.99, 1), na.rm = TRUE)
  
  # add centroid?
  centroid <- centroid_change %>%
    filter (species == sci_name) %>%
    group_by (scenario) %>%
    summarize_at (vars(-model), mean)
    
  
  png (paste0("Figures/Thermpred_map_", sci_name, "_Borm_14_alltemp.png"), width = 16, height = 9, units = "in", res = 300)
  
  par (mfrow = c (1, 3))
  plot (log(mn_hist), 
        #breaks = spp_breaks, col = viridis(length(spp_breaks) -1),
        #breaks = quantile (getValues(overall_vals), probs = c (0, 0.25, .5, .75, 1), na.rm = TRUE), 
        col = viridis(25),
        xlim = c (-32, -3), ylim = c (60, 69),
        zlim = col_lims,
        cex.main = 2,
        legend = FALSE,
        main = paste0(sci_name, " historical"))
  map('worldHires',add=TRUE, col='grey90', fill=TRUE)
  
  # add centroid of distribution and triangles for warm and cold edges
  points (x = centroid$hist_lon[1], y = centroid$hist_lat[1], pch = 21, bg = "white", col = "black", cex = 2)
  points (x = centroid$hist_lon[1], y = centroid$hist_warm[1], pch = 2, col = "white", cex = 2)
  points (x = centroid$hist_lon[1], y = centroid$hist_cold[1], pch = 6, col = "white", cex = 2)
  
  
  
  plot (log(mn_245), 
        col = viridis(25),
        xlim = c (-32, -3), ylim = c (60, 69),
        zlim = col_lims,
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

plot_pred_maps_fun (sci_name = "Anarhichas_lupus", 
                    model_name = "Borm_14_alltemp")

plot_pred_maps_fun (sci_name = "Argentina_silus", 
                    model_name = "Borm_14_alltemp")

plot_pred_maps_fun (sci_name = "Eutrigla_gurnardus", 
                    model_name = "Borm_14_alltemp")

plot_pred_maps_fun (sci_name = "Micromesistius_poutassou", 
                    model_name = "Borm_14_alltemp")

plot_pred_maps_fun (sci_name = "Pollachius_virens", 
                    model_name = "Borm_14_alltemp")

plot_pred_maps_fun (sci_name = "Lepidorhombus_whiffiagonis", 
                    model_name = "Borm_14_alltemp")

plot_pred_maps_fun (sci_name = "Pleuronectes_platessa", 
                    model_name = "Borm_14_alltemp")


plot_pred_maps_fun (sci_name = "Melanogrammus_aeglefinus", 
                    model_name = "Borm_14_alltemp")

plot_pred_maps_fun (sci_name = "Scomber_scombrus", 
                    model_name = "Borm_14_alltemp")

plot_pred_maps_fun (sci_name = "Mallotus_villosus", 
                    model_name = "Borm_14_alltemp")

plot_pred_maps_fun (sci_name = "Lophius_piscatorius", 
                    model_name = "Borm_14_alltemp")


plot_pred_maps_fun (sci_name = "Phycis_blennoides", 
                    model_name = "Borm_14_alltemp")
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
