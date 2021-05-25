# Plot prediction maps
# 9/22/2020
#JGM




# plot habitat difference ----
# Used this for Figure 4

library (tidyverse)

library (raster)
library (rasterVis)
library (viridis)

# library (grid)
# library (gridExtra)
library (patchwork)
library (stringr)

library (beepr)

spp_list  <- read_csv ("Data/species_eng.csv",
                                   col_types = cols(
                                     Spp_ID = col_factor()
                                   )) %>%
  rename (species = sci_name_underscore) 


# for the difference maps, I would want to make sure the colors are centered around zero. Looks like this is possible with ggplot but not sure about base R. 

# RasterVis: 
#https://stackoverflow.com/questions/33750235/plotting-a-raster-with-the-color-ramp-diverging-around-zero

# ggplot:
# https://stackoverflow.com/questions/50805606/use-viridis-and-map-values-to-colour-in-a-histogram-plot
# https://stackoverflow.com/questions/33748871/raster-map-with-discrete-color-scale-for-negative-and-positive-values-r

# return a gplot object plotting the difference between the ensemble prediction mean and historical


# first a function for calculating the predicted diff 245, or diff 585. Returns a raster layer of predicted difference for either scenario
calc_diff_maps <- function (sci_name, scenario, GAM) {
  # sci_name is all lowercase with underscore, e.g. gadus_morhua
  # scenario is 245, 585
  # GAM is GAM model name/folder name--Borm_14_alltemp, etc.
  
  # load historical brick, which has a layer for each month and year
  br_hist <- brick (paste0("Models/Prediction_bricks/", sci_name, "_", GAM, "_", "2000_2018.grd"))
  
  # calculate a mean, so just one layer
  mn_hist <- calc (br_hist, mean)

  # for predictions, have to calculate an ensemble mean. Find all future files, all climate models
  file_names <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name, "_", GAM, ".*", scenario, "_2061_2080.grd"), full.names = TRUE)
  
  br_pred <- stack (file_names)
  
  # take a mean
  mn_pred <- calc (br_pred, mean, na.rm = TRUE)
  
  # calculate difference between future and historical
  diff_pred <- overlay (mn_pred, mn_hist, 
                        fun = function (r1, r2) {return (r1 - r2)}
  )

} # end function

# next layer: plot both scenarios for each species, with diverging colorbar
plot_diff_maps_2scen <- function (sci_name, GAM) {
  # sci_name is all lowercase with underscore, e.g. gadus_morhua
  # GAM is GAM model name/folder name--Borm_14_alltemp, etc.
  
  scenario <- c (245, 585)
  
  # apply above function to calculate difference for both scenarios
  diff_ls <- lapply (scenario, calc_diff_maps, sci_name = sci_name, GAM = GAM)
  diff_br <- brick (diff_ls)
  
  # fix scenario names for labeller
  scen_labs <- setNames ( c("SSP 2-4.5", "SSP 5-8.5"), names (diff_br))
  
  # add silhouette
  #://cran.r-project.org/web/packages/rphylopic/readme/README.html
  
  # plot
  gplot (diff_br) +
    geom_raster (aes (fill = value)) +
    borders (fill = "grey90", col = "black", 
             xlim = c (-32, -3), ylim = c (60, 69)) +
    coord_quickmap (xlim = c (-32, -3), ylim = c (60, 69)) +
    facet_wrap (~variable, labeller = labeller (variable = scen_labs)) +
    # diverging colorscale with white middle. Using values from BrBG R colorbrewer
    scale_fill_gradient2 (midpoint = 0, low = "#8C510A", mid = "#F5F5F5", high = "#01665E", na.value = "white") +
    labs (fill = "Habitat \ndifference") +
    theme_bw () +
    theme (axis.title = element_blank()) #+
    # add_phylopic (img, x = -10, y = 62, alpha = 1, ysize = 5)
    # annotation_custom (cod2, xmin=, xmax=4, ymin=7.5, ymax=Inf)
}


# Figure 4 multipanel habitat difference ----

sci_name_vec <- c("Gadus_morhua", "Hippoglossoides_platessoides", "Merlangius_merlangus", "Cyclopterus_lumpus")
p_pred <- lapply (sci_name_vec, plot_diff_maps_2scen, GAM = "Borm_14_alltemp") 

plaice <- plot_diff 

ggsave ("Figures/Fig4a_gm_diff.eps", p_pred[[1]], width = 170, height = 85, units = "mm", dpi= 300)
ggsave ("Figures/Fig4b_lrd_diff.eps", p_pred[[2]], width = 170, height = 85, units = "mm", dpi= 300)
ggsave ("Figures/Fig4c_whit_diff.eps", p_pred[[3]], width = 170, height = 85, units = "mm", dpi= 300)
ggsave ("Figures/Fig4d_lf_diff.eps", p_pred[[4]], width = 170, height = 85, units = "mm", dpi= 300)


# add phylopic silhouette? 
# https://cran.r-project.org/web/packages/rphylopic/readme/README.html
# https://stackoverflow.com/questions/60917778/how-do-i-plot-an-image-from-phylopic-in-top-right-corner-of-my-ggplot-graph-in-r

# plot differences, all periods for supplement ----

#  helper function for plotting each period
calc_diff_maps_ts <- function (sci_name, scenario, period) {
  # sci_name is all lowercase with underscore, e.g. gadus_morhua
  # scenario is 245 or 585
  # period is first year, 2021, 2041, 2061, 2081. will be in file name

  
  # load bricks, which have a layer for each month and year
  br_hist <- brick (paste0("Models/Prediction_bricks/", sci_name, "_Borm_14_alltemp_2000_2018.grd"))
  
  # calculate a mean, so just one layer
  mn_hist <- calc (br_hist, mean)
  
  # load predictions, stack, and take ensemble mean
  file_names <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name, ".*", scenario, ".*", period, ".*", ".grd"), full.names = TRUE)
  
  br_pred <- stack (file_names)
  
  mn_pred <- calc (br_pred, mean, na.rm = TRUE)
  
  # calculate difference
  diff_pred <- overlay (mn_pred, mn_hist, 
                       fun = function (r1, r2) {return (r1 - r2)}
                       )
} # end function 


# Supplemental figure: habitat difference for all periods and scenarios
# ideally produce a pdf with each page labeled with the species name and the plot
plot_diff_maps_ts <- function (sci_name) {
  
  diff_plot_input <- expand_grid (
    sci_name = sci_name,
    scenario = c (245, 585),
    period = c(2021, 2041, 2061, 2081)) %>%
    as.list()
  
    # apply calc_diff function 
    diff_plot_ls <- pmap (diff_plot_input, calc_diff_maps_ts) # about 5.5 mins
    
    # convert to raster brick
    diff_plot_br <- brick (diff_plot_ls)
    
    # set names for labeller to indicate the prediction period
    period_labs <- setNames (c("SSP 2-4.5: 2021-2040", "2041-2060", "2061-2080", "2081-2100", "SSP 5-8.5: 2021-2040", "2041-2060", "2061-2080", "2081-2100"), names (diff_plot_br))
    
  # plot
  
  # png (paste0("Figures/Thermpred_Diff_map_allperiods_", sci_name, ".png"), width = 8, height = 4, res = 150, unit = "in")
  
  p_future <- gplot (diff_plot_br) +
    geom_raster (aes (fill = value)) +
    borders (fill = "grey90", col = "black", 
             xlim = c (-32, -3), ylim = c (60, 69)) +
    coord_quickmap (xlim = c (-32, -3), ylim = c (60, 69)) +
    facet_wrap (~variable, ncol = 4, 
                labeller = labeller (variable = period_labs)) +
    # colorscale from brewer.pal (9, "PuBuGn")
    scale_fill_gradient2 (midpoint = 0, low = "#8C510A", mid = "#F5F5F5", high = "#01665E", na.value = "white") +
    labs (fill = "Habitat \ndifference") +
    ggtitle ("Difference in projected future suitable thermal habitat") +
    theme_bw () +
    theme (axis.title = element_blank()) 
  
  # also show historical habitat
  br_hist <- brick (paste0("Models/Prediction_bricks/", sci_name, "_Borm_14_alltemp_2000_2018.grd"))
  
  # calculate a mean, so just one layer
  mn_hist <- calc (br_hist, mean)
  

  #p_hist <- readRDS ("Figures/SI_tmp/Gadus_morhua_hist.RDS")
  p_hist <- 
    gplot (mn_hist) +
      geom_raster (aes (fill = value)) +
      borders (fill = "grey90", col = "black", 
               xlim = c (-32, -3), ylim = c (60, 69)) +
      coord_quickmap (xlim = c (-32, -3), ylim = c (60, 69)) +
      # .~ does "all", ~variable does (getValues(x)), as_labller ("Historical) does NA
      #facet_wrap (~variable, labeller = labeller (variable = hist_lab), nrow = 1) +
      scale_fill_viridis ( na.value = "white") +
      labs (fill = "Suitable \nhabitat") +
      theme_bw () +
      theme (axis.title = element_blank()) +
      ggtitle ("Suitable thermal habitat, historical (2000-2018)")
  
  
  # start saving these to a folder because it's dumb to have this fail so many times
  saveRDS(p_hist, file = paste0("Figures/SI_tmp/", sci_name, "_hist.RDS"))
  saveRDS (p_future, file = paste0 ("Figures/SI_tmp/", sci_name, "_future.RDS"))
  # common and species name for overall plot title
  # grab common name for clearer title
  comm_name <- spp_list$Common_name[which (spp_list$species == sci_name)]
 # for clarity, make an object for scientific name without underscore
  title_name <- str_replace (sci_name, "_", " ")
  
  # print plot, using patchwork
  #print(
 
    
    p_hist / p_future + 
      plot_layout (heights = c (0.7, 1)) +
      plot_annotation (
        title = 
          bquote(.(comm_name)*","~italic(.(title_name))),
        theme = theme (plot.title = element_text (size =20))
      )
    
    
  
  #)
  #dev.off()

  
}# end function 



# compile a pdf of all the species I modeled
load ("Models/spp_Borm_suit.RData")

pdf ("Figures/FigSI2_habitat_diff_maps.pdf")
lapply (sort(borm_suit), plot_diff_maps_ts)
dev.off()
beep()



# takes about 4.5 minutes per species
 
###########################################
# Additional options not used in publication ----



# Function to plot historical, 245, and 585 predictions in a panel ----

library (tidyverse)
library (raster)
library (viridis)
library (maps)
library (mapdata)
library (gridExtra)


library (beepr)

# Note--maps breaks when mgcv is loaded (I think). also conflict with tidyverse?

# centroids df, so I can look at centroid, warm edge, cool edge?
load ("Data/centroids_Borm14_alltemp_allscenarios.RData")

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
  
  
  # add centroid
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

comm_spp <- c("Brosme_brosme", "Microstomus_kitt", "Merlangius_merlangus", "Limanda_limanda", "Glyptocephalus_cynoglossus", "Sebastes_marinus", "Anarchicus_minor", "Clupea_harengus", "Scomber_scombrus")

purrr::map (comm_spp, plot_pred_maps_fun); beep()


# look at differences among CM predictions----

# plot standard deviation of projections ----

calc_SD_maps <- function (sci_name, scenario, GAM) {
  # sci_name is all lowercase with underscore, e.g. gadus_morhua
  # scenario is 245, 585
  # GAM is GAM model name/folder name--Borm_14_alltemp, etc.

  
  # for predictions, have to calculate an ensemble mean. Find all future files, all climate models
  file_names <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name, "_", GAM, ".*", scenario, "_2061_2080.grd"), full.names = TRUE)
  
  br_pred <- stack (file_names)
  
  # calculate standard deviation of the ensemble
  sd_pred <- calc (br_pred, sd, na.rm = TRUE)
  
  
} # end function

plot_SD_maps_2scen <- function (sci_name, GAM) {
  # sci_name is all lowercase with underscore, e.g. gadus_morhua
  # GAM is GAM model name/folder name--Borm_14_alltemp, etc.
  
  scenario <- c (245, 585)
  
  # apply above function to calculate difference for both scenarios
  sd_ls <- lapply (scenario, calc_SD_maps, sci_name = sci_name, GAM = GAM)
  sd_br <- brick (sd_ls)
  
  # fix scenario names for labeller
  scen_labs <- setNames ( c("SSP 2-4.5", "SSP 5-8.5"), names (sd_br))
  
  # add silhouette
  #://cran.r-project.org/web/packages/rphylopic/readme/README.html
  
  # plot
  gplot (sd_br) +
    geom_raster (aes (fill = value)) +
    borders (fill = "grey90", col = "black", 
             xlim = c (-32, -3), ylim = c (60, 69)) +
    coord_quickmap (xlim = c (-32, -3), ylim = c (60, 69)) +
    facet_wrap (~variable, labeller = labeller (variable = scen_labs)) +
    # diverging colorscale with white middle. Using values from BrBG R colorbrewer
     scale_fill_distiller (palette = "BuGn", direction = 1, na.value = "white") +
    labs (fill = "Projection standard deviation") +
    theme_bw () +
    theme (axis.title = element_blank()) #+
  # add_phylopic (img, x = -10, y = 62, alpha = 1, ysize = 5)
  # annotation_custom (cod2, xmin=, xmax=4, ymin=7.5, ymax=Inf)
}

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
  
 # plot
  
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

