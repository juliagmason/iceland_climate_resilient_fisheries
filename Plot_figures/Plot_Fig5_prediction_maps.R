# Plot prediction maps
# 9/22/2020
#JGM


# Used this for Figure 5

library (tidyverse)
library (raster)
library (rasterVis)
library (viridis)
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
  med_hist <- calc (br_hist, median)

  # for predictions, have to calculate an ensemble mean. Find all future files, all climate models
  file_names <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name, "_", GAM, ".*", scenario, "_2061_2080.grd"), full.names = TRUE)
  
  br_pred <- stack (file_names)
  
  # take a mean
  med_pred <- calc (br_pred, median, na.rm = TRUE)
  
  # calculate difference between future and historical
  diff_pred <- overlay (med_pred, med_hist, 
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


# compile all maps and plot from list ----
plot_predmaps_from_list <- function (predmap, sci_name, GAM) {
  
  map_title <- paste0 (sci_name, ", ", GAM, " response predictions")
  plot (predmap + ggtitle (map_title))
  
}

load ("Models/spp_Borm.RData")

system.time (rug_predmap_ls_full <- lapply (spp_borm, plot_diff_maps_2scen, GAM = "Rug_tw_LL")); beep() 

pdf (file = "Figures/Rug_tw_predmaps.pdf")
mapply (plot_predmaps_from_list, rug_predmap_ls_full, sci_name = borm_suit, GAM = "Rugosity Tweedie")
dev.off()


# Figure 5 multipanel habitat difference ----

sci_name_vec <- c("Gadus_morhua", "Trisopterus_esmarkii", "Lophius_piscatorius", "Cyclopterus_lumpus")
p_pred <- lapply (sci_name_vec, plot_diff_maps_2scen, GAM = "Rug_tw_LL") 


ggsave ("Figures/Fig5a_gm_diff.eps", p_pred[[1]], width = 170, height = 85, units = "mm", dpi= 300)
ggsave ("Figures/Fig5b_te_diff.eps", p_pred[[2]], width = 170, height = 85, units = "mm", dpi= 300)
ggsave ("Figures/Fig5c_lp_diff.eps", p_pred[[3]], width = 170, height = 85, units = "mm", dpi= 300)
ggsave ("Figures/Fig5d_cl_diff.eps", p_pred[[4]], width = 170, height = 85, units = "mm", dpi= 300)


# add phylopic silhouette? 
# https://cran.r-project.org/web/packages/rphylopic/readme/README.html
# https://stackoverflow.com/questions/60917778/how-do-i-plot-an-image-from-phylopic-in-top-right-corner-of-my-ggplot-graph-in-r

# plot differences, all periods for supplement ----


# show baseline historical habitat
plot_hist_hab <- function (sci_name) {
  
  br_hist <- brick (paste0("Models/Prediction_bricks/Rug_tw_depth_crop", sci_name, "_Rug_tw_LL_2000_2018.grd"))
  
  # calculate a mean, so just one layer
  med_hist <- calc (br_hist, median)
  
  p_hist <- 
    gplot (med_hist) +
    geom_raster (aes (fill = value)) +
    borders (fill = "grey90", col = "black", 
             xlim = c (-32, -3), ylim = c (60, 69)) +
    coord_quickmap (xlim = c (-32, -3), ylim = c (60, 69)) +
    scale_fill_viridis ( na.value = "white") +
    labs (fill = "Suitable \nthermal habitat") +
    theme_bw () +
    theme (axis.title = element_blank()) +
    ggtitle ("Suitable thermal habitat, historical (2000-2018)")
  
  
  # save plot to folder to avoid memory issues
  saveRDS(p_hist, file = paste0("Figures/SI_tmp/", sci_name, "_hist.RDS"))
}

lapply (sort(borm_spp$sci_name_underscore), plot_hist_hab)


# predictions

#  helper function for plotting each period
calc_diff_maps_ts <- function (sci_name, scenario, period) {
  # sci_name is all lowercase with underscore, e.g. gadus_morhua
  # scenario is 245 or 585
  # period is first year, 2021, 2041, 2061, 2081. will be in file name
  
  
  # load bricks, which have a layer for each month and year
  br_hist <- brick (paste0("Models/Prediction_bricks/Rug_tw_depth_crop", sci_name, "_Rug_tw_LL_2000_2018.grd"))
  
  # calculate a mean, so just one layer
  med_hist <- calc (br_hist, median)
  
  # load predictions, stack, and take ensemble mean
  
  file_names <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name, "_Rug_tw_LL", ".*", scenario, ".*", period, ".*", ".grd"), full.names = TRUE)
  
  br_pred <- stack (file_names)
  
  med_pred <- calc (br_pred, median, na.rm = TRUE)
  
  # calculate difference
  diff_pred <- overlay (med_pred, med_hist, 
                        fun = function (r1, r2) {return (r1 - r2)}
  )
} # end function 

# plot projections
plot_pred_hab <- function (sci_name) {
  
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
  
  # save to folder
  saveRDS (p_future, file = paste0 ("Figures/SI_tmp/", sci_name, "_future.RDS"))
}


lapply (sort(borm$sci_name_underscore), plot_pred_hab)


# load and plot from SI_tmp ----
plot_maps_allper <- function (sci_name) {
  p_hist <- readRDS (paste0("Figures/SI_tmp/", sci_name, "_hist.RDS"))
  p_future <- readRDS (paste0("Figures/SI_tmp/", sci_name, "_future.RDS"))
  
  comm_name <- spp_list$Common_name[which (spp_list$species == sci_name)]
  
  # for clarity, make an object for scientific name without underscore
  title_name <- str_replace (sci_name, "_", " ")
  
  # print plot, using patchwork
 p_hist / p_future + 
    plot_layout (heights = c (0.7, 1)) +
    plot_annotation (
      title = 
        bquote(.(comm_name)*","~italic(.(title_name))),
      theme = theme (plot.title = element_text (size =20))
    ) 
  
}



suit_spp <- borm_spp %>%
  filter (species != c("Squalus_acanthias", "Icelus_bicornis"))


pdf ("Figures/FigSI2_habitat_diff_maps.pdf")
lapply (sort(suit_spp$sci_name_underscore), plot_maps_allper)
dev.off()
beep()