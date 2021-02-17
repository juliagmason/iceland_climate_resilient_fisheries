# Plot prediction maps
# 9/22/2020
#JGM

#library (tidyverse)
library (raster)
library (viridis)
library (maps)
library (mapdata)
library (gridExtra)

library (beepr)

# Note--maps breaks when mgcv is loaded (I think). also conflict with tidyverse


# centroids df, so I can look at centroid, warm edge, cool edge?
load ("Data/centroids_Borm14_alltemp_allscenarios.RData")


# Function to plot historical, 245, and 585 predictions in a panel ----
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


# plot habitat difference ----

library (tidyverse)

library (raster)
library (rasterVis)

library (grid)
library (gridExtra)
library (stringr)

library (beepr)


# for the difference maps, I would want to make sure the colors are centered around zero. Looks like this is possible with ggplot but not sure about base R. 

# RasterVis: 
#https://stackoverflow.com/questions/33750235/plotting-a-raster-with-the-color-ramp-diverging-around-zero

# ggplot:
# https://stackoverflow.com/questions/50805606/use-viridis-and-map-values-to-colour-in-a-histogram-plot
# https://stackoverflow.com/questions/33748871/raster-map-with-discrete-color-scale-for-negative-and-positive-values-r

# return a gplot object plotting the difference between the ensemble prediction mean and historical

# multipanel figure for historical 2060 period and species of interest ----
# first a function for calculating the predicted diff 245, or diff 585. Returns a raster layer of predicted difference for either scenario
calc_diff_maps <- function (sci_name, scenario, model) {
  # sci_name is all lowercase with underscore, e.g. gadus_morhua
  # scenario is 245, 585
  
  # load historical brick, which have a layer for each month and year
  br_hist <- brick (paste0("Models/Prediction_bricks/", sci_name, "_", model, "_", "2000_2018.grd"))
  
  # calculate a mean, so just one layer
  mn_hist <- calc (br_hist, mean)

  # for predictions, have to calculate an ensemble mean
  file_names <- list.files (path = "Models/Prediction_bricks/", pattern = paste0(sci_name, "_", model, ".*", scenario, "_2061_2080.grd"), full.names = TRUE)
  
  br_pred <- stack (file_names)
  
  mn_pred <- calc (br_pred, mean, na.rm = TRUE)
  
  # calculate difference
  diff_pred <- overlay (mn_pred, mn_hist, 
                        fun = function (r1, r2) {return (r1 - r2)}
  )

} # end function

# next layer: plot both scenarios for each species, with diverging colorbar
plot_diff_maps_2scen <- function (sci_name, model) {
  
  scenario <- c (245, 585)
  #scenario <- 585
  
  diff_ls <- lapply (scenario, calc_diff_maps, sci_name = sci_name, model = model)
  diff_br <- brick (diff_ls)
  
  # fix names for labeller
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
    scale_fill_gradient2 (midpoint = 0, low = "#8C510A", mid = "#F5F5F5", high = "#01665E", na.value = "white") +
    labs (fill = "Habitat \ndifference") +
    theme_bw () +
    theme (axis.title = element_blank()) +
    add_phylopic (img, x = -10, y = 62, alpha = 1, ysize = 5)
    annotation_custom (cod2, xmin=, xmax=4, ymin=7.5, ymax=Inf)
}

# strip text labelling issue workaround--function for mean historical later
calc_mn_hist_fun <- function (sci_name, model) {
  # load historical brick, which have a layer for each month and year
  br_hist <- brick (paste0("Models/Prediction_bricks/", sci_name, "_", model, "_", "2000_2018.grd"))
  
  # calculate a mean, so just one layer
  mn_hist <- calc (br_hist, mean)
  
  
}

# now grid.arrange the plots for species of interest. Can't figure out how to match historical and future, but can maybe plot 2 with the same height
sci_name_vec <- c("Gadus_morhua", "Hippoglossoides_platessoides","Lophius_piscatorius", "Cyclopterus_lumpus")

#import phylopic
library (rphylopic)
library (RCurl)
library (png)

cod_df <- name_search(text = "Gadus morhua", options = "namebankID")[[1]]
name_get (uuid = cod_df$uid[1])
image_get(uuid = id)
cod <- image_data(cod_df$uid[20], size = 128)[[1]]
ubio_get (namebankID = 180556)
img <- image_data ("bba1800a-dd86-451d-a79b-c5944cfe5231", size = "512")[[1]]
cod2 <- rasterGrob(img, interpolate=TRUE)
cat <- image_data("23cd6aa4-9587-4a2e-8e26-de42885004c9", size = 128)[[1]]

cod_iud <- phylopic_uid

codurl<-"http://phylopic.org/name/a1a2209d-ad95-4817-bd19-bfcd93acf4d5"
codlogo = readPNG(getURLContnt("http://phylopic.org/assets/images/submissions/27f02109-4604-4f10-a95c-73686a59a172.thumb.png"))
cod <- image_data ("a1a2209d-ad95-4817-bd19-bfcd93acf4d5", size = 128)[[1]]
id <- "a1a2209d-ad95-4817-bd19-bfcd93acf4d5"

lump <- "35690EED-2223-41CF-B825-A3B0CD808BD8"
lf <- image_get (http://phylopic.org/assets/images/submissions/27f02109-4604-4f10-a95c-73686a59a172.thumb.png, size = "512")[[1]]
name_get (uuid = id)

ggplot (aes (x = mpg, y = cyl), data = mtcars) +
  geom_point() +
  add_phylopic(codlogo)

# https://scrogster.wordpress.com/2014/06/02/adding-phylopic-org-silhouettes-to-r-plots/
logoing_func<-function(logo, x, y, size){
  dims<-dim(logo)[1:2] #number of x-y pixels for the logo (aspect ratio)
  AR<-dims[1]/dims[2]
  par(usr=c(0, 1, 0, 1))
  rasterImage(logo, x-(size/2), y-(AR*size/2), x+(size/2), y+(AR*size/2), interpolate=TRUE)
}


codlogo = readPNG(getURLContent(codurl), native = T)


p_pred <- lapply (sci_name_vec, plot_diff_maps_2scen, model = "Borm_14_alltemp") 
# https://intellipaat.com/community/23231/how-do-i-arrange-a-variable-list-of-plots-using-grid-arrange

ggsave ("Figures/Fig4_4panel_test.eps", width = 170, height = 350, units = "mm", dip = 300)
do.call ("grid.arrange", c(p_pred, ncol = 1))
dev.off()

ggsave ("Figures/Fig4a_cod_diff_logo.eps", width = 170, height = 85, units = "mm", dpi= 300)
plot (p_pred[[1]])
logoing_func(img, x = 0.10, y = 0.90, size = 0.15)
dev.off()

ggsave ("Figures/Fig4b_lrd_diff.eps", width = 170, height = 85, units = "mm", dpi= 300)
plot (p_pred[[2]])
dev.off()

ggsave ("Figures/Fig4a_mf_diff.eps", width = 170, height = 85, units = "mm", dpi= 300)
plot (p_pred[[3]], )
dev.off()

ggsave ("Figures/Fig4c_whit_diff.eps", width = 170, height = 85, units = "mm", dpi= 300)
plot_diff_maps_2scen(sci_name = "Merlangius_merlangus", model = "Borm_14_alltemp") 
dev.off()

ggsave ("Figures/Fig4a_lf_diff.eps", width = 170, height = 85, units = "mm", dpi= 300)
plot (p_pred[[4]])
dev.off()

ls_hist <- lapply (sci_name_vec, calc_mn_hist_fun)
br_hist <- brick (ls_hist)

# jettison historical. try to facet_grid with multiple species?


# set labeller names as sci_name
hist_lab <- setNames (str_replace (sci_name_vec, "_", " "), names (br_hist))

png ("Figures/histmap_test.png", width = 2, height = 2, units = "in", res = 300)

 gplot (br_hist[[1]]) +
  geom_raster (aes (fill = value)) +
  borders (fill = "grey90", col = "black", 
           xlim = c (-32, -3), ylim = c (60, 69)) +
  coord_quickmap (xlim = c (-32, -3), ylim = c (60, 69)) +
  # .~ does "all", ~variable does (getValues(x)), as_labller ("Historical) does NA
  facet_wrap (~variable, labeller = labeller (variable = hist_lab), ncol = 1) +
  scale_fill_gradient2 (midpoint = 0, low = "#F7FCFD", mid = "#66C2A4", high = "#00441B", na.value = "white") +
  labs (fill = "Suitable \nhabitat") +
  theme_bw () +
  theme (axis.title = element_blank())

dev.off()

plot_diff_maps_3panel <- function (sci_name_vec) {
  # hard code the number for now--sci_name_vec should be length 4
  
  ls_hist <- lapply (sci_name_vec, calc_mn_hist_fun)
  br_hist <- brick (ls_hist)
  
  # # set labeller names as sci_name
  hist_lab <- setNames (str_replace (sci_name_vec, "_", " "), names (br_hist))
  
  p_hist <- gplot (br_hist) +
    geom_raster (aes (fill = value)) +
    borders (fill = "grey90", col = "black", 
             xlim = c (-32, -3), ylim = c (60, 69)) +
    coord_quickmap (xlim = c (-32, -3), ylim = c (60, 69)) +
    # .~ does "all", ~variable does (getValues(x)), as_labller ("Historical) does NA
    facet_wrap (~variable, labeller = labeller (variable = hist_lab), nrow = 1) +
    scale_fill_gradient2 (midpoint = 0, low = "#F7FCFD", mid = "#66C2A4", high = "#00441B", na.value = "white") +
    labs (fill = "Suitable \nhabitat") +
    theme_bw () +
    theme (axis.title = element_blank())
  
 
  
  p_layout <- matrix (c(rep (1, length (sci_name_vec)), 2:(length (sci_name_vec) +1)), ncol = 2, byrow = F)

  
}

tmp <- calc_diff_maps (sci_name = "Gadus_morhua", scenario = "hist")

p_layout <- rbind (c(rep (1, 3), 2:4))
grid.arrange (p_hist, p_pred[[1]] + theme (legend.position = "bottom"), p_pred[[2]] + theme (legend.position = "bottom"), p_pred[[3]] + theme (legend.position = "bottom"), layout_matrix = p_layout, nrow = 2)

grid.arrange (p_hist[[2]], arrangeGrob (p_pred[[1]], p_pred[[2]]), ncol = 2)

grid.arrange (p_hist + theme (plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "in")), p_pred[[1]] + theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "in")), ncol = 2)

# cowplot plot_layout or plot_grid?
# https://canvas.uw.edu/files/61689473/download?download_frd=1
library (cowplot)
tmp_layout <- '
AB
AC
'
plot_grid (p_hist, p_pred[[1]], p_pred[[2]], ncol = 2, rel_heights = c (1, 1/2, 1/2), plot_layout (design = tmp_layout))
p_pred1 <- p_pred[[1]]
p_pred2 <- p_pred[[2]]
p_hist + p_pred1 + p_pred2 + 
  
  
 # https://www.datanovia.com/en/blog/ggplot-multiple-plots-made-ridiculuous-simple-using-patchwork-r-package/1
library (patchwork)  

p_hist/ p_pred[[1]]/p_pred[[2]]/p_pred[[3]] 

p_hist1 <- gplot (br_hist[[1]]) +
  geom_raster (aes (fill = value)) +
  borders (fill = "grey90", col = "black", 
           xlim = c (-32, -3), ylim = c (60, 69)) +
  coord_quickmap (xlim = c (-32, -3), ylim = c (60, 69)) +
  # .~ does "all", ~variable does (getValues(x)), as_labller ("Historical) does NA
  facet_wrap (~variable, labeller = labeller (variable = hist_lab), ncol = 1) +
  scale_fill_gradient2 (midpoint = 0, low = "#F7FCFD", mid = "#66C2A4", high = "#00441B", na.value = "white") +
  labs (fill = "Suitable \nhabitat") +
  theme_bw () +
  theme (axis.title = element_blank(),
         plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "in"))

p_hist1 + p_pred[[1]] + plot_layout (ncol = 2, heights = c (1, 2))

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


# Supplemental figure: plot grid of prediction periods and scenarios----
plot_diff_maps_ts <- function (sci_name) {
  
  diff_plot_input <- expand_grid (
    sci_name = sci_name,
    scenario = c (245, 585),
    period = c(2021, 2041, 2061, 2081)) %>%
    as.list()
  
    # apply calc_diff function 
    diff_plot_ls <- pmap (diff_plot_input, calc_diff_maps_ts) # about 5.5 mims
    
    # convert to raster brick
    diff_plot_br <- brick (diff_plot_ls)
    
    # set names for labeller to indicate the prediction period
    period_labs <- setNames (c("SSP 2-4.5: 2021-2040", "2041-2060", "2061-2080", "2081-2100", "SSP 5-8.5: 2021-2040", "2041-2060", "2061-2080", "2081-2100"), names (diff_plot_br))
    
  # plot
  
  png (paste0("Figures/Thermpred_Diff_map_allperiods_", sci_name, ".png"), width = 8, height = 4, res = 150, unit = "in")
  print(
  gplot (diff_plot_br) +
    geom_raster (aes (fill = value)) +
    borders (fill = "grey90", col = "black", 
             xlim = c (-32, -3), ylim = c (60, 69)) +
    coord_quickmap (xlim = c (-32, -3), ylim = c (60, 69)) +
    facet_wrap (~variable, ncol = 4, 
                labeller = labeller (variable = period_labs)) +
    # colorscale from brewer.pal (9, "PuBuGn")
    scale_fill_gradient2 (midpoint = 0, low = "#8C510A", mid = "#F5F5F5", high = "#01665E", na.value = "white") +
    labs (fill = "Habitat \ndifference") +
    ggtitle (str_replace (sci_name, "_", " ")) +
    theme_bw () +
    theme (axis.title = element_blank(),
           plot.title = element_text(face = "italic")) 
  )
  
  dev.off()

  
}# end function 


load ("Models/spp_Borm_suit.RData")
lapply (borm_suit, plot_diff_maps_ts); beep() # about 4.5 mins each


 
  

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

