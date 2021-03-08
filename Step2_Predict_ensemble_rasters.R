## Build predictor rasters
# 7/27/2020
# JGM


library (raster)
library (marmap) # for depth layer
library (mgcv)
library (sf) # for EEZ shapefile

library (tidyverse) # for map and pmap
library (beepr) # to let me know when R is done running! 

# I'll run predictions for all species, all climate models, both scenarios. Will set up the static variables, then bring in the temperature bricks for each model and scenario. Clipping to Iceland EEZ for now to save time. Predict on each layer (month) of the brick for each climate model. 

#dir.create ("Models/Prediction_bricks")

# load standardized raster template
load ("Data/prediction_raster_template.RData") # named pred_r_template

# iceland EEZ shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=5680
eez <- st_read("Data/eez.shp")

# function for applying focal statistics on a brick--for spatial standard deviation
# https://stat.ethz.ch/pipermail/r-sig-geo/2016-May/024454.html
multiFocal <- function(x, w=matrix(1, nr=3, nc=3), ...) {
  
  if(is.character(x)) {
    x <- brick(x)
  }
  # The function to be applied to each individual layer
  fun <- function(ind, x, w, na.rm = TRUE, ...){
    focal(x[[ind]], w=w, ...)
  }
  
  n <- seq(nlayers(x))
  list <- lapply(X=n, FUN=fun, x=x, w=w, ...)
  
  out <- stack(list)
  return(out)
}

# ==================
# set up static variables ----
# ==================


# Depth ----
depth <- getNOAA.bathy(lon1 = -45, lon2 = 15,
                       lat1 = 50, lat2 = 85, 
                       resolution = 4)

#plot (depth, image = TRUE, land = TRUE)

# rasterize and resample to my template

depth_r <- as.raster(depth)

# need to convert positive values to match GAMs!!
depth_r_neg <- calc (depth_r, function (x) -x)
depth_r <- raster::resample (depth_r_neg, pred_r_template, method = "bilinear")

# # plot depth and eez map for reference
# depth_r_pts <- rasterToPoints (depth_r, spatial = TRUE)
# depth_r_df <- data.frame(depth_r_pts)
# 
# png ("Figures/Depth_map_eez.png")
# ggplot() +
#   geom_raster(data = depth_r_df , aes(x = x, y = y, fill = layer)) +
#   geom_polygon(data = map_data("world",'Iceland'), 
#                aes(long,lat,group = group), col = 'gray10' ,fill = 'gray90',size = 0.3) +
#   geom_sf (data  = eez, fill = NA, lwd = 0.5, col = "gray10") 
# dev.off()
#   

# cropped plot for MFRI
# png ("Figures/Depth_map_MFRI.png", width = 6, height = 6, units = "in", res = 300)
# plot (depth_r, 
#       xlim = c (-32, -3), ylim = c (60, 69),
#       col = rev(brewer.pal(name = "PuBu", n = 9)),
#       cex.main = 2,
#       main = "Bathymetry")
# map('worldHires',add=TRUE, col='grey90', fill=TRUE)
# dev.off()
  
# Bormicon region ----
# I created this in Rasterize_bormicon_regions.R
borm_r <- raster ("Data/bormicon_divisions.grd")

# vector of names ----
# this will depend on the terms in the GAM
stack_names <- c("bormicon_region", "surface_temp", "bottom_temp", "tow_depth_begin", "sst_max", "sst_min", "bt_max", "sst_dev", "bt_dev")

#stack_names <- c( "surface_temp", "bottom_temp", "tow_depth_begin", "sst_max", "sst_min", "bt_max", "sst_dev", "bt_dev")

# ==================
# Future predictions ----
# ==================

# this is a helper function that will go in the function below. It goes through each layer (month) of the climate model temperature projections, and saves the model prediction raster as an element of a list. The next function will convert the list into a raster brick and save. 

predict_list_fun <- function (sci_name, GAM, CM, scenario, month_index) {
  
  # month_index is numeric, minimum 1 and max 1032. corresponds to layers of CMIP6 projections, jan 2015-dec 2100. If sst_min and max are in the model, then the minimum is 13 (Jan 2016). if CM 2.6 is included, then the maximum is 792, dec 2080. 2061-2080 would be 553:792
  
  # sci_name is the scientific name separated by an underscore 
  # GAM is the name of the model (name of directory where I saved them in "Fit_GAMS_function.R")
  # scenario is either 245 or 585
  # CM is four-letter lowercase abbreviation

  
  # load saved GAM for species of interest ----
  # load previously fit GAMs, which have slightly different naming conventions depending on the model
  if (GAM %in% c ("First_full_tensor_season", "Tensor_drop_season", "Smooth_latlon")) {
    load (file.path("Models", GAM, paste0(sci_name, "_PA_full.Rdata")))
    load (file.path("Models", GAM, paste0(sci_name, "_LB_full.Rdata")))
  } else {
    load (file.path("Models", GAM, paste0(sci_name, "_PA.Rdata")))
    load (file.path("Models", GAM, paste0(sci_name, "_LB.Rdata")))
  }

  
  # run predictions for each climate model ----

  # load temperature bricks
  sst_brick <- brick (paste0("Data/CMIP6_delta_projections/", CM, "_", scenario, "_sst_projection.grd"))
  bt_brick <- brick (paste0("Data/CMIP6_delta_projections/", CM, "_", scenario, "_bt_projection.grd"))
    
  
  # Kind of hard coding this because CM26 is a different size. working backwards to do last period. 
  # CM 2.6 has 960 layers, which correspond to 2001-2080. CMIP6 have 1032 layers, 2015-2100. It seems really complicated to have separate indices for CM2.6 in an expand_grid object. I think the best option is to have a special case here for CM26. If you start with month_index = 1, 2015-01 for CMIP6, that's the equivalent of 169 for CM2.6
  month_val <- ifelse (CM == "CM26",
                       168 + month_index,
                       month_index
                       )
  
  
  # subset layer of sst and bt
  sst_r <- sst_brick[[month_val]] 
  bt_r <- bt_brick[[month_val]]
    
  # also calculate sst min/max and bt max if these are terms in the model
  # subset 12 preceeding months, then calc to get min or max
  sst_max_r <- calc ( subset (sst_brick, (month_val - 13):(month_val - 1)), max)

  sst_min_r <- calc (
    subset (sst_brick, (month_val - 13):(month_val - 1)),
    min)

  bt_max_r <- calc (
    subset (bt_brick, (month_val - 13):(month_val - 1)),
    max)

  
  # calculate sst and bt dev
  sst_dev_r <- multiFocal(sst_r, fun = function (x) sd (x, na.rm = TRUE))
  bt_dev_r <- multiFocal(bt_r, fun = function (x) sd (x, na.rm = TRUE))
  
  # stack the layers together
  stack_1 <- stack (borm_r, sst_r, bt_r, depth_r, sst_max_r, sst_min_r, bt_max_r, sst_dev_r, bt_dev_r)
  names (stack_1) <- stack_names
  
  # crop to Iceland EEZ
  clip_1 <- mask (stack_1, eez)
  
  # make predictions
  predict_PA <- raster::predict (clip_1, gam_PA, type = "response")
  predict_LB <- raster::predict (clip_1, gam_LB, type = "response")
  
  # get Eresid smear transformation value
  Eresid <- mean (exp (residuals.gam (gam_LB)))
  
  # thermal prediction--relate PA to log biomass
  pred_stack <- overlay (predict_PA, predict_LB, 
                      fun = function (x,y) {x * exp(y) * Eresid}
                      )

} # end prediction function 

#system.time (pred_test <- predict_brick_fun(sci_name = "Gadus_morhua", GAM = "Borm_14_alltemp", CM = "gfdl", scenario = 245, month_index = 33))



# Outer function: computes the month indices based on what projection years you want, converts the raster list into a brick, and saves to a folder. 
# this takes about 15 minutes per model/scenario for 20 years. 

predict_brick_fun <- function (sci_name, GAM, CM, scenario, year1, year2) {
  
  # year1 is starting year, year2 is ending year
  
  # translate years to month index
  years <- year1:year2
  cmip6_vals <- rep (2015:2100, each = 12)
  
  month_index <- which (cmip6_vals %in% years)
 
  # apply predict_list function. This will return a list, where each element is a raster of the prediction for that month
  predict_list_input <- list (sci_name, GAM, CM, scenario, month_index)
  
  predict_list <- pmap (predict_list_input, predict_list_fun)
  
  # convert to brick 
  predict_brick <- brick (predict_list)
  
  #filename should have all of the function arguments
  CM_save_path <- paste (sci_name, GAM, CM, scenario, year1, year2, sep = "_")
  
  # save raster brick to Prediction bricks folder 
  writeRaster (predict_brick, file = paste0("Models/Prediction_bricks/", CM_save_path, ".grd"), overwrite = TRUE)
  
}

### Run on Borm 14 species
load ("Models/spp_Smooth_latlon.RData")

CM_list <- c("gfdl", "cnrm", "ipsl", "mohc", "CM26")

# #https://amber.rbind.io/2018/03/26/purrr/

# from my experiments, lapply might be faster than pmap, about ~2 mins per run which could be significant (13 mins vs 15 mins). Not actually that different from the for loops. But, no good way to feed it all the different options. Might actually be better for my computer/sanity to run one CM and scenario at a time, though. 

# 3925.65 s, 65 mins for lapply Scomber 585
# 3235.67  for pmap scomber 245
# 3622 s + 4745.94 s for a. raj mapply, both scenarios (had to redo because doesn't do the permutations)

# use expand_grid to get all the possible permutations of species, CM, scenario. Cut out CM 2.6 and 245. 
# I did A. radiata, M. villosus, and S. scombrus separately while testing. I also don't have M. scorpius
borm_spp <- spp_Smooth_latlon %>%
  filter (!sci_name_underscore %in% c ( "Myoxocephalus_scorpius",  "Mallotus_villosus",  "Amblyraja_radiata" , "Scomber_scombrus"))

borm_expand <- expand_grid (sci_name = borm_spp$sci_name_underscore,
                            GAM = "Borm_14_alltemp",
                            CM = CM_list,
                            scenario = c(245, 585),
                            year1 = 2061,
                            year2 = 2080) %>%
  filter (!(CM == "CM26" & scenario == 245)) %>% 
  as.list()
# for 61 species (there will be 64 total if running again from scratch), there should be 549. I found expand_grid much more straightforward than cross or cross_df. Seems to play better with pmap, and I couldn't figure out how to filter with cross. 



system.time (pmap (borm_expand, predict_brick_fun)); beep (sound = 3)

# redo with borm_empty spp
# try with just one first
monk_expand <- expand_grid (sci_name = c("Phycis_blennoides", "Cottunculus_microps", "Bathyraja_spinicauda"),
                        GAM = "Borm_14_alltemp",
                        CM = CM_list,
                        scenario = c(245, 585),
                        year1 = 2061,
                        year2 = 2080) %>%
  filter (!(CM == "CM26" & scenario == 245)) %>% 
  as.list()

system.time (pmap (monk_expand, predict_brick_fun)) # 2.17 hrs for monkfish

# do a few at a time

# just do cod, a. radiata and r. hippoglossus for temp x depth intn
TxD_expand <- expand_grid (sci_name = c("Amblyraja_radiata", "Reinhardtius_hippoglossoides", "Cyclopterus_lumpus", "Pleuronectes_platessa", "Sebastes_mentella", "Hippoglossus_hippoglossus", "Lophius_piscatorius"),
                            GAM = "Borm_14_alltemp_intn_test",
                            CM = CM_list,
                            scenario = 585,
                            year1 = 2061,
                            year2 = 2080) %>%
  #filter (!(CM == "CM26" & scenario == 245)) %>% 
  as.list()

TxD_expand <- expand_grid (sci_name = c("Cyclopterus_lumpus"),
                           GAM = "Borm_14_alltemp_intn_test",
                           CM = CM_list,
                           scenario = 245,
                           year1 = 2061,
                           year2 = 2080) %>%
  filter (!(CM == "CM26" & scenario == 245)) %>% 
  as.list()

system.time (pmap (TxD_expand, predict_brick_fun)) 


# ==================
## Historical period using GLORYS temperature ----
# ==================

# stack names to match model formula
stack_names <- c("bormicon_region", "surface_temp", "bottom_temp", "tow_depth_begin", "sst_max", "sst_min", "bt_max", "sst_dev", "bt_dev")

# load temperature bricks
sst_brick_hist <- brick ("Data/GLORYS_sst_hist.grd") # these go from 2000-2018; 228 layers
bt_brick_hist <- brick ("Data/GLORYS_bt_hist.grd")

sst_max_brick_hist <- brick ("Data/glorys_sst_month_max.grd") # these go from 1993-2018; 312 layers
sst_min_brick_hist <- brick ("Data/glorys_sst_month_min.grd")
bt_max_brick_hist <- brick ("Data/glorys_bt_month_max.grd")

# historical function ----

# same as with predictions, one function makes a prediction for each month and returns a list; the outer function converts the list into a brick and saves

hist_list_fun <- function (sci_name, GAM, month_index) {
  #month_index is a number from 1-228, for 2000-2018
  # sci_name is sci_name_underscore
  # GAM is the GAM name/folder
  
  # load saved GAM for species of interest
  load (paste0("Models/", GAM, "/", sci_name, "_PA.Rdata")) # gam_PA
  load (paste0("Models/", GAM, "/", sci_name, "_LB.Rdata")) # gam_LB
  
  
  # subset temperature layers
   # regular sst and bottom t are 2000-2018, 228 layers
  sst_r <- sst_brick_hist[[month_index]] 
  bt_r <- bt_brick_hist[[month_index]]
    
  # need to subset year and take max/min. these go from 1993-2018, so 12 mos prior to Jan 2000 [85] would be 73:84. month_index 1 corresponds to january 2000. 
  # glorys[73] would be january 1999. 
  sst_max_r <- calc (subset (sst_max_brick_hist, (72 + month_index):(83 + month_index)), max)
  sst_min_r <- calc (subset (sst_min_brick_hist, (72 + month_index):(83 + month_index)), min)
  bt_max_r <- calc (subset (bt_max_brick_hist, (72 + month_index):(83 + month_index)), max)
  
  # calculate sst and bt dev
  sst_dev_r <- multiFocal(sst_r, fun = function (x) sd (x, na.rm = TRUE))
  bt_dev_r <- multiFocal(bt_r, fun = function (x) sd (x, na.rm = TRUE))
    
  # stack the layers together and give names that match model formula
  stack_1 <- stack (borm_r, sst_r, bt_r, depth_r, sst_max_r, sst_min_r, bt_max_r, sst_dev_r, bt_dev_r)
  names (stack_1) <- stack_names
    
  # crop to Iceland EEZ
  clip_1 <- mask (stack_1, eez)
    
  # make predictions
  predict_PA <- raster::predict (clip_1, gam_PA, type = "response")
  predict_LB <- raster::predict (clip_1, gam_LB, type = "response")
    
  # get Eresid smear transformation value
  Eresid <- mean (exp (residuals.gam (gam_LB)))
  
  # thermal prediction--relate PA to log biomass
  pred_stack <- overlay (predict_PA, predict_LB, 
                         fun = function (x,y) {x * exp(y) * Eresid}
  )
    
} # end hist function

hist_brick_fun <- function (sci_name, GAM, year1, year2) {
  # year1 is starting year, year2 is ending year
  
  # translate years to month index
  possible_years <- rep (2000:2018, each = 12)
  month_index <- which (possible_years %in% year1:year2)

  
  # apply hist_list function. This will return a list, where each element is a raster of the prediction for that month
  hist_list_input <- list (sci_name, GAM, month_index)
  
  hist_list <- pmap (hist_list_input, hist_list_fun)
  
  # convert to brick 
  hist_brick <- brick (hist_list)
  
  #filename should have all of the function arguments
  hist_save_path <- paste (sci_name, GAM, year1, year2, sep = "_")
  
  # save raster brick to Prediction bricks folder 
  writeRaster (hist_brick, file = paste0("Models/Prediction_bricks/", hist_save_path, ".grd"), overwrite = TRUE)
  
} # end brick function


# run again on empty species, defined in fit_GAMS_fun

borm_expand_hist <- expand_grid (sci_name = empty_spp$sci_name_underscore,
                            GAM = "Borm_14_alltemp",
                            year1 = 2000,
                            year2 = 2018) %>%
  as.list()

system.time (pmap (borm_expand_hist, hist_brick_fun)); beep (sound = 3)
# 14.7 hours, about 14-15 mins each

# forgot to bring back in the species I tested for the pred function
addl_hist <- expand_grid (sci_name = c("Mallotus_villosus",  "Amblyraja_radiata" , "Scomber_scombrus"),
                          GAM = "Borm_14_alltemp",
                          year1 = 2000,
                          year2 = 2018) %>%
  as.list()

pmap (addl_hist, hist_brick_fun); beep()

TxD_expand <- expand_grid (sci_name = c("Gadus_morhua", "Amblyraja_radiata", "Reinhardtius_hippoglossoides", "Cyclopterus_lumpus", "Pleuronectes_platessa", "Sebastes_mentella", "Hippoglossus_hippoglossus", "Lophius_piscatorius"),
                           GAM = "Borm_14_alltemp_intn_test",

                           year1 = 2000,
                           year2 = 2018) %>%
  #filter (!(CM == "CM26" & scenario == 245)) %>% 
  as.list()

system.time (pmap (TxD_expand, hist_brick_fun)) 
