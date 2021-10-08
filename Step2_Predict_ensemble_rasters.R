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

# make a depth mask for depth > 1200m
depth_r_mask <- depth_r
depth_r_mask  [depth_r_mask[] > 1200] <- NA
writeRaster (depth_r_mask, file = "Data/depth_1200_mask.RData")


# Rugosity ----

f <- matrix(1, nrow=3, ncol=3)

rug_abs <- focal(depth_r, w=f, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8, pad=TRUE, padValue=NA)
rug_r <- raster::resample (rug_abs, pred_r_template, method = "bilinear")

# very large values are skewing the predictions where there's a very narrow peninsula off the southwest. 
#rug_r <- clamp(rug_r, upper=600, useValues=FALSE)
probcell <- cellFromXY(rug_r, cbind(c(-20, -19.25), c(63.25, 63.5)))

rug_r[20741] <- NA
rug_r[20742] <- NA
rug_r[20743] <- NA

  
# Bormicon region ----
# I created this in Rasterize_bormicon_regions.R
#borm_r <- raster ("Data/bormicon_divisions.grd")

# vector of names ----
# this will depend on the terms in the GAM

stack_names <- c("rugosity", "surface_temp", "bottom_temp", "tow_depth_begin", "sst_max", "sst_min", "bt_max")

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
  
  
  # For some species outliers in deeper values are uninterpretable; crop at 1200m depth
  
  if (sci_name %in% c("Sebastes_marinus", "Sebastes_viviparus", "Sebastes_mentella", "Lycodes_gracilis", "Chimaera_monstrosa", "Lepidion_eques")) {
    depth_r[depth_r[] > 1200] <- NA
  }
  
  # load saved GAM for species of interest ----
  load (file.path("Models", GAM, paste0(sci_name, ".Rdata")))
  
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

  
  # stack the layers together
  stack_1 <- stack (rug_r, sst_r, bt_r, depth_r, sst_max_r, sst_min_r, bt_max_r)
  names (stack_1) <- stack_names
  
  # crop to Iceland EEZ
  clip_1 <- mask (stack_1, eez)
  
  # make predictions
  predict_ras <- raster::predict (clip_1, gam_nb, type = "response") 

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
load ("Models/spp_Borm.RData")

CM_list <- c("gfdl", "cnrm", "ipsl", "mohc", "CM26")

# #https://amber.rbind.io/2018/03/26/purrr/

# from my experiments, lapply might be faster than pmap, about ~2 mins per run which could be significant (13 mins vs 15 mins). Not actually that different from the for loops. But, no good way to feed it all the different options. 

# 3925.65 s, 65 mins for lapply Scomber 585
# 3235.67  for pmap scomber 245
# 3622 s + 4745.94 s for a. raj mapply, both scenarios (had to redo because doesn't do the permutations)

# use expand_grid to get all the possible permutations of species, CM, scenario. Cut out CM 2.6 and 245. 

borm_expand <- expand_grid (sci_name = borm_spp$sci_name_underscore,
                            GAM = "Rug_tw_LL",
                            CM = CM_list,
                            scenario = c(245, 585),
                            year1 = 2061,
                            year2 = 2080) %>%
  filter (!(CM == "CM26" & scenario == 245)) %>% 
  as.list()


system.time (pmap (borm_expand, predict_brick_fun)); beep (sound = 3)
# takes about 4 days

# ==================
## Historical period using GLORYS temperature ----
# ==================

# stack names to match model formula
stack_names <- c("bormicon_region", "surface_temp", "bottom_temp", "tow_depth_begin", "sst_max", "sst_min", "bt_max")

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

  load (file.path("Models", GAM, paste0(sci_name, ".Rdata")))
  
  # subset temperature layers
   # regular sst and bottom t are 2000-2018, 228 layers
  sst_r <- sst_brick_hist[[month_index]] 
  bt_r <- bt_brick_hist[[month_index]]
    
  # need to subset year and take max/min. these go from 1993-2018, so 12 mos prior to Jan 2000 [85] would be 73:84. month_index 1 corresponds to january 2000. 
  # glorys[73] would be january 1999. 
  sst_max_r <- calc (subset (sst_max_brick_hist, (72 + month_index):(83 + month_index)), max)
  sst_min_r <- calc (subset (sst_min_brick_hist, (72 + month_index):(83 + month_index)), min)
  bt_max_r <- calc (subset (bt_max_brick_hist, (72 + month_index):(83 + month_index)), max)
  
    
  # stack the layers together and give names that match model formula
  stack_1 <- stack (rug_r, sst_r, bt_r, depth_r, sst_max_r, sst_min_r, bt_max_r)
  names (stack_1) <- stack_names
    
  # crop to Iceland EEZ
  clip_1 <- mask (stack_1, eez)
    
  # make predictions
  pred_stack <- raster::predict (clip_1, gam_nb, type = "response")
    
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



borm_expand_hist <- expand_grid (sci_name = borm_spp$sci_name_underscore,
                            GAM = "Rug_tw_LL",
                            year1 = 2000,
                            year2 = 2018) %>%
  as.list()

system.time (pmap (borm_expand_hist, hist_brick_fun)); beep (sound = 3)
# 14.7 hours, about 14-15 mins each