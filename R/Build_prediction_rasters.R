## Build predictor rasters
# 7/27/2020
# JGM

# I should make a folder that has predictor variable raster stacks for each month and year, and both scenarios. Then I'll run the models for each species and save prediction stacks.

# I will have a mean raster brick for SST and BT for both scenarios. I need to take the appropriate layer from each brick and stack with the static variables, and assign the year. 

library (raster)
library (marmap)
library (mgcv)


# I'll have to run this through all species, all scenarios. make a function that does both scenarios for each species
dir.create ("Models/Prediction_bricks")

# load raster template
load ("Data/prediction_raster_template.RData") # named pred_r_template


# static variables ----

# Lat ----

# get lat/lon values from template
x <- xyFromCell (pred_r_template, 1:ncell (pred_r_template))

lat <- x[,2]
lat.r <- pred_r_template
values (lat.r) <- lat

# Lon ----
lon <- x[,1]
lon.r <- pred_r_template
values (lon.r) <- lon

# Depth ----
depth <- getNOAA.bathy(lon1 = -45, lon2 = 15,
                       lat1 = 50, lat2 = 85, 
                       resolution = 4)


#plot (depth, image = TRUE, land = TRUE)

depth.r <- as.raster(depth)
depth.r <- raster::resample (depth.r, pred_r_template, method = "bilinear")

# Year ----
# make raster, but don't assign value yet
year.r <- raster()
extent (year.r) <- extent (pred_r_template)
year.r <- raster::resample (year.r, pred_r_template, method = "bilinear") # not sure why I needed this (from PD sharks code)
#values (year.r) <- 2090 # will probably iteratively assign this?

# make vector of years
year_vals <- rep (2015:2100, each = 12)

# # Season ?? ----
# # I have season still in the models I saved. should take out?? will also be weird because i'll need to arbitrarily assign the winter and summer months. for now will call it spring. actually doesn't work with factor, should change to numeric
# season.r <- raster()
# extent (season.r) <- extent (pred_r_template)
# season.r <- raster::resample (season.r, pred_r_template, method = "bilinear")
# 
# season_vals <- 1

# temperature bricks ----
# SST----

# eventually will make one mean raster brick once I have CM 2.6 deltas. I'll then need to break up the brick and make additional stacks?

# for now, just load one. I redid gfdl 245 bt raster on 7/28. 
sst_245 <- brick ("Data/CMIP6_delta_projections/gfdl_245_sst_projection.grd")

bt_245 <- brick ("Data/CMIP6_delta_projections/gfdl_245_bt_projection.grd")

# sst_585 <- brick()
# bt_585 <- brick()

# vector of names
stack_names <- c("lat", "lon", "year", "surface_temp", "bottom_temp", "tow_depth_begin")

# function to go through each species ----
predict_brick_fun <- function (sci_name) {
  
  # load gams
  load (paste0("Models/PresAbs/Smooth_latlon/", sci_name, "_PA_full.Rdata")) # gam_PA
  load (paste0("Models/Biomass/Smooth_latlon/", sci_name, "_LB_full.Rdata")) # gam_LB
  

# make first prediction raster to start the stack
sst_245.r <- sst_245[[1]]
bt_245.r <- bt_245[[1]]

sst_585.r <- sst_585[[1]]
bt_585.r <- bt_585[[1]]

values (year.r) <- year_vals[1]

stack_245_1 <- stack (lat.r, lon.r, year.r, sst_245.r, bt_245.r, depth.r)
names (stack_245_1) <- stack_names

predict_PA_245 <- raster::predict (stack_245_1, gam_PA, type = "response")
predict_LB_245 <- raster::predict (stack_245_1, gam_LB, type = "response")

pred_stack_245 <- overlay (predict_PA_245, predict_LB_245, 
                          fun = function (x,y) {x * exp(y)}
                          )

stack_585_1 <- stack (lat.r, lon.r, year.r, sst585.r, bt585.r, depth.r)
names (stack_585_1) <- stack_names

predict_PA_585 <- raster::predict (stack_585_1, gam_PA, type = "response")
predict_LB_585 <- raster::predict (stack_585_1, gam_LB, type = "response")

pred_stack_585 <- overlay (predict_PA_585, predict_LB_585, 
                              fun = function (x,y) {x * exp(y)}
                           )

# make for loop----

for (i in 2:dim(bt_245)[3]) { # length of time series
  sst_245.r <- sst_245[[i]]
  bt_245.r <- bt_245[[i]]
  
  #sst_585.r <- gfdl_585_sst[[i]]
  #bt_585.r <- gfdl_585_bt[[i]]
  
  values (year.r) <- year_vals[i]
  
  stack_245 <- stack (lat.r, lon.r, year.r, sst_245.r, bt_245.r, depth.r)
  names (stack_245) <- stack_names
  
  predict_PA_245 <- raster::predict (stack_245, gam_PA, type = "response")
  predict_LB_245 <- raster::predict (stack_245, gam_LB, type = "response")
  
  predict_therm_245 <- overlay (predict_PA_245, predict_LB_245, 
                            fun = function (x,y) {x * exp(y)}
                            )

  # stack together
  pred_stack_245 <- stack (pred_stack_245, predict_therm_245)
  
  # 585
  # stack_585 <- stack (lat.r, lon.r, year.r, season.r, sst_585.r, bt_585.r, depth.r)
  # names (stack_585) <- stack_names
  # 
  # predict_PA_585 <- raster::predict (stack_585, gam_PA, type = "response")
  # predict_LB_585 <- raster::predict (stack_585, gam_LB, type = "response")
  # 
  # predict_therm_585 <- overlay (predict_PA_585, predict_LB_585, 
  #                               fun = function (x,y) {x * exp(y)})
  # 
  # pred_stack_585 <- stack (pred_stack_245, predict_therm_245)
  
} # end for loop

# convert to brick, save
pred_brick_245 <- brick (pred_stack_245)
pred_brick_585 <- brick (pred_stack_585)

writeRaster (pred_brick_245, file = paste0("Models/Prediction_bricks/", sci_name, "_245.grd"))
writeRaster (pred_brick_245, file = paste0("Models/Prediction_bricks/", sci_name, "_585.grd"))

} # end function

writeRaster (pred_brick_245, file = "Models/Prediction_bricks/Gadus_morhua_245_temp.grd", overwrite = TRUE)

#### quick prediction test------
# test predict
library (mgcv)
load ("Models/Biomass/Amblyraja_radiata_LB_full.Rdata") # error, can't process these factors. I think I would need to change to numeric. 

# try without lon
gam_PA <- gam (Presence ~ s(lat) + year + s(surface_temp) + s(bottom_temp) + s(tow_depth_begin),
               family = "binomial", 
               data = full_data_sci_name)

gam_LB <- gam (kg_log ~ s(lat) + year + s(surface_temp) + s(bottom_temp) + s(tow_depth_begin),
               family = "gaussian", 
               data = full_data_sci_name)

# try without season
gam_PA <- gam (Presence ~ te(lon, lat) + year + s(surface_temp) + s(bottom_temp) + s(tow_depth_begin),
               family = "binomial", 
               data = full_data_sci_name)

gam_LB <- gam (kg_log ~ te(lon, lat) + year + s(surface_temp) + s(bottom_temp) + s(tow_depth_begin),
               family = "gaussian", 
               data = full_data_sci_name)

predict_PA <- raster::predict (stack, gam_PA, type = "response")
predict_LB <- raster::predict (stack, gam_LB, type = "response")

predict_therm <- overlay (predict_PA, predict_LB, 
                          fun = function (x,y) {x * exp(y)}
                          )

plot (predict_therm)
