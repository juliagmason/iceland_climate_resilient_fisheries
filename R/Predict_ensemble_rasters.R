## Build predictor rasters
# 7/27/2020
# JGM


library (raster)
library (marmap) # for depth layer
library (mgcv)
library (sf) # for EEZ shapefile

# I'll run predictions for all species, all models, both scenarios. Will set up the static variables, then bring in the temperature bricks for each model and scenario. Clipping to Iceland EEZ for now to save time. Predict on each layer (year) of the brick for each model, take a mean, and save. 

dir.create ("Models/Prediction_bricks")

# load standardized raster template
load ("Data/prediction_raster_template.RData") # named pred_r_template

# iceland EEZ shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=5680
eez <- st_read("Data/eez.shp")

# set up static variables ----

# Lat ----

# get lat/lon values from template
x <- xyFromCell (pred_r_template, 1:ncell (pred_r_template))

lat <- x[,2]
lat_r <- pred_r_template
values (lat_r) <- lat

# Lon ----
lon <- x[,1]
lon_r <- pred_r_template
values (lon_r) <- lon

# Depth ----
depth <- getNOAA.bathy(lon1 = -45, lon2 = 15,
                       lat1 = 50, lat2 = 85, 
                       resolution = 4)


#plot (depth, image = TRUE, land = TRUE)

depth_r <- as.raster(depth)
depth_r <- raster::resample (depth_r, pred_r_template, method = "bilinear")

# Year ----
# make raster, but don't assign value yet
year_r <- raster()
extent (year_r) <- extent (pred_r_template)
year_r <- raster::resample (year_r, pred_r_template, method = "bilinear") # not sure why I needed this (from PD sharks code)


# make vector of years
#year_vals <- rep (2015:2100, each = 12) # may need to change to 2021 for CM 2.6

# try just with 2091-2100
year_vals <- rep (2091:2100, each = 12)

# # Season ?? ----
# # I have season still in the models I saved. should take out?? will also be weird because i'll need to arbitrarily assign the winter and summer months. for now will call it spring. actually doesn't work with factor, should change to numeric
# season_r <- raster()
# extent (season_r) <- extent (pred_r_template)
# season_r <- raster::resample (season_r, pred_r_template, method = "bilinear")
# 
# season_vals <- 1


# vector of names
stack_names <- c("lat", "lon", "year", "surface_temp", "bottom_temp", "tow_depth_begin")

# function to go through each species, CM, scenario ----

# tried putting vector of year_vals in function argument but get errors--attempt to apply non-function. maybe better to set it out here. do 2060-2080, to match with kristin/CM26
year_vals <- rep (2061:2080, each = 12) # model projections are monthly
 
predict_brick_fun <- function (sci_name, GAM, scenario) {
  
  print (sci_name)
  start1 <- Sys.time()
  
  # load saved GAM for species of interest
  load (paste0("Models/PresAbs/", GAM, "/", sci_name, "_PA_full.Rdata")) # gam_PA
  load (paste0("Models/Biomass/", GAM, "/", sci_name, "_LB_full.Rdata")) # gam_LB
  
  # CM2.6 run corresponds to 585 scenario, but don't have a 245 equivalent. 
  if (scenario == 585) {
    CM_list = c("gfdl", "cnrm", "ipsl", "mohc", "CM26")
  } else {CM_list = c("gfdl", "cnrm", "ipsl", "mohc") }
  
  for (CM in CM_list) {
    print (CM)
    
    # load temperature bricks
    sst_brick <- brick (paste0("Data/CMIP6_delta_projections/", CM, "_", scenario, "_sst_projection.grd"))
    bt_brick <- brick (paste0("Data/CMIP6_delta_projections/", CM, "_", scenario, "_bt_projection.grd"))
    
    
    # make first prediction raster to start the stack
    
    # Kind of hard coding this because CM26 is a different size. working backwards to do last period
    start_val <- ifelse (CM == "CM26",
                         dim(sst_brick)[3] - length (year_vals) ,
                         dim(sst_brick)[3] - (length (year_vals) + 240)
    )
    sst_r <- sst_brick[[start_val + 1]] 
    bt_r <- bt_brick[[start_val + 1]]
    
    values (year_r) <- year_vals[1]
    
    stack_1 <- stack (lat_r, lon_r, year_r, sst_r, bt_r, depth_r)
    names (stack_1) <- stack_names
    
    # crop to Iceland EEZ
    clip_1 <- mask (stack_1, eez)
    
    # make predictions
    predict_PA <- raster::predict (clip_1, gam_PA, type = "response")
    predict_LB <- raster::predict (clip_1, gam_LB, type = "response")
    
    # thermal prediction--relate PA to log biomass
    pred_stack <- overlay (predict_PA, predict_LB, 
                        fun = function (x,y) {x * exp(y)}
                        )


    # for loop to stack for all years----
    start_time <- Sys.time()
    for (i in 2:length(year_vals)) { # length of time series
      
      # if doing the whole time series, progress report
      
      #if (i%%100 == 0) {print (i)}
      sst_i <- sst_brick[[i + start_val]]
      bt_i <- bt_brick[[i + start_val]]
      
      values (year_r) <- year_vals[i]
      
      stack_i <- stack (lat_r, lon_r, year_r, sst_i, bt_i, depth_r)
      names (stack_i) <- stack_names
      
      clip_i <- mask (stack_i, eez)
      
      predict_PA_i <- raster::predict (clip_i, gam_PA, type = "response")
      predict_LB_i <- raster::predict (clip_i, gam_LB, type = "response")
      
      predict_therm_i <- overlay (predict_PA_i, predict_LB_i, 
                                fun = function (x,y) {x * exp(y)}
                                )
    
      # stack predictions for each year
      pred_stack <- stack (pred_stack, predict_therm_i)
    
    
    } # end year_vals for loop
    end_time = Sys.time()
  
    print (end_time - start_time)
    
    # keep as stack, give model name
    stack_name <- paste0 (CM, "_stack")
    assign (stack_name, pred_stack)

  } # end CM for loop
    
  
  if (scenario == 585) {ensemble_stack <- stack (gfdl_stack, cnrm_stack, ipsl_stack, mohc_stack, CM26_stack)} else {ensemble_stack <- stack (gfdl_stack, cnrm_stack, ipsl_stack, mohc_stack) }
    
  system.time(ensemble_brick <- brick (ensemble_stack)) # 2.4s for 10 years. convert to brick to make stackapply go faster
  
  system.time(
    mean_brick <- stackApply (ensemble_brick, mean, indices = 1:length(year_vals)) # also ~2s
  )
  
    # write prediction brick as raster
     save_path <- paste (sci_name, GAM, scenario, year_vals[1], year_vals[length(year_vals)], sep = "_")
    
     writeRaster (mean_brick, file = paste0("Models/Prediction_bricks/", save_path, ".grd"), overwrite = TRUE)
end1 <- Sys.time()

print (end1 - start1)

} # end function

# test again


predict_brick_fun (sci_name = "Lepidorhombus_whiffiagonis", GAM = "Smooth_latlon", scenario = 585)

# run on full species list that I ran GAMs for
spp_totals <- mfri_abun %>% 
  group_by (species) %>% 
  summarize (count = n()) %>% 
  filter (count > 50) %>%  # need to have enough observations for the model: https://stackoverflow.com/questions/36719799/mgcv-gam-error-model-has-more-coefficients-than-data
  filter (!species %in% c(41, 72, 97, 35, 92, 49, 89))

spp_table <- read.csv("Data/species_eng.csv")
spp <- spp_table$sci_name_underscore[which (spp_table$Spp_ID %in% spp_totals$species)]

system.time (sapply (spp, predict_brick_fun, GAM = GAM, scenario = 585)) # 5 hours for 25 spp




### Trial code down here #######

# plot check 
tmp <- brick ("Models/Prediction_bricks/Amblyraja_radiata_Smooth_latlon_245_2091_2100.grd")
dim (tmp)
plot (tmp, 100)
summary (getValues(tmp[[1]])) # super weird big values

cod_tmp <- brick ("Models/Prediction_bricks/Gadus_morhua_Smooth_latlon_245_2091_2100.grd")
plot (cod_tmp, 1)
summary (getValues (cod_tmp[[1]]))

cod_mn <- overlay (cod_tmp, fun = mean)

cod_2 <- brick ("Models/Prediction_bricks/Gadus_morhua_245_slatlon_temp.grd")
cod_2 <- mask (cod_2, eez)
summary (getValues (cod_2[[913]]))
### Run predictions----

CM_list = c("gfdl", "cnrm", "ipsl", "mohc", "CM26")

# function to run the predictions and take mean?
library (rgdal)
library (data.table)
library (parallel)

mean_predict_brick_fun <- function (sci_name, GAM, scenario) {
  
  # CM 2.6 only corresponds to 585
  if (scenario == 585) {
    CM_list = c("gfdl", "cnrm", "ipsl", "mohc", "CM26")
  } else {CM_list = c("gfdl", "cnrm", "ipsl", "mohc") }
  
  # apply prediction function
  sapply (CM_list, predict_brick_fun, sci_name = sci_name, GAM = GAM, scenario = scenario)
  
  # load files and take mean
  
  # https://gis.stackexchange.com/questions/259011/looping-several-raster-files-in-r
  
  brick_pattern <- paste0(
    paste(sci_name, GAM, sep = "_"), 
    ".*", 
    paste0(scenario, ".grd")
  )
  
  brick_files <- list.files (path = "Models/Prediction_bricks", pattern = brick_pattern)
  
  brickTab <- data.table (brick_files)
  # https://www.rdocumentation.org/packages/data.table/versions/1.13.0/topics/%3A%3D
  brickTab[, col := lapply (file.path("Models/Prediction_bricks", brick_files), raster),]
  
  mean_fun <- function (brick_files) {
    require (raster)
    return (overlay (brick_files),
            fun = function (CM26, cnrm, gfdl, ipsl, mohc) {return (mean (CM26, cnrm, gfdl, ipsl, mohc, na.rm = TRUE))}
    )

    
  }
  
  
  
}

# try with one species I know worked
sapply (CM_list, predict_brick_fun, sci_name = "Microstomus_kitt", GAM = "Smooth_latlon", scenario = 585)
sapply (CM_list, predict_brick_fun, sci_name = "Gadus_morhua", GAM = "Smooth_latlon", scenario = 585)

predict_brick_fun(sci_name = "Melanogrammus_aeglefinus", GAM = "Smooth_latlon", CM = "mohc", scenario = 245)

# load and plot
test <- brick("Models/Prediction_bricks/Microstomus_kitt_Smooth_latlon_cnrm_585.grd")
test_clip <- mask(test, eez)
dim(test)
plot (test_clip[[100]])
summary (getValues(test_clip[[100]]))
#really not seeing anything. don't understand. why is pred_stack only 181 long. 

test_Ma <- brick("Models/Prediction_bricks/Melanogrammus_aeglefinus_Smooth_latlon_mohc_245.grd")
dim(test_Ma)
plot (test_Ma, 1000)
q_breaks <- quantile (getValues(test_Ma), probs = seq (0, 1, 0.1), na.rm = TRUE)
plot (test_Ma, 1000, 
      breaks = q_breaks, col = terrain.colors(11),
      xlim = c (-32, 0), ylim = c (60, 70))

# try with cod
predict_brick_fun(sci_name = "Gadus_morhua", GAM = "Smooth_latlon", CM = "gfdl", scenario = 245)
cod_test <- brick ("Models/Prediction_bricks/Gadus_morhua_Smooth_latlon_gfdl_245.grd")
dim (cod_test)
plot (cod_test, 100)

q_breaks <- quantile (getValues(cod_test), probs = seq (0, 1, 0.1), na.rm = TRUE)
plot (cod_test, 100, 
      breaks = q_breaks, col = terrain.colors(11),
      xlim = c (-32, 0), ylim = c (60, 70))


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

tmp

## quick plot check ----

library (raster)
library (sf)
library (mapdata)

# iceland EEZ shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=5680
eez <- st_read("Data/eez.shp")

# vector of dates
projected_dates <- seq (ymd("2015-01-01"), ymd("2100-12-12"), by = "months")

# load prediction bricks
# eventually these will be in Models/prediction_bricks

# for now, have temporary saved file in Data

# this one is tensor lat lon
te_brick <- brick ("Models/Prediction_bricks/Gadus_morhua_245_temp.grd")
s_brick <- brick ("Models/Prediction_bricks/Gadus_morhua_245_slatlon_temp.grd")

plot (tmp_brick, 1)
# make color scale based on quantiles
col_breaks <- quantile (getValues(s_brick), probs = seq (0, 1, 0.1), na.rm = TRUE)
plot (tmp_brick, 1000, breaks = col_breaks, col = terrain.colors(11))

## take decade averages
# https://gis.stackexchange.com/questions/222443/find-mean-of-a-group-of-every-10-layers-in-raster-stack-in-r

# not evenly split into decades. would want 2015-2020, then 2021-2030 and so on. need to repeat first index 72 times, then 120. 
dec_index <- c (rep (1, 72), rep (2:9, each = 120))

dec_names <- c(
  "2015-2020",
  "2021-2030",
  "2031-2040",
  "2041-2050",
  "2051-2060",
  "2061-2070",
  "2071-2080",
  "2081-2090",
  "2091-2100"
)

dec_mean <- stackApply (tmp_brick, dec_index, fun = mean)
names (dec_mean) <- dec_names
dec_breaks <- quantile (getValues(dec_mean), probs = seq (0, 1, 0.1), na.rm = TRUE)
plot (dec_mean, breaks = dec_breaks, col = terrain.colors(11))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

# clip decades to island eez
dec_clip <- mask (dec_mean, eez)
dec_breaks_isl <- quantile (getValues(dec_clip), probs = c(seq (0, 0.9, 0.1), 0.95, 0.99, 0.999, 1), na.rm = TRUE)
plot (dec_clip, breaks = dec_breaks_isl, col = terrain.colors(14),
      xlim = c(-33, 0), ylim = c (58, 72))


vals1 <- getValues (tmp_brick[[1]])
summary (vals1) # max e38
# plot with color range excluding outliers
quantile (vals1, 0.75, na.rm = TRUE) + 1.5 * (quantile (vals1, 0.75, na.rm = TRUE) - quantile (vals1, 0.25, na.rm = TRUE))

allvals <- as.vector(getValues(tmp_brick))
summary (allvals)

# should my range exclude zeros?
max_scale <- quantile (allvals, 0.75, na.rm = TRUE) + 1.5 * (quantile (allvals, 0.75, na.rm = TRUE) - quantile (allvals, 0.25, na.rm = TRUE)) # 34

allvals_P <- allvals[which(allvals > 0)]
summary (allvals_P) # exactly the same?? more values have been exlcuded than just NA...

library (viridis)
library (maps)
library (mapdata)

plot (tmp_brick, 1, col = viridis(64), zlim = c (0, 35))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (tmp_brick, 100, col = viridis(64), zlim = c (0, 35))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (tmp_brick, 1000, col = viridis(64), zlim = c (0, 35))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (tmp_brick, 1000, col = viridis(64), zlim = c (0, 350))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

pred_clip <- mask (s_brick, eez)

plot (tmp_brick, 1000)
plot (log(tmp_brick), 1000)

vals <- getValues (pred_clip[[1000]])

# also quick checks with first layer for different cod models run through build_prediciton_rasters

# for tensor, have weird thing where exp() values > 710 are Inf, so messes up color bar. weird behavior in extreme north for biomass giving huge values. 
pred_stack_245[pred_stack_245 > 10000] <- NA

pred_clip <- mask (pred_stack_245, eez)
isl_breaks <- quantile (getValues(pred_clip), probs = seq (0, 1, 0.1), na.rm = TRUE)
plot (pred_clip, 1000, 
      breaks = isl_breaks, col = terrain.colors(11),
      xlim = c (-32, 0), ylim = c (60, 70))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

# plot full map
col_breaks <- quantile (getValues(pred_stack_245), probs = seq (0, 1, 0.1), na.rm = TRUE)
plot (pred_stack_245, breaks = col_breaks, col = terrain.colors(11))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

# zoom to get weird greenland corner
col_breaks <- quantile (getValues(pred_stack_245), probs = seq (0, 1, 0.1), na.rm = TRUE)
plot (pred_stack_245, breaks = col_breaks, col = terrain.colors(11),
      xlim = c(-33, 0), ylim = c (58, 72))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

