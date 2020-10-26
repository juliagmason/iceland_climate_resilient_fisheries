## Build predictor rasters
# 7/27/2020
# JGM


library (raster)
library (marmap) # for depth layer
library (mgcv)
library (sf) # for EEZ shapefile

# I'll run predictions for all species, all models, both scenarios. Will set up the static variables, then bring in the temperature bricks for each model and scenario. Clipping to Iceland EEZ for now to save time. Predict on each layer (year) of the brick for each climate model. Also take a mean of all the climate models. 

#dir.create ("Models/Prediction_bricks")

# load standardized raster template
load ("Data/prediction_raster_template.RData") # named pred_r_template

# iceland EEZ shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=5680
eez <- st_read("Data/eez.shp")

# function for applying focal statistics on a brick
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

# rasterize and resample to my template
depth_r <- as.raster(depth)
depth_r <- raster::resample (depth_r, pred_r_template, method = "bilinear")

# # Year ----
# # make raster, but don't assign value yet
# year_r <- raster()
# extent (year_r) <- extent (pred_r_template)
# year_r <- raster::resample (year_r, pred_r_template, method = "bilinear") # not sure why I needed this (from PD sharks code)


# Bormicon region ----
borm_r <- raster ("Data/bormicon_divisions.grd")

# vector of names ----
# this will depend on the terms in the GAM

#stack_names <- c("lat", "lon", "year", "surface_temp", "bottom_temp", "tow_depth_begin")

# td names
stack_names <- c("bormicon_region", "surface_temp", "bottom_temp", "tow_depth_begin", "sst_max", "sst_min", "bt_max", "sst_dev", "bt_dev")

# tensor simple names
#stack_names <- c("lat", "lon", "surface_temp", "bottom_temp", "tow_depth_begin")

# Future predictions ----
# function to go through each species, CM, scenario ----

# tried putting vector of year_vals in function argument but get errors--attempt to apply non-function. maybe better to set it out here. do 2060-2080, to match with kristin/CM26
year_vals <- rep (2061:2080, each = 12) # model projections are monthly
 
predict_brick_fun <- function (sci_name, GAM, scenario) {
  # sci_name is the scientific name separated by an underscore 
  # GAM is the name of the model (name of directory where I saved them in "Fit_GAMS_function.R")
  # scenario is either 245 or 585
  
  print (sci_name)
  print (Sys.time())
  start1 <- Sys.time()
  
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
  # CM2.6 run corresponds to 585 scenario, but doesn't have a 245 equivalent. 
  if (scenario == 585) {
    CM_list = c("gfdl", "cnrm", "ipsl", "mohc", "CM26")
  } else {CM_list = c("gfdl", "cnrm", "ipsl", "mohc") }
  
  for (CM in CM_list) {
    #print (CM)
    
    # load temperature bricks
    sst_brick <- brick (paste0("Data/CMIP6_delta_projections/", CM, "_", scenario, "_sst_projection.grd"))
    bt_brick <- brick (paste0("Data/CMIP6_delta_projections/", CM, "_", scenario, "_bt_projection.grd"))
    
    
    # make first prediction raster to start the stack
    
    # Kind of hard coding this because CM26 is a different size. working backwards to do last period
    start_val <- ifelse (CM == "CM26",
                         dim(sst_brick)[3] - length (year_vals) ,
                         dim(sst_brick)[3] - (length (year_vals) + 240)
    )
    # start in 2061
    sst_r <- sst_brick[[start_val + 1]] 
    bt_r <- bt_brick[[start_val + 1]]
    
    # also calculate sst min/max and bt max if these are terms in the model
    # subset start value and 11 preceding layers, then calc
    sst_max_r <- calc (
      subset (sst_brick, (start_val - 11):start_val),
    max)

    sst_min_r <- calc (
      subset (sst_brick, (start_val - 11):start_val),
      min)

    bt_max_r <- calc (
      subset (bt_brick, (start_val - 11):start_val),
      max)

    
    # calculate sst and bt dev
    sst_dev_r <- multiFocal(sst_r, fun = function (x) sd (x, na.rm = TRUE))
    bt_dev_r <- multiFocal(bt_r, fun = function (x) sd (x, na.rm = TRUE))
    
    # set values for year if these are terms in the model
    #values (year_r) <- year_vals[1]
    
    # stack all the predictor layers; this will change based on model terms ----
    #stack_1 <- stack (lat_r, lon_r, sst_r, bt_r, depth_r)
    #stack_1 <- stack (lat_r, lon_r, year_r, sst_r, bt_r, depth_r)
    #stack_1 <- stack (sst_r, bt_r, depth_r, sst_max_r, sst_min_r, bt_max_r)
    stack_1 <- stack (borm_r, sst_r, bt_r, depth_r, sst_max_r, sst_min_r, bt_max_r, sst_dev_r, bt_dev_r)
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
    #start_time <- Sys.time()
    for (i in 2:length(year_vals)) { # length of time series

      # if doing the whole time series, progress report
      
      #if (i%%100 == 0) {print (i)}
      sst_i <- sst_brick[[i + start_val]]
      bt_i <- bt_brick[[i + start_val]]
      
      sst_max_i <- calc (
        subset (sst_brick, (i + start_val - 12):(i + start_val -1)),
        max)

      sst_min_i <- calc (
        subset (sst_brick, (i + start_val - 12):(i + start_val -1)),
        min)

      bt_max_i <- calc (
        subset (bt_brick, (i + start_val - 12):(i + start_val -1)),
        max)

      # calculate sst and bt dev
      sst_dev_i <- multiFocal(sst_i, fun = function (x) sd (x, na.rm = TRUE))
      bt_dev_i <- multiFocal(bt_i, fun = function (x) sd (x, na.rm = TRUE))
      #values (year_r) <- year_vals[i]
      
      #stack_i <- stack (lat_r, lon_r, sst_i, bt_i, depth_r)
      #stack_i <- stack (lat_r, lon_r, year_r, sst_i, bt_i, depth_r)
      #stack_i <- stack (sst_i, bt_i, depth_r, sst_max_i, sst_min_i, bt_max_i)
      stack_i <- stack (borm_r, sst_i, bt_i, depth_r, sst_max_i, sst_min_i, bt_max_i, sst_dev_i, bt_dev_i)
      names (stack_i) <- stack_names
      
      clip_i <- mask (stack_i, eez)
      
      predict_PA_i <- raster::predict (clip_i, gam_PA, type = "response")
      predict_LB_i <- raster::predict (clip_i, gam_LB, type = "response")
      
      predict_therm_i <- overlay (predict_PA_i, predict_LB_i, 
                                fun = function (x,y) {x * exp(y)}
                                )
    
      # iteratively stack predictions for each year
      pred_stack <- stack (pred_stack, predict_therm_i)
    
    
    } # end year_vals for loop
    #end_time = Sys.time()
  
    #print (end_time - start_time)
    
    # save each CM prediction
    CM_save_path <- paste (sci_name, GAM, CM, scenario, year_vals[1], year_vals[length(year_vals)], sep = "_")
    writeRaster (pred_stack, file = paste0("Models/Prediction_bricks/", CM_save_path, ".grd"), overwrite = TRUE)
    
    # keep each individual CM stack in my environment, give unique model name so I can stack them all
    stack_name <- paste0 (CM, "_stack")
    assign (stack_name, pred_stack)
    

  } # end CM for loop
    
  # stack up all the climate model predictions
  if (scenario == 585) {
    ensemble_stack <- stack (gfdl_stack, cnrm_stack, ipsl_stack, mohc_stack, CM26_stack)
    } else {ensemble_stack <- stack (gfdl_stack, cnrm_stack, ipsl_stack, mohc_stack) }
    
  # convert to brick to make stackapply go faster
  ensemble_brick <- brick (ensemble_stack) # 2.4s for 10 years. 
  
  # take mean of all the climate models
  mean_brick <- stackApply (ensemble_brick, mean, indices = 1:length(year_vals)) # also ~2s
  
  
  # write mean prediction brick as raster
  save_path <- paste (sci_name, GAM, "ensemble_mean", scenario, year_vals[1], year_vals[length(year_vals)], sep = "_")
    
  writeRaster (mean_brick, file = paste0("Models/Prediction_bricks/", save_path, ".grd"), overwrite = TRUE)
  
  end1 <- Sys.time()
  
  # let me know how long that took
  print (end1 - start1)


} # end function


# test on one species 
predict_brick_fun (sci_name = "Lepidorhombus_whiffiagonis", GAM = "Depth_Temp", scenario = 585)

# ==================
# run on full species list that I ran GAMs for----
# ==================

load ("Models/spp_Smooth_latlon.RData")

GAM <- "Smooth_latlon"

system.time (sapply (spp, predict_brick_fun, GAM = GAM, scenario = 585)) # 5 hours for 25 spp, 25 hours for 65 species, 20 ears

system.time (sapply (spp_Smooth_latlon$sci_name_underscore, predict_brick_fun, GAM = GAM, scenario = 245))
sapply (spp_Smooth_latlon$sci_name_underscore, predict_brick_fun, GAM = GAM, scenario = 585)

# run for temp depth ----
load ("Models/spp_Depth_Temp.RData")
system.time (sapply (td_spp_names, predict_brick_fun, GAM = "Depth_Temp", scenario = 245)) #25 hours
sapply (td_spp_names, predict_brick_fun, GAM = "Depth_Temp", scenario = 585)

# run for tensor simple
load ("Models/spp_Smooth_latlon.RData")

sapply (spp_Smooth_latlon$sci_name_underscore, predict_brick_fun, GAM = "Tensor_simple", scenario = 245)
sapply (spp_Smooth_latlon$sci_name_underscore, predict_brick_fun, GAM = "Tensor_simple", scenario = 585)

# failed on squid, continue without and circle back? dims [product 10080000] do not match the length of object [9504768]. worked when I loaded the individual CMs


# failed on l seminudus
# incorrect number of layer names--is the model different for some reason?
# failed for mean of g retrodorsalis
te_spp_2 <- spp_Smooth_latlon$sci_name_underscore[37:65]
te_spp_4 <- spp_Smooth_latlon$sci_name_underscore[53:65]
sapply (te_spp_4, predict_brick_fun, GAM = "Tensor_simple", scenario = 585)

# run on bormicon all temp, start with 25spp
spp_25 <- read_csv ("Data/species_eng.csv") %>%
  filter (n_autumn > 24 & n_spring > 35)

# P platessa and m kitt didn't calculate mean but took all 5 models it looks like. come back to that one later?

# take mean for m kitt and p platessa 585
m_kitt_files <- list.files (path = "Models/Prediction_bricks", pattern = "Microstomus_kitt_Borm_14_alltemp.*_585_2061_2080.grd", full.names = TRUE)

p_plat_files <- list.files (path = "Models/Prediction_bricks", pattern = "Pleuronectes_platessa_Borm_14_alltemp.*_585_2061_2080.grd", full.names = TRUE)
CM_list = c("gfdl", "cnrm", "ipsl", "mohc", "CM26")
for (CM in CM_list) {
  file <- m_kitt_files[grep (CM, m_kitt_files)]
  #file <- p_plat_files[grep (CM, p_plat_files)]
  brick <- brick (file)
  stack_name <- paste0 (CM, "_stack")
  assign (stack_name, brick)
  
}

ensemble_stack <- stack (gfdl_stack, cnrm_stack, ipsl_stack, mohc_stack, CM26_stack)
# convert to brick to make stackapply go faster
ensemble_brick <- raster::brick (ensemble_stack) # 2.4s for 10 years. 
# take mean of all the climate models
mean_brick <- stackApply (ensemble_brick, mean, indices = 1:length(year_vals)) # also ~2s
# write mean prediction brick as raster
save_path <- paste (sci_name, GAM, "ensemble_mean", scenario, year_vals[1], year_vals[length(year_vals)], sep = "_")

writeRaster (mean_brick, file = paste0("Models/Prediction_bricks/", save_path, ".grd"), overwrite = TRUE)

sapply (spp_25$sci_name_underscore[18:25], predict_brick_fun, GAM = "Borm_14_alltemp", scenario = 585)
sapply (spp_25$sci_name_underscore, predict_brick_fun, GAM = "Borm_14_alltemp", scenario = 245) # each about 55 mins

borm_addl <- spp_Smooth_latlon$sci_name_underscore[which (! spp_Smooth_latlon$sci_name_underscore %in% c(spp_25$sci_name_underscore, "Myoxocephalus_scorpius"))]
sapply (borm_addl[34:39], predict_brick_fun, GAM = "Borm_14_alltemp", scenario = 585)
sapply (borm_addl, predict_brick_fun, GAM = "Borm_14_alltemp", scenario = 245)
# stopped at p blennoide

# redo m kitt and s mentella
redo_spp <- c ("Microstomus_kitt", "Sebastes_mentella")
sapply (redo_spp, predict_brick_fun, GAM= "Borm_14_alltemp", scenario = 585)
# do both scenarios for s mentella
predict_brick_fun(GAM= "Borm_14_alltemp", scenario = 245, sci_name = "Sebastes_mentella")









# ==================
## Historical period using GLORYS temperature ----
# ==================

year_vals <- rep (2000:2018, each = 12)

hist_brick_fun <- function (sci_name, GAM) {
  
  print (sci_name)
  start1 <- Sys.time()
  
  # load saved GAM for species of interest
  load (paste0("Models/", GAM, "/", sci_name, "_PA.Rdata")) # gam_PA
  load (paste0("Models/", GAM, "/", sci_name, "_LB.Rdata")) # gam_LB
  
    
  # load temperature bricks
  sst_brick <- brick ("Data/GLORYS_sst_hist.grd")
  bt_brick <- brick ("Data/GLORYS_bt_hist.grd")
  
  sst_max_brick <- brick ("Data/glorys_sst_month_max.grd")
  sst_min_brick <- brick ("Data/glorys_sst_month_min.grd")
  bt_max_brick <- brick ("Data/glorys_bt_month_max.grd")

    
    # make first prediction raster to start the stack
   
    sst_r <- sst_brick[[1]] 
    bt_r <- bt_brick[[1]]
    
    # need to subset year and take max/min. 1993-2018, so 12 mos prior to Jan 2000 [85] would be 73:84
    sst_max_r <- calc (subset (sst_max_brick, 73:84), max)
    sst_min_r <- calc (subset (sst_min_brick, 73:84), min)
    bt_max_r <- calc (subset (bt_max_brick, 73:84), max)
    
    # calculate sst and bt dev
    sst_dev_r <- multiFocal(sst_r, fun = function (x) sd (x, na.rm = TRUE))
    bt_dev_r <- multiFocal(bt_r, fun = function (x) sd (x, na.rm = TRUE))
    
    #values (year_r) <- year_vals[1]
    
    #stack_1 <- stack (lat_r, lon_r, sst_r, bt_r, depth_r)
    #stack_1 <- stack (lat_r, lon_r, year_r, sst_r, bt_r, depth_r)
    #stack_1 <- stack (sst_r, bt_r, depth_r, smax_r, smin_r, bmax_r)
    stack_1 <- stack (borm_r, sst_r, bt_r, depth_r, sst_max_r, sst_min_r, bt_max_r, sst_dev_r, bt_dev_r)
    
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
    #start_time <- Sys.time()
    for (i in 2:length(year_vals)) { # length of time series
      
      # if doing the whole time series, progress report
      
      #if (i%%10 == 0) {print (i)}
      sst_i <- sst_brick[[i]]
      bt_i <- bt_brick[[i]]
      
      sst_max_i <- calc (subset (sst_max_brick, (72 + i):(83 + i)), max)
      sst_min_i <- calc (subset (sst_min_brick, (72 + i):(83 + i)), min)
      bt_max_i <- calc (subset (bt_max_brick, (72 + i):(83 + i)), max)
      
      # calculate sst and bt dev
      sst_dev_i <- multiFocal(sst_i, fun = function (x) sd (x, na.rm = TRUE))
      bt_dev_i <- multiFocal(bt_i, fun = function (x) sd (x, na.rm = TRUE))
      
      #values (year_r) <- year_vals[1]
      
      #stack_i <- stack (lat_r, lon_r, sst_i, bt_i, depth_r)
      #stack_i <- stack (sst_i, bt_i, depth_r, smax_i, smin_i, bmax_i)
      #stack_i <- stack (lat_r, lon_r, year_r, sst_i, bt_i, depth_r)
      stack_i <- stack (borm_r, sst_i, bt_i, depth_r, sst_max_i, sst_min_i, bt_max_i, sst_dev_i, bt_dev_i)
      
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
    #end_time = Sys.time()
    
    #print (end_time - start_time)

  
  pred_brick <- brick (pred_stack) # 2.4s for 10 years. convert to brick to make stackapply go faster
  
  # write prediction brick as raster
  save_path <- paste (sci_name, GAM, "2000_2018", sep = "_")
  
  writeRaster (pred_brick, file = paste0("Models/Prediction_bricks/", save_path, ".grd"), overwrite = TRUE)
  end1 <- Sys.time()
  
  print (end1 - start1)
  
} # end function

hist_brick_fun ("Gadus_morhua", "Smooth_latlon")
cod_test <- brick ("Models/Prediction_bricks/Gadus_morhua_Smooth_latlon_2000_2018.grd")
dim (cod_test)
plot (cod_test, 100)

system.time (sapply (spp, hist_brick_fun, GAM = "Smooth_latlon")) # 4.5 hours

hist_brick_fun ("Gadus_morhua", "Depth_Temp")
sapply (td_spp_names, hist_brick_fun, GAM = "Depth_Temp")
td_spp_names_2 <- td_spp_names[22:length(td_spp_names)]

# tensor simple
sapply (spp_Smooth_latlon$sci_name_underscore, hist_brick_fun, GAM = "Tensor_simple")

# borm 14
sapply (spp_25$sci_name_underscore, hist_brick_fun, GAM = "Borm_14_alltemp")

# start additional spp
borm_addl <- spp_Smooth_latlon$sci_name_underscore[which (! spp_Smooth_latlon$sci_name_underscore %in% c(spp_25$sci_name_underscore, "Myoxocephalus_scorpius"))]
sapply(borm_addl, hist_brick_fun, GAM = "Borm_14_alltemp") # ~ 12 mins each
