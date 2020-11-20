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

# set up static variables ----

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


# Bormicon region ----
# I created this in Rasterize_bormicon_regions.R
borm_r <- raster ("Data/bormicon_divisions.grd")

# vector of names ----
# this will depend on the terms in the GAM
stack_names <- c("bormicon_region", "surface_temp", "bottom_temp", "tow_depth_begin", "sst_max", "sst_min", "bt_max", "sst_dev", "bt_dev")


# Future predictions ----
# function to go through each species, CM, scenario ----

predict_list_fun <- function (sci_name, GAM, CM, scenario, month_index) {
  # month_index is numeric, minimum 1 and max 1032. corresponds to layers of CMIP6 projections, jan 2015-dec 2100. If sst_min and max are in the model, then the minimum is 13 (Jan 2016). if CM 2.6 is included, then the maximum is 792, dec 2080. 2061-2080 would be 553:792
  
  # also dependent on arguments in predict_brick_fun:
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
  # CM 2.6 has 960 layers, which correspond to 2001-2080. CMIP6 have 1032 layers, 2015-2100. would I fix this outside the function, in the cm_expand situation? I think the best option is to have a special case here for CM26. If you start with month_index = 1, 2015-01 for CMIP6, that's the equivalent of 169 for CM2.6
  month_val <- ifelse (CM == "CM26",
                       168 + month_index,
                       month_index
                       )
  
  # start in 2061
  sst_r <- sst_brick[[month_val]] 
  bt_r <- bt_brick[[month_val]]
    
  # also calculate sst min/max and bt max if these are terms in the model
  # subset 12 preceeding months, then calc
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

system.time (pred_test <- predict_brick_fun(sci_name = "Gadus_morhua", GAM = "Borm_14_alltemp", CM = "gfdl", scenario = 245, month_index = 33))

# function within a function? feed the inner function the month index, and the the outer function bricks and saves?
# this takes about 15 minutes per model/scenario. 
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


# try pmap with a small cross
small_ls <- list (sci_name = "Gadus_morhua", 
                  GAM = "Borm_14_alltemp",  
                  CM = "gfdl", 
                  scenario = c(245, 585),
                  year1 = 2061,
                  year2 = 2062)

# weird situation where pmap doesn't work on a crossed list, need to "flatten." Works with cross_df, but then need to turn back into a list??
small_cross <- cross_df(small_ls) %>% as.list()
small_expand <- expand.grid (sci_name = "Gadus_morhua", 
                              GAM = "Borm_14_alltemp",  
                              CM = "gfdl", 
                              scenario = c(245, 585),
                              year1 = 2061,
                              year2 = 2080) %>% as.list()

system.time (pmap (small_cross, predict_brick_fun)) # 287 seconds for 2 years, 2 scenarios

system.time (predict_brick_fun (sci_name = "Gadus_morhua", 
                                GAM = "Borm_14_alltemp",  
                                 CM = "gfdl", 
                                 scenario = 245,
                                 year1 = 2061,
                                 year2 = 2080)
)

library (beepr)
library (tidyverse)

# run on capelin

 CM_list <- c("gfdl", "cnrm", "ipsl", "mohc", "CM26")
# need to do expand grid and exclude CM2.6 and 245! :(
 capelin_ls <- expand_grid  (sci_name = "Mallotus_villosus", 
                    GAM = "Borm_14_alltemp",  
                    CM = CM_list,
                    scenario = c(245, 585),
                    year1 = 2061,
                    year2 = 2080) %>%
                    filter (!(CM == "CM26" & scenario == 245)) %>% 
                    as.list()

system.time (pmap (capelin_ls, predict_brick_fun)); beep()

cap_585_ls <- list (sci_name = "Mallotus_villosus", 
                     GAM = "Borm_14_alltemp",  
                     CM = CM_list,
                     scenario =  585,
                     year1 = 2061,
                     year2 = 2080)
system.time (pmap (cap_585_ls, predict_brick_fun)); beep()
# took about 15 minutes per model/scenario. 60 mins for 245, 75 mins for 585. more or less the same as the for loop situation

# try lapply
system.time (lapply (CM_list, predict_brick_fun, 
                     sci_name = "Scomber_scombrus", 
                     GAM = "Borm_14_alltemp",  
                     scenario =  585,
                     year1 = 2061,
                     year2 = 2080)
             ); beep()

#3925.65 s, 65 mins. so slightly faster?
system.time (pmap (list (sci_name = "Scomber_scombrus", 
                         GAM = "Borm_14_alltemp",  
                         CM = CM_list[-5],
                         scenario =  245,
                         year1 = 2061,
                         year2 = 2080), 
                   predict_brick_fun)); beep(sound = 5) # 3235.67 

# try mapply?
a_raj_ls <- expand_grid  (sci_name = "Amblyraja_radiata", 
                          GAM = "Borm_14_alltemp",  
                          CM = CM_list,
                          scenario = c(245, 585),
                          year1 = 2061,
                          year2 = 2080) %>%
  filter (!(CM == "CM26" & scenario == 245)) %>% 
  as.list()

# can't figure out how to exclude CM2.6 and 245, so excluding cm2.6 for now
system.time (mapply (predict_brick_fun,  CM = c("gfdl", "cnrm", "ipsl", "mohc"), scenario = c(245, 585), MoreArgs = list (sci_name = "Amblyraja_radiata",GAM = "Borm_14_alltemp", year1 = 2061, year2 = 2080))); beep() # 3622 s, still ~15 mins each. also I forgot that it wouldn't iterate over all the options. 

system.time (mapply (predict_brick_fun,  CM = c("gfdl", "cnrm", "ipsl", "mohc", "CM26"), scenario = c(585, 245), MoreArgs = list (sci_name = "Amblyraja_radiata",GAM = "Borm_14_alltemp", year1 = 2061, year2 = 2080))); beep()
#4745.94 s

#https://amber.rbind.io/2018/03/26/purrr/


  
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

# stack names to match model formula
stack_names <- c("bormicon_region", "surface_temp", "bottom_temp", "tow_depth_begin", "sst_max", "sst_min", "bt_max", "sst_dev", "bt_dev")

# load temperature bricks
sst_brick_hist <- brick ("Data/GLORYS_sst_hist.grd") # these go from 2000-2018; 228 layers
bt_brick_hist <- brick ("Data/GLORYS_bt_hist.grd")

sst_max_brick_hist <- brick ("Data/glorys_sst_month_max.grd") # these go from 1993-2018; 312 layers
sst_min_brick_hist <- brick ("Data/glorys_sst_month_min.grd")
bt_max_brick_hist <- brick ("Data/glorys_bt_month_max.grd")

# historical function ----
hist_brick_fun <- function (sci_name, GAM, month_index) {
  #month_index is a number from 1-228, for 2000-2018
  # sci_name is sci_name_underscore
  # GAM is the GAM name/folder
  
  # load saved GAM for species of interest
  load (paste0("Models/", GAM, "/", sci_name, "_PA.Rdata")) # gam_PA
  load (paste0("Models/", GAM, "/", sci_name, "_LB.Rdata")) # gam_LB
  
  
  # subset temperature layers
   # regular sst and bottom t are 
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

hist_test <- hist_brick_fun (
  sci_name = "Gadus_morhua",
  GAM = "Borm_14_alltemp",
  month_index = 3
)

system.time (hist_test2 <- lapply (1:5, hist_brick_fun, sci_name = "Gadus_morhua",
                      GAM = "Borm_14_alltemp"))

system.time ()
system.time (hist_test_br <- brick (hist_test2))

 