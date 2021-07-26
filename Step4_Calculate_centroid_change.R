## Projected change in centroid of thermal suitable habitat distribution 
# 8/21/2020
# JGM

library (geosphere) # for distHaversine and bearing
library (raster)
library (tidyverse)
library (beepr) # for alerting me when code is done
library (SDMTools) # for calculating circular average. Not available for R version 4.0.2, downloaded from https://www.rforge.net/SDMTools/git.html
#library (spatstat) # for weighted quantiles


# species data
spp_list  <- read_csv ("Data/species_eng.csv",
                       col_types = cols(
                         Spp_ID = col_factor()
                       )) %>%
  rename (species = sci_name_underscore) %>% #rename to match species column
  # Common names have several options separated by commas, only grab first option for more legible graphs
  # https://stackoverflow.com/questions/42565539/using-strsplit-and-subset-in-dplyr-and-mutate
  mutate(Common_name = sapply(str_split(Common_name, ","), function(x) x[1]), 
         Common_name = gsub ("Atlantic", "Atl", Common_name))


################################################################
# Calculate change in centroid of distribution ----
# Code from Morely et al. 2018, calculating % habitat change as change in sum of prediction values

# trying something different, with purr. Still think it's faster to separate historical and future
hist_centroids_fun <- function (sci_name) {
  # load raster brick for historical period
  if (sci_name %in% c("Molva_molva", "Hippoglossoides_platessoides")) {
    hist_br <- brick (file.path ("Models/Prediction_bricks", 
                                 paste0("Rug_nb_depth_crop", sci_name, "_Rug_nb_", "2000_2018.grd"))
    )
  } else {
    
    hist_br <- brick (file.path ("Models/Prediction_bricks", 
                                 paste(sci_name, "Rug_nb", "2000_2018.grd", sep = "_"))
    )
    
  }
  
  # take mean so have one value per period
  hist_mn <- calc (hist_br, median) # calc is faster than StackApply
  
  # spatial points df so I have a value corresponding to each lat/lon combo
  df_pre <- data.frame(rasterToPoints(hist_mn))
  
  
  # subset above a certain threshold for warm and cool edge. for cod, 0.05 is 67th percentile and 0.5 is 70th percentile. go with 0.5? Tried this, changing to 0.05 because was zero for a lot of species
  df_pre_suit <- filter (df_pre, layer > 0.05)
  
  # eventually want something about circular movement around iceland??
  data.frame (
    species = sci_name,
    hist_lat = weighted.mean(df_pre$y, df_pre$layer), # layer is the thermpred prediction value
    hist_lon =  weighted.mean(df_pre$x, df_pre$layer),
    #warm and cool edges, Fredston-Herman 2020. should probably get this from obs, not backcast? 5th and 95th latitude quantile
    # might not be that helpful because i'm limiting to EEZ
    # weighted quantile is making these very central. Fredston used 5th and 95th where occurs above 80% probability, but not sure how to capture that with thermpred. 
    hist_warm = quantile (df_pre_suit$y, probs = 0.05, na.rm = TRUE),
    hist_cold  = quantile (df_pre_suit$y,  probs = 0.95, na.rm = TRUE)
  )
}


load ("Models/spp_Borm.RData") 

system.time( hist_centroids <- map_df (borm_spp$sci_name_underscore, hist_centroids_fun)); beep() # 98s

future_centroids_fun <- function (sci_name, CM, scenario) {
  # load raster brick for historical period
  pred_br <- brick (file.path ("Models/Prediction_bricks", 
                               paste(sci_name, "Borm_14_alltemp", CM, scenario, "2061_2080.grd", sep = "_")
                               )
                    )
  
  #take mean so have one value per period
  pred_mn <- calc (pred_br, median) # calc is faster
  df_post <- data.frame(rasterToPoints(pred_mn))
  
  df_post_suit <- filter (df_post, layer > 0.05)
  
  # eventually want something about circular movement around iceland??
  # calculate mean lon and lat, store as data.frame to match with climate models
  # compile in dataframe
  CM_df <- data.frame (
    species = sci_name,
    model = CM, 
    scenario = scenario,
    pred_lat = weighted.mean(df_post$y, df_post$layer),
    pred_lon = weighted.mean(df_post$x, df_post$layer),
    pred_warm = quantile (df_post_suit$y,  probs = 0.05, na.rm = TRUE),
    pred_cold = quantile (df_post_suit$y,  probs = 0.95, na.rm = TRUE)
  )
}


# list of all the possible combinations of species, CM, and scenario. I couldn't figure out how to use cross and filter to get rid of the CM2.6 and 245 combination
CM_list <- c("gfdl", "cnrm", "ipsl", "mohc", "CM26")

cm_expand <- expand_grid (sci_name = borm_spp$sci_name_underscore,
                          CM = CM_list,
                          scenario = c(245, 585)) %>%
  filter (!(CM == "CM26" & scenario == 245)) %>% 
  as.list() # convert to list to feed to pmap

system.time( pred_centroids <- pmap_dfr (cm_expand, future_centroids_fun)); beep() # 382s landed, 1206.62  full. 923 secodn time

# combine and save 
# start with just centroids, do warm/cold later
centroid_change <- pred_centroids %>%
  left_join (hist_centroids, by = "species") %>%
  mutate (dist = distHaversine (cbind(hist_lon, hist_lat),
                                cbind(pred_lon, pred_lat)),
          bearing = bearing (cbind(hist_lon, hist_lat),
                             cbind(pred_lon, pred_lat)
                             ) # bearing is -180-180
  )

# save 
save (centroid_change, file = "Data/centroids_Rug_allscenarios_DepthCrop.RData")
