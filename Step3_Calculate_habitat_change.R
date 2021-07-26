## Calculate change in thermal suitable habitat projections
# JGM
# 9/9/2020

library (raster)
library (tidyverse)
library (beepr) # for letting me know when long runs finish


# list of species
spp_list  <- read_csv ("Data/species_eng.csv",
                       col_types = cols(
                         Spp_ID = col_factor()
                       )) %>%
  rename (species = sci_name_underscore) %>% #rename to match species column
  # only grab first option for species name (more legible graphs)
  # https://stackoverflow.com/questions/42565539/using-strsplit-and-subset-in-dplyr-and-mutate
  mutate(Common_name = sapply(str_split(Common_name, ","), function(x) x[1]))


# ==================
# Calculate habitat change ----
# ==================

# spp I ran models for
load ("Models/spp_Borm.RData") 

# raster with extent of GFDL model, resolves issues with historical vs. projections coastal NAs
load ("Data/gfdl_ISL_mask.RData")


# Calculate amount of habitat in historical and predicted bricks from my rasters (created in Step2_Predict_ensemble_rasters.R)

# doing both historical and future at the same time takes >2x longer, because repeating the historical one 4-5 times. Instead, make separate df for historical and future, and combine with left_join. 

# historical function ----
sum_habitat_hist <- function (sci_name) {
  
  # load raster brick for historical period
  hist_br <- brick (file.path ("Models/Prediction_bricks", 
                               paste(sci_name, "Rug_nb", "2000_2018.grd", sep = "_"))
  )
  
  # mask to gfdl
  hist_br <- mask(hist_br, gfdl_mask)
  
  # take sum of all the cells for each time step, for Morely et al. 2018 method
  sum_hist <- cellStats(hist_br, stat = 'sum') # this should have 228 values, for each month 2000-2018
  
  df <- data.frame (species = sci_name,
                    hist_mean = mean (sum_hist),
                    hist_med = median (sum_hist),
                    hist_sd = sd (sum_hist))
  
}



# apply function, base r method:

landed_hist_hab <- do.call(rbind, lapply(landed_spp, sum_habitat_hist, GAM = "Borm_14_alltemp"))

# purr method:
system.time (hist_hab <- map_df (borm_spp$sci_name_underscore, sum_habitat_hist)) # 20s for landed_spp, 95s for all spp

# future function ----
sum_habitat_future <- function (sci_name, CM, scenario) {
  # CM is 4-letter lowercase climate model abbreviation
  # scenario is 245 or 585
  pred_br <- brick (file.path ("Models/Prediction_bricks", 
                               paste(sci_name, "Rug_nb", CM, scenario, "2061_2080.grd", sep = "_")
                               )
  )
  
  # mask to gfdl
  pred_br <- mask(pred_br, gfdl_mask)
  
  sum_pred <- cellStats(pred_br, stat = 'sum')
  
  df <- data.frame (species = sci_name,
                    model = CM,
                    scenario = scenario, 
                    pred_mean = mean (sum_pred),
                    pred_med = median (sum_pred),
                    pred_sd = sd (sum_pred)
                    )

  
} # end pred function 


# list of all the possible combinations of species, CM, and scenario. I couldn't figure out how to use cross and filter to get rid of the CM2.6 and 245 combination, but can do with expand_grid 

CM_list <- c("gfdl", "cnrm", "ipsl", "mohc", "CM26")

cm_expand <- expand_grid (sci_name = borm_spp$sci_name_underscore,
                          CM = CM_list,
                          scenario = c(245, 585)) %>%
  filter (!(CM == "CM26" & scenario == 245)) %>% 
  as.list() # convert to list to feed to pmap


system.time(pred_hab <- pmap_dfr (cm_expand, sum_habitat_future)); beep() 
# 233 seconds for landed spp, 530 seconds for all spp

# combine and save
pred_hist_hab <- pred_hab %>%
  left_join (hist_hab, by = "species")
save (pred_hist_hab, file = "Data/pred_hist_hab_df.RData")


# calculate habitat change ----


# Looking at various metrics. Percent change is the most common, but hard to interpret when negative and positive changes are very large. Makes positive changes look way more important than negative changes. 

hab_change <- pred_hist_hab %>%
  left_join (spp_list, by = "species") %>% 
  mutate (log10_foldchange = log10 (pred_med / hist_med),
          log2_foldchange = log2 (pred_med / hist_med),
          perc_change = (pred_med - hist_med) / hist_med * 100,
          simple_ratio = ifelse (pred_med > hist_med,
                                 pred_med/hist_med,
                                 -(hist_med/pred_med)
                                 )
          )

# save
save (hab_change, file = "Data/hab_change_df.RData")
