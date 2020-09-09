## Calculate and plot percent change in thermal suitable habitat
# JGM
# 9/9/2020

library (raster)
library (tidyverse)

pred_files <- list.files (path = "Models/Prediction_bricks", pattern = ".*Smooth_latlon.*2091_2100.grd", full.names = TRUE)

# make data table: species, scenario, period, mean, sd

hab_change <- data.frame (species = character(),
                          scenario = factor(),
                          period = factor(),
                          hab_mean = numeric(),
                          hab_sd = numeric()
                          )

test <- brick (pred_files[1])
system.time (test_mn <- overlay (test, fun = mean))

hab_change$species[1] <- paste (strsplit (strsplit (pred_files[1], "/")[[1]][3], "_")[[1]][1:2], sep = "_")

system.time(mn <- cellStats(test_mn, stat = 'sum'))
system.time(sd <- cellStats(test_mn, stat = 'sd'))
