# Calculate rugosity
# 5/25/21
# JGM


library (raster)
library (marmap) # for depth layer

# load standardized raster template
load ("Data/prediction_raster_template.RData") # named pred_r_template

depth20 <- getNOAA.bathy(lon1 = -45, lon2 = 15,
                       lat1 = 50, lat2 = 85, 
                       resolution = 15)



depth_r <- as.raster(depth20)
res (depth_r)

#Morely et al. does mean of abs differences in 8 surrounding cells, averaged to 0.05x0.05 degrees. 
# Gemma does stdev of depth

system.time(rug_sd_4m <- focal (depth_r, w = matrix(1, nc = 3, nr = 3), fun = sd, na.rm = TRUE))
plot (rug_sd_4m)

mean_abs_diff <- function (x, y) {
  diff <- y - x
  abs_diff <- abs(diff)
}

# https://www.rdocumentation.org/packages/raster/versions/2.6-7/topics/terrain
f <- matrix(1, nrow=3, ncol=3)

rug_abs_4m <- focal(depth_r, w=f, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8, pad=TRUE, padValue=NA)

# test difference in rugosity values for different resolutions?

rug_size_test <- function (res) {
  
  start_time <- Sys.time()
  depth <- getNOAA.bathy(lon1 = -45, lon2 = 15,
                         lat1 = 50, lat2 = 85, 
                         resolution = res)
  
  
  
  depth_r <- as.raster(depth)
  
  f <- matrix(1, nrow=3, ncol=3)
  
  rug_abs <- focal(depth_r, w=f, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8, pad=TRUE, padValue=NA)
  
  rug_rs <- raster::resample (rug_abs, pred_r_template, method = "bilinear")
  
  #rug_p <- plot (rug_abs, main = paste0 ("Rugosity mean abs diff, ", res, " min res"))
  
  png (paste0("Figures/Rugos_test_", res, ".png"), width = 3, height = 4, units = "in", res= 150)
  plot (rug_abs, main = paste0 ("Rugosity mean abs diff, ", res, " min res"))
  dev.off()
  
  
  vals <- getValues (rug_abs)
  vals_rs <- getValues (rug_rs)
  
  end_time <- Sys.time()
  
  stats_df <- data.frame (
    resolution = res,
    mean = mean(vals, na.rm = TRUE),
    sd = sd(vals, na.rm = TRUE),
    mean_rs = mean (vals_rs, na.rm = TRUE),
    sd_rs = sd (vals_rs, na.rm = TRUE),
    time = end_time - start_time
  )
  
  
}

x <- rug_size_test (14)

# 11, 13 doesn't work?
test_seq <- c(1:10, 12, 14:15)
system.time(rug_test <- map_dfr (test_seq, rug_size_test)) # 88s

save (rug_test, file = "Data/Rugosity_test_absdiff_resample.RData")

# 4 is 0.06, 10 is 0.17, 20 is 0.33. 15 is 0.25 degrees

# but also have to look at what happens once I resample to the 25x25 degree cell. 