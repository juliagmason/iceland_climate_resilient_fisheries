## Calculate and plot percent change in thermal suitable habitat
# JGM
# 9/9/2020

library (raster)
library (tidyverse)
library (rgeos)

pred_files <- list.files (path = "Models/Prediction_bricks", pattern = ".*Smooth_latlon.*2061_2080.grd") #, full.names = TRUE)
hist_files <- list.files (path = "Models/Prediction_bricks", pattern = ".*Smooth_latlon.*2000_2018.grd", full.names = TRUE)

# list of species names in Models folder
#load ("Models/spp_Smooth_latlon.RData")

# make data table: species, scenario, period, mean, sd

hab_change <- data.frame ()

for (file in hist_files) {
  
  # load file as raster brick
  br <- brick (file.path ("Models/Prediction_bricks", file))
  projection (br) <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
  
  # Morely et al. 2018 just use sum of values for each year, then takes mean
  br_sum <- cellStats(br, stat = 'sum')
  mean_Morely <- mean (br_sum)
  sd_Morely <- sd (br_sum)
  
  # Kleisner 2017 selects all points with values greater than one sd above the mean, then computes area of those kernels. but what mean?? For now, will take overall mean of historical period--just one value. calc is significantly faster than overlay for this (1.4 vs 9s for test brick). 
  
  # rgeos gArea vs. geosphere areaPolygon??
  # https://gis.stackexchange.com/questions/264517/why-does-projection-not-affect-answer-calculated-by-rgeosgarea
 
  hist_mn <- cellStats(calc (br, fun = mean), stat = "mean")
  hist_sd <- cellStats(calc (br, fun = mean), stat = "sd")
  
  # suitable habitat value is mean + one sd
  suit_val <- hist_mn + hist_sd
  
  # https://stackoverflow.com/questions/45428129/how-to-subset-a-raster-based-on-grid-cell-values
  
  # convert brick to points df, subset values > suit_val
  hist_suit <- rasterToPolygons(br[[100]], fun = function (x){ x > suit_val})
  projection (hist_suit) <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
  
  # http://taromieno.netlify.com/post/raster_to_polygons_2018/
  hist_suit_sp <- as (br, 'SpatialPolygonsDataFrame') # this works for brick, but can't do subset?
  
  # https://www.rdocumentation.org/packages/taRifx/versions/1.0.3/topics/subsetSPDF
  x <- subsetSPDF (hist_suit_sp, x > suit_val)
  
  # maybe this would be helpful?
  # https://cengel.github.io/R-spatial/spatialops.html
  
  hist_area <- gArea (hist_suit) # this gives 10.7, no idea what the units are
  
  library (geosphere)
  area_geosphere <- areaPolygon(hist_suit)/1e6 # this gives 174 values (for 174 cells > suit_val)
  
  # just record number of cells above suit val for now?
  dim(hist_suit)[1]
  
  
  
  # get values of each layer of brick
  hist_vals <- getValues (br[[1]]) # 33600
  length (which (hist_vals > suit_val))
  
  # break up elements of filename to make it easier to parse
  file_split <- strsplit (file, "_")[[1]]
  
  # fill out temporary df 
  
  hab_df <- data.frame (
    # species name will be first two elements of file_split
    species = paste (file_split[1], file_split[2], sep = "_"),
    
    # take first year as period indicator so I don't have to cut off .grd
    period = file_split[length(file_split) -1],
    
    mean_Morely = mean_Morely,
    sd_Morely = sd_Morely,
    
    
    
    
    
    
    
    
  )

  
  
  
  # fill data frame with overall mean and sd for each period
  # how do I do this tidily. two rows for two time periods. 
  
  
}

(species = character(),
                          scenario = factor(),
                          period = factor(),
                          hab_mean = numeric(),
                          hab_sd = numeric()
                          )



system.time(mn <- cellStats(test_mn, stat = 'sum'))
system.time(sd <- cellStats(test_mn, stat = 'sd'))
