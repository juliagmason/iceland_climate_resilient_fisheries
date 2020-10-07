# Rasterize GLORYS
# 9/10/2020
# JGM

library (ncdf4)
library (raster)

# load standardized raster template
load ("Data/prediction_raster_template.RData") # named pred_r_template

projstring <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'

#glorys_files <- list.files (path = "Data", pattern = "glorys.*month.*.nc") 

# glorys 16 files are where I took one day of each month (the 16th) instead of trying to download the daily data. I figured it's equivalent to monthly observation data. 
glorys_files <- list.files (path = "Data", pattern = "glorys.*16.*.nc") 

rasterize_glorys_fun <- function (file) {
  
  # split file name to get the elements I want. want to take off .nc and also split on the underscore, so will split twice.
  # remove .nc-will use this as the raster file name
  file_base <- strsplit (file, "[.]")[[1]][1]
  file_split <- strsplit (file_base, "_")[[1]]
  
  # open netcdf and get variables
  glorys_nc <- nc_open (file.path ("Data", file))
  
  lat <- ncvar_get (glorys_nc, 'latitude')
  lon <- ncvar_get (glorys_nc, 'longitude')
  latlons <- expand.grid (lon, lat); colnames (latlons) <- c('lon', 'lat')
  
  # get either sst, 'thetao', or bottom temp, 'bottomT'
  ifelse (file_split[2] == "sst", 
          temp <- ncvar_get (glorys_nc, "thetao"),
          temp <- ncvar_get (glorys_nc, "bottomT"))
  
  # transpose, rotate, and convert to brick
  t_rot <- aperm (temp, c (2, 1, 3))
  t_rev <- apply (t_rot, c(2,3), rev)
  
  temp_br <- brick (t_rev)
  
  # set brick projection and extent
  projection (temp_br) <- CRS(projstring)
  
  extent (temp_br) <- c (xmn = min (latlons$lon),
                       xmx = max (latlons$lon),
                       ymn = min (latlons$lat),
                       ymnx = max (latlons$lat))
  
  temp_resamp <- raster::resample (temp_br, pred_r_template, method = "bilinear")
  
  # Write and save raster
  writeRaster(temp_resamp, filename = paste0("Data/", file_base, ".grd"), overwrite = TRUE)
  
}

sapply (glorys_files, rasterize_glorys_fun)

# Calculate and rasterize standard deviation----

sst_gl <- brick ("Data/glorys_sst_16_sample.grd")
bt_gl <- brick ("Data/glorys_bt_16_sample.grd")
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

sst_gl_dev <- multiFocal(sst_gl, fun = function (x) sd (x, na.rm = TRUE))
system.time(bt_gl_dev <- multiFocal (bt_gl, fun = function (x) sd (x, na.rm = TRUE))) # 5mins

writeRaster (sst_gl_dev, filename = "Data/glorys_sst_dev.grd", overwrite = TRUE)
writeRaster (bt_gl_dev, filename = "Data/glorys_bt_dev.grd", overwrite = TRUE)
