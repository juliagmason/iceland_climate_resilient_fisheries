# Write interpolated matlab CMIP6 projections into raster bricks

# 7/20/2020 

library (R.matlab)
library (stringr)
library (raster)

# Lat and lon from OISST
lon.m <- readMat("../Documents/MATLAB/CMIP6/oisst_lon.mat")
lat.m <- readMat("../Documents/MATLAB/CMIP6/oisst_lat.mat")

# actually get data from matlab file
lon <- lon.m$oisst.lon
lat <- lat.m$oisst.lat

# Set up raster extent
latlons <- expand.grid (lon = lon, lat = lat)
coordinates (latlons) <- ~lon + lat
projstring <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towsg84=0,0,0"
proj4string(latlons) <- CRS(projstring)

# create folder to hold deltas
#dir.create ("Data/CMIP6_deltas")
dir.create ("Data/CMIP6_delta_projections")


# SST files in CMIP6 folder
base_path <- "../Documents/MATLAB/CMIP6/"
file_list <- list.files (path = base_path, pattern = "projection.mat")
#file_list2 <- file_list[which (grepl("projection", file_list))]

# now doing bt [if redoing, can do all at once]
#bt_list <- list.files (path = base_path, pattern = "bt_deltas.mat")

for (file in file_list) {
  
  #open matlab file
  mat <- readMat(paste0(base_path, file))
  
  # in list format, so get actual data
  deltas <- mat[[1]]
  
  #rotate and reverse so matches lat/lon
  #https://astrostatistics.psu.edu/su07/R/library/base/html/aperm.html
  rot <- aperm(deltas, c(2, 1,3))
  rev <- apply (deltas, c(2,3), rev)
  
  # rasterize and set extent
  deltas_r <- brick(rev)
  projection(deltas_r) <- CRS(projstring)
  extent (deltas_r) <- c(xmn = min(latlons$lon), xmx = max (latlons$lon), ymn = min(latlons$lat), ymx = max(latlons$lat))
  
  # Write and save raster
  fname <- str_sub (file, end = -5) # take off .mat
  
  deltas_r <- writeRaster(deltas_r, filename = paste0 ("Data/CMIP6_delta_projections/", fname, ".grd"), overwrite = TRUE) # add overwrite = TRUE if redoing
  
}
