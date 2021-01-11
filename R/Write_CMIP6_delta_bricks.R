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

# redo for cnrm sst
base_path <- "../Documents/MATLAB/CMIP6/"
file_list <- list.files (path = base_path, pattern = "cnrm.*.sst_projection.mat")
for (file in file_list) {
  
  #open matlab file
  mat <- readMat(paste0(base_path, file))
  
  # in list format, so get actual data
  deltas <- mat[[1]] #not actually the deltas. actually the projection. 
  
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

# quick check of cnrm 245 vs 585
cnrm_245 <- brick ("Data/CMIP6_delta_projections/cnrm_245_sst_projection.grd")
cnrm_585 <- brick ("Data/CMIP6_delta_projections/cnrm_585_sst_projection.grd")

plot (cnrm_245, 1000)
plot (cnrm_585, 1000)

## # do with CM 2.6----
base_path <- "../Documents/MATLAB/CM2_6/"
#sst_file_list <- list.files (path = base_path, pattern = "sst_projection")
#bt_file_list <- list.files (path = base_path, pattern = "bt_projection")

# repeat with deltas
bt_file_list <- list.files (path = base_path, pattern = "bt_delta")
sst_file_list <- list.files (path = base_path, pattern = "sst_delta")
# did a run-through and they're each only 8MB. I think I can get away with stacking them first. do the first one, and then iteratively stack the rest?
mat1 <- readMat(paste0(base_path, sst_file_list[1]))

# in list format, so get actual data
deltas1 <- mat1[[1]] #not actually the deltas. actually the projection. 

#rotate and reverse so matches lat/lon
#https://astrostatistics.psu.edu/su07/R/library/base/html/aperm.html
#rot1 <- aperm(deltas1, c(2, 1,3))
rev1 <- apply (deltas1, c(2,3), rev)

# rasterize and set extent
sst_stack <- brick(rev1)
projection(sst_stack) <- CRS(projstring)
extent (sst_stack) <- c(xmn = min(latlons$lon), xmx = max (latlons$lon), ymn = min(latlons$lat), ymx = max(latlons$lat))


for (i in 2:length(sst_file_list)) {
  
  file = sst_file_list[i]
  
  #open matlab file
  mat <- readMat(paste0(base_path, file))
  
  # in list format, so get actual data
  deltas <- mat[[1]] #not actually the deltas. actually the projection. 
  
  #rotate and reverse so matches lat/lon
  #https://astrostatistics.psu.edu/su07/R/library/base/html/aperm.html
  #rot <- aperm(deltas, c(2, 1,3))
  rev <- apply (deltas, c(2,3), rev)
  
  # rasterize and set extent
  deltas_r <- brick(rev)
  projection(deltas_r) <- CRS(projstring)
  extent (deltas_r) <- c(xmn = min(latlons$lon), xmx = max (latlons$lon), ymn = min(latlons$lat), ymx = max(latlons$lat))
  
  sst_stack <- raster::stack (sst_stack, deltas_r) # new one goes underneath
  
}

# Write and save raster. label as 585 because it's analogous. 
#CM26_deltas_r <- writeRaster(bt_stack, filename = "Data/CMIP6_delta_projections/CM26_585_bt_projection.grd") #, overwrite = TRUE) # add overwrite = TRUE if redoing
CM26_deltas_r <- writeRaster(sst_stack, filename = "Data/CMIP6_deltas/CM26_585_sst_deltas.grd")
