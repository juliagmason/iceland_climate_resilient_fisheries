## Process GINS data and find bottom
# 5/15/2020
# JGM

library (ncdf4)
library (raster)

#=======================================
### Bottom temperature for seasonal GINS ----
#=======================================
# Previously worked with this in MATLAB
# messy folder with both .nc and converted ".nc.mat" files from matlab attempts, so extract just ".nc" filenames by taking all ".nc" files and subtracting the ".nc.mat" files
gins_files_all <- list.files (path = "../Documents/MATLAB/GINS", pattern = ".nc", full.names = T)
gins_files_mat <- list.files (path = "../Documents/MATLAB/GINS", pattern = ".mat", full.names = T)
gins_files <- setdiff (gins_files_all, gins_files_mat)

# open one to test
gins_tmp <- nc_open(gins_files[1])
print (gins_tmp)
gins_tan <- ncvar_get (gins_tmp, "t_an") # 241 x 141 x 102

# Save gins_depths for future reference
gins_depth <- ncvar_get (gins_tmp, "depth") # 50m increments from 0-2000m, then 100m increments 2100-5500
save (gins_depth, file = "Data/gins_depths.RData")

# I want to grab the winter, spring, summer, fall and average them. Then for each season I want to find the actual bottom temp. file structure is t13 = winter, t14 = spring, 15 is summer, 16 is fall. 

gins_w_files <- gins_files[which (grepl(13, gins_files) == TRUE)]
gins_sp_files <- gins_files[which (grepl(14, gins_files) == TRUE)]
gins_su_files <- gins_files[which (grepl(15, gins_files) == TRUE)]
gins_f_files <- gins_files[which (grepl(16, gins_files) == TRUE)]
gins_files_season <- list ("Winter" = gins_w_files, 
                           "Spring" = gins_sp_files, 
                           "Summer" = gins_su_files,
                           "Fall" = gins_f_files)


# create function that goes through my list of lists. each input will be a list of 3 filenames for the three decades I need. 

find_bottom <- function (filenames){
  gins <- lapply (filenames, nc_open)
  gins_t <- lapply (gins, function (x) ncvar_get(x, "t_an")) # each is 241 x 141 x 102
  
  # close nc files
  lapply (gins, nc_close)
  
  # put the three decades side by side in an array so I can take a mean
  gins_arr <- array (unlist (gins_t), c (241, 141, 102, 3)) 
  
  # take mean along 4th dimension (decade)
  gins_mn <- apply (gins_arr, 1:3, mean, na.rm = TRUE) 
  
  # find deepest non-NA value along 3rd (depth) dimension
  gins_bottom <- apply (gins_mn, c(1,2), function(x) x[max(which(!is.na(x)))]) 
}

gins_bt <- lapply (gins_files_season, find_bottom) # labeled list with bottom temperature for each

# plot to check
lat <- ncvar_get (gins_tmp, "lat")
lon <- ncvar_get (gins_tmp, "lon")

image (lon, lat, gins_bt$Summer)

# save as RData file
save (gins_bt, file = "Data/GINS_bottom_temp_seasonal.RData")

##############################################
### Monthly GINS data ----
#############################################
# 6/25/2020

# Downloaded via python xarray. Three netcdf files, each for a decade: 85-94, 95-04, 05-12. 

# find bottom function that takes different variables----
find_bottom_gins <- function (filename, var){
  
  gins <- nc_open(filename)
  gins_t <- ncvar_get(gins, var) # 241 x 141 x 57 x 12
  nc_close (gins)
  
  # find deepest non-NA value along 3rd (depth) dimension
  gins_bottom <- apply (gins_t, c(1,2,4), function(x) x[max(which(!is.na(x)))]) 
  
}

# Salinity data----

# salinity files by decade, downloaded in python notebook
gins_sal_files <- list.files (path = "../Documents/Python Scripts/", pattern = "GINS_sal_", full.names = T)

gins_bottom_sal <- lapply (gins_sal_files, find_bottom_gins) # list with 3 elements

# make into a raster brick

# turn list into array
gins_bottom_sal_array <- array (unlist (gins_bottom_sal), 
                                c(241, 141,36))

# open one nc to get lat and lon
gins <- nc_open (gins_sal_files[1])
lat <- ncvar_get (gins, "lat")
lon <- ncvar_get (gins, "lon")
nc_close(gins)

image (lon, lat, gins_bottom_sal_array[,,1])

latlons <- expand.grid (lon = lon, lat = lat)
coordinates (latlons) <- ~lon + lat
projstring <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towsg84=0,0,0"
proj4string(latlons) <- CRS(projstring)

# transpose
#https://astrostatistics.psu.edu/su07/R/library/base/html/aperm.html
gins_sal_rot <- aperm(gins_bottom_sal_array, c(2, 1,3))
gins_sal_rev <- apply (gins_sal_rot, c(2,3), rev)

gins_sal_r <- brick(gins_sal_rev)
projection(gins_sal_r) <- CRS(projstring)
extent (gins_sal_r) <- c(xmn = min(latlons$lon), xmx = max (latlons$lon), ymn = min(latlons$lat), ymx = max(latlons$lat))

# not assigning names for now, see what would be best

plot (gins_sal_r, 10)

# Write and save raster
gins_sal <- writeRaster(gins_sal_r, filename = "Data/GINS_bottom_sal.grd")

# Temperature data ----
gins_temp_files <- list.files (path = "../Documents/Python Scripts/", pattern = "GINS_temp_", full.names = T)

gins_bottom_temp <- lapply (gins_temp_files, find_bottom_gins, var = "t_an")

# make into array
gins_bt_arr <- array (unlist (gins_bottom_temp), 
                      c(241, 141, 36))

image (lon, lat, gins_bt_arr[,,1])

# transpose and rasterize
gins_bt_rot <- aperm(gins_bt_arr, c(2, 1,3))
gins_bt_rev <- apply (gins_bt_rot, c(2,3), rev)

gins_bt_r <- brick(gins_bt_rev)
projection(gins_bt_r) <- CRS(projstring)
extent (gins_bt_r) <- c(xmn = min(latlons$lon), xmx = max (latlons$lon), ymn = min(latlons$lat), ymx = max(latlons$lat))


plot (gins_bt_r, 1)

# Write and save raster
gins_bt <- writeRaster(gins_bt_r, filename = "Data/GINS_bottom_temp.grd")
