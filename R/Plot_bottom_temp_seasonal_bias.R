## Plot seasonal bias of model bottom temperature and GINS data
# 5/18/2020

# JGM

# GINS data are even 0.25 increments, 50.125 - 85.125 lat, -44.875 - 15.125 lon

library (ncdf4)

# sample gins_nc
gins_nc <- nc_open ("../Documents/MATLAB/GINS/gins_8594_t13_04.nc")

gins_lat <- ncvar_get(gins_nc, "lat") # 50.125 to 85.125, 0.25 increments
gins_lon <- ncvar_get (gins_nc, "lon") # -44.875 to 15.125, 0.25 increments

# GFDL: gridded 1440x1080 lat and lon. 
load ("Data/gfdl_lat.RData") 
load ("Data/gfdl_lon.RData")

# check differences in gridding between the models. 
load ("Data/gfdl_sst.RData") # lon looks same as ipsl, but lat is distorted differently
load ("Data/IPSL_sst.RData") # south america in middle
load ("Data/mohc_sst.RData") # looks same as ipsl

library (raster)
ipsl_rot <- apply (t(ipsl_sst_mn[,,1]), 2, rev)
ipsl_r_tmp <- raster(ipsl_rot)
plot (ipsl_r_tmp)

gfdl_rot <- apply (t(gfdl_sst_mn[,,1]), 2, rev)
gfdl_r_tmp <- raster(gfdl_rot)
plot (gfdl_r_tmp)

mohc_rot <- apply (t(mohc_sst_mn[,,1]), 2, rev)
mohc_r_tmp <- raster(mohc_rot)
plot (mohc_r_tmp)
