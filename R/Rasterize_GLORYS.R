# Rasterize GLORYS
# 9/10/2020
# JGM

library (ncdf4)
library (raster)

# load standardized raster template
load ("Data/prediction_raster_template.RData") # named pred_r_template

projstring <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'

# historical monthly means, 2000-2018. have not resampled. 
glorys_sst_hist <- nc_open ("Data/glorys_sst_month_mean.nc")
glorys_bt_hist <- nc_open ("Data/glorys_bt_month_mean.nc")
print (glorys_sst_hist)

lat <- ncvar_get (glorys_sst_hist, 'latitude')
lon <- ncvar_get (glorys_sst_hist, 'longitude')
latlons <- expand.grid (lon, lat); colnames (latlons) <- c('lon', 'lat')

sst <- ncvar_get (glorys_sst_hist, 'thetao') # 721 x 421 x 228
bt <- ncvar_get (glorys_bt_hist, 'bottomT')

image (lon, lat, bt[,,1])

# transpose
sst_rot <- aperm(bt, c(2, 1,3))
sst_rev <- apply (sst_rot, c(2,3), rev)

sst_r <- brick(sst_rev)
projection (sst_r) <- CRS(projstring)
extent (sst_r) <- c (xmn = min (latlons$lon),
                     xmx = max (latlons$lon),
                     ymn = min (latlons$lat),
                     ymnx = max (latlons$lat))

sst_resamp <- raster::resample (sst_r, pred_r_template, method = "bilinear")

# Write and save raster
glorys_sst_hist <- writeRaster(sst_resamp, filename = "Data/GLORYS_sst_hist.grd")
glorys_bt_hist <- writeRaster(sst_resamp, filename = "Data/GLORYS_bt_hist.grd")
