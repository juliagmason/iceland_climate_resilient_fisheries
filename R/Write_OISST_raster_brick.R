## Write raster brick of NOAA OISST data for MFRI survey extent
# 6/26/2020
# JGM


library (ncdf4)
library (raster)

# Have monthly means from 1985-01 to 2020-06 calculated and subset for Iceland waters (MFRI survey extent) from python
oisst_mfri <- nc_open ("../../Documents/Python Scripts/oisst_mfri_month_mean.nc")

oisst_lat <- ncvar_get (oisst_mfri, "lat")
oisst_lon <- ncvar_get (oisst_mfri, "lon")

oisst <- ncvar_get (oisst_mfri, "sst")

nc_close (oisst_mfri)

# Set up raster extent
latlons <- expand.grid (lon = oisst_lon, lat = oisst_lat)
coordinates (latlons) <- ~lon + lat
projstring <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towsg84=0,0,0"
proj4string(latlons) <- CRS(projstring)

# transpose
#https://astrostatistics.psu.edu/su07/R/library/base/html/aperm.html
oisst_rot <- aperm(oisst, c(2, 1,3))
oisst_rev <- apply (oisst_rot, c(2,3), rev)

oisst_r <- brick(oisst_rev)
projection(oisst_r) <- CRS(projstring)
extent (oisst_r) <- c(xmn = min(latlons$lon), xmx = max (latlons$lon), ymn = min(latlons$lat), ymx = max(latlons$lat))

# Write and save raster
oisst_r <- writeRaster(oisst_r, filename = "Data/OISST_MFRI.grd")
