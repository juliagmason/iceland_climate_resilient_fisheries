# GFDL delta test

library (R.matlab)

g245_sst_delta <- readMat("../Documents/MATLAB/CMIP6/gfdl_245_sst_deltas.mat")

lon <- readMat("../Documents/MATLAB/CMIP6/oisst_lon.mat")
lat <- readMat("../Documents/MATLAB/CMIP6/oisst_lat.mat")

lon <- lon$oisst.lon
lat <- lat$oisst.lat

g245 <- g245_sst_delta$gfdl.245.sst.delta

image(lon, lat, g245[,,1])
maps::map("world", add = TRUE)

latlons <- expand.grid (lon = lon, lat = lat)
coordinates (latlons) <- ~lon + lat
projstring <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towsg84=0,0,0"
proj4string(latlons) <- CRS(projstring)

# transpose
#https://astrostatistics.psu.edu/su07/R/library/base/html/aperm.html
rot <- aperm(g245, c(2, 1,3))
rev <- apply (g245, c(2,3), rev)

g245_r <- brick(rev)
projection(g245_r) <- CRS(projstring)
extent (g245_r) <- c(xmn = min(latlons$lon), xmx = max (latlons$lon), ymn = min(latlons$lat), ymx = max(latlons$lat))

# Write and save raster
g245_r <- writeRaster(oisst_r, filename = "Data/GFDL_245_sst_deltas.grd")
