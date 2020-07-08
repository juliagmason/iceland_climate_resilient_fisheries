## Regrid data
# 5/18/2020
# revisited 7/8/2020

# JGM

# https://rpubs.com/markpayne/132500

library (ncdf4)
library (raster)
library (mapdata)
library(akima)
library (reshape)

# load OISST target file and sample GFDL file
oisst <- nc_open ("../Documents/Python Scripts/oisst_month_climatology_1982_2011.nc")
print (oisst)
# only sst is a variable. lat, lon, month are dimensions...

oisst_lat <- ncvar_get (oisst, "lat")
oisst_lon <- ncvar_get (oisst, "lon")

oisst_sst <- ncvar_get (oisst, "sst")

nc_close(oisst)

# create lat/lon lookup table

# he gets lon and lat from raster, but that only works if I have lat and lon explicitly as variables. 
#lon <- raster ("../Documents/Python Scripts/oisst_month_climatology_1982_2011.nc", varname = "lon")
lon <- raster(as.vector(oisst_lon))
remap.tbl <- data.frame(coordinates(lon),
                        lon=as.vector(oisst_lon),lat=as.vector(oisst_lat))

# can't do coordinates for an array?

# Set up raster extent as I would do ordinarily?
latlons <- expand.grid (lon = oisst_lon, lat = oisst_lat)
coordinates (latlons) <- ~lon + lat
projstring <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towsg84=0,0,0"
proj4string(latlons) <- CRS(projstring)

oisst_rot <- aperm(oisst_sst, c(2, 1,3))
oisst_rev <- apply (oisst_rot, c(2,3), rev)

oisst_r <- brick(oisst_rev)
projection(oisst_r) <- CRS(projstring)
extent (oisst_r) <- c(xmn = min(latlons$lon), xmx = max (latlons$lon), ymn = min(latlons$lat), ymx = max(latlons$lat))

# plot check
plot (oisst_r, 10)


# make an empty extent raster
geo.r <- raster (extent (-45, 15, 50, 85))
res (geo.r) <- c(0.25, 0.25)

# interpolate
r.coords.x <- interp(latlons$lon,latlons$lat,latlons$x,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords.y <- interp(remap.NA$lon,remap.NA$lat,remap.NA$y,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords <- expand.grid(lon=xFromCol(geo.r),
                        lat=yFromRow(geo.r))
r.coords$x <- as.vector(r.coords.x$z)
r.coords$y <- as.vector(r.coords.y$z)


# try gfdl----
g_245 <- nc_open ("../Documents/Python Scripts/gfdl_245_sst_crop.nc")
print (g_245) # luckily still has lat and lon in variables

g_lon <- raster("../Documents/Python Scripts/gfdl_245_sst_crop.nc",varname="lon")
g_lat <- raster("../Documents/Python Scripts/gfdl_245_sst_crop.nc",varname="lat")

# Error in .rasterObjectFromCDF(x, type = objecttype, band = band, ...) : 
#   cells are not equally spaced; you should extract values as points

#We now convert these to a data.frame
library(reshape)
remap.tbl <- data.frame(coordinates(lon),
                        lon=as.vector(lon),lat=as.vector(lat))
tail(remap.tbl)

library (reshape)
gfdl_remap_tbl <- data.frame (coordinates (lon_gfdl_r),
                              lon = as.vector (lon_gfdl_r),
                              lat = as.vector (lat_gfdl_r))
# gfdl lon is -300 to 60

gfdl_remap_tbl$lon <- gfdl_remap_tbl$lon + 120
range(gfdl_remap_tbl$lon)

library(maps)
map("world",fill=TRUE,mar=c(0,0,0,0))
points(lat ~lon,gfdl_remap_tbl,pch=".",col="red")
##????

gfdl_r <- raster (extent (-45, 15.25, 50, 85.25)) # gins extent, but make slightly larger? fixes issues with NAs
res (gfdl_r) <- c(0.25, 0.25)


remap.NA <- subset(gfdl_remap_tbl,lon>=-44.875 & lon<=15.125 & lat >= 50.125 & lat<= 85.125)

r.coords.x <- interp(remap.NA$lon,remap.NA$lat,remap.NA$x,
                     xo=xFromCol(gfdl_r),yo=yFromRow(gfdl_r),
                     linear = F)
# https://stackoverflow.com/questions/43003065/r-understading-na-values-in-the-result-of-akimainterp
# not sure if this is the right thing to do??
r.coords.y <- interp(remap.NA$lon,remap.NA$lat,remap.NA$y,
                     xo=xFromCol(gfdl_r),yo=yFromRow(gfdl_r),
                     linear = F)
r.coords <- expand.grid(lon=xFromCol(gfdl_r),
                        lat=yFromRow(gfdl_r))

r.coords$x <- as.vector(r.coords.x$z)
r.coords$y <- as.vector(r.coords.y$z)


b <- brick(gfdl_sst_mn)
r.coords.sp <- r.coords
coordinates(r.coords.sp) <- ~x +y
r.coords$temp <- extract(b[[1]],r.coords.sp,method="bilinear")
gfdl_r <- setValues(gfdl_r,r.coords$temp)


plot(geo.r)
map("worldHires",add=TRUE)