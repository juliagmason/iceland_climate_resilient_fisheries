## Regrid data
# 5/18/2020

# JGM

# https://rpubs.com/markpayne/132500

library (ncdf4)
library (raster)

# can I do this with my saved RData files?
lon_gfdl_r <- raster (lon_gfdl)
lat_gfdl_r <- raster (lat_gfdl)

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

library(akima)
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

library (mapdata)
plot(geo.r)
map("worldHires",add=TRUE)