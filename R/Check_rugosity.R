# Check KMK rugosity data
# 8/18/2020
# JGM

library (rgdal)
library (raster)

Rugosity<-readGDAL("Data/Raw_data/tri_log") # issue was the slash after tri_log :)

ALLres2$IDxx<-1:1449157
myPoints<-unique(data.frame(ALLres2$LON, ALLres2$LAT))
names(myPoints)<-c('LON', 'LAT')
coordinates(myPoints) <- c('LON', 'LAT')

##Create a grid of the CM region:
grd <- GridTopology(cellcentre.offset=c(-79.95000,29.98016), cellsize=c(0.1,0.07603204), cells.dim=c(250,250))
grdSBT <- as.SpatialPolygons.GridTopology(grd)

##Determine which points from the survey are associated with the CM grid:
TEST<-over(myPoints, grdSBT)
test<-data.frame(TEST)
names(test)<-"grid_id"

r<-raster(Rugosity)
rug<-extract(r, myPoints)
myPoints_2<-cbind(myPoints, test, rug)
names(myPoints_2)<-c("LON", "LAT", "grid_id", "rugosity")
