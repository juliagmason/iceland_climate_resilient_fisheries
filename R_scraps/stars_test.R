## STARS test
# 5/22/2020
# JGM

# https://www.mattreusswig.com/post/use-stars-to-visualize-curvilinear-netcdf-rasters/

library(stars)
library (dplyr)
library (tidyverse)
library (ncdf4)

load ("Data/cnrm_sst.RData")
load ("Data/cnrm_lat.RData")
load ("Data/cnrm_lon.RData")

# load sample nc? still need dim data
sst_samp <- read_ncdf("Data/thetao_Omon_CNRM-CM6-1-HR_historical_r1i1p1f2_gn_198501-198912.nc", 
                      var = c("thetao"),
                      ncsub = cbind (start = c(1,1,1,1), count = c(1442, 1050, 1, 1))
                      )# just one layer for now


cnrm_sst_test <- st_as_stars (sst_samp,
                          curvilinear = list (x = lon_cnrm,
                                              y = lat_cnrm))
st_crs(cnrm_sst_test) <- "+proj=longlat +ellps=WGS84 +no_defs"

library (maps)

ggplot () +
  geom_stars (data = cnrm_sst_test) # too big

# attempt to filter
library(cubelyr)
cnrm_crop <- cnrm_sst_test %>%
  dplyr::filter (between (x, -45, 15.25) & between (y, 50, 80.25))

