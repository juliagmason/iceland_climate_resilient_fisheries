## Load CMIP6 3D ocean potential temp NetCDFs and extract surface and bottom temperature
# 5/15/2020
# JGM


# The levels are not constant across models. But I'm going to assume that I can use the top and bottom of each one. 
# GINS I took 1985-2012

library(ncdf4)

# GFDL
gfdl <- nc_open ("Data/thetao_Omon_GFDL-CM4_historical_r1i1p1f1_gn_201001-201412.nc")
print (gfdl)
#thetao is temp, lev is depth. top grid cell 0-2m?

gfdl_thetao <- gfdl$var[[1]] 
gfdl_start <- rep (1, gfdl_thetao$ndims)
gfdl_count <- gfdl_thetao$varsize #1440, 1080, 35, 60
gfdl_count[3] <- 1

gfdl_sst <- ncvar_get (gfdl, "thetao", start = rep(1,4), count = gfdl_count)

gfdl_start[3] <- 35
gfdl_bt <- ncvar_get (gfdl, "thetao", start = gfdl_start, count = gfdl_count)

# HadGEM ----
# 3 files files, 1950-1991 and 199
mohc <- nc_open ("Data/thetao_Omon_HadGEM3-GC31-LL_historical_r1i1p1f3_gn_195001-199912.nc")

# IPSL----
# one file, 1950-2014. 780 time steps--I want 324 months total (1985-2012). Start at 432 and should end at 756.
ipsl <- nc_open ("Data/thetao_Omon_IPSL-CM6A-LR_historical_r1i1p1f1_gn_195001-201412.nc")
print (ipsl)

ipsl_thetao <- ipsl$var[[8]] # varsize 362, 332, 75, 780

# need to figure out where bottom temp is. first take full layers for the years that I want, then try to subset. still too big. take 1 year.

# looks like this is like GINS, where numbers drop off at deeper depths. nothing at level 70, but 21204 values at level 74, 42720 at 73.

# maybe try subsetting my region. GINS is lat 50-85, -44.875- 15.125
# can't, it's not a standard grid. 
lat_ipsl <- ncvar_get (ipsl, "nav_lat")
lon_ipsl <- ncvar_get (ipsl, "nav_lon")

# save these so I can delete the netcdf
save (lat_ipsl, file = "Data/ipsl_lat.RData")
save (lon_ipsl, file = "Data/ipsl_lon.RData")


# Start with SST- 1st layer
ipsl_sst <- ncvar_get (ipsl, "thetao", start = c(1,1,1, 432), count = c(362, 332, 1, 324))

# take average across months
#https://stackoverflow.com/questions/43619818/how-can-i-average-a-matrix-every-nth-elements-in-a-vectorized-way
# can't figure this out for 3d, just using a forloop

month_seq <- seq(0, 312, by = 12)

ipsl_sst_mn <- array(dim = c(362, 332, 12))

for (i in 1:12){
  ipsl_month <- ipsl_sst [,,month_seq + i]
  ipsl_sst_mn[,,i] <- apply (ipsl_month, 1:2, mean, na.rm = TRUE)
  
}
save (ipsl_sst_mn, file = "Data/IPSL_sst.RData")

# bottom temp. layer 75 is all NA, so I need to take all the layers or at least some of them. 

# try taking one year at a time and putting together in a list.breaks at i = 19--too big. make 3 ten year ones instead.  
ipsl_bt_array_1 <- array (dim = c(362, 332, 74, 120))
count_bt <- c(362, 332, 74, 12) # will start at 2, and then take 12 time steps

for (i in 1:10){ # 27 total years
  start_bt <- c(1, 1, 2, 432 + 12*(i-1))
  ipsl_bt_tmp <- ncvar_get (ipsl, "thetao", 
                            start = start_bt,
                            count = count_bt)
  ipsl_bt_array_1[ , , , (12*(i-1) + 1):(12*i)] <- ipsl_bt_tmp
}

# too big to save. take mean first
ipsl_bt_mn_1 <- array(dim = c(362, 332, 74, 12))
month_seq <- seq(0, 108, by = 12)

for (i in 1:12){
  ipsl_month <- ipsl_bt_array_1 [,,, month_seq + i]
  ipsl_bt_mn_1[,,,i] <- apply (ipsl_month, 1:3, mean, na.rm = TRUE)
  print (i)
}

# quick plot check
ipsl_rot <- apply (t(ipsl_bt_mn_3[,,50,1]), 2, rev)
ipsl_r_tmp <- raster(ipsl_rot)
plot (ipsl_r_tmp)

save (ipsl_bt_mn_1, file = "Data/IPSL_bt_mn_1.RData")

rm (ipsl_bt_array_1)
rm (ipsl_bt_mn_1)
rm (ipsl_month)

ipsl_bt_array_2 <- array (dim = c(362, 332, 74, 120))

# this one is too big. start at depth 10?
#count_bt <- c(362, 332, 65, 12) # will start at depth 10, and then take 12 time steps
for (i in 1:10){ # 27 total years
  start_bt <- c(1, 1, 2, 552 + 12*(i-1)) # start 120 steps after array_1
  ipsl_bt_tmp <- ncvar_get (ipsl, "thetao", 
                            start = start_bt,
                            count = count_bt)
  ipsl_bt_array_2[ , , , (12*(i-1) + 1):(12*i)] <- ipsl_bt_tmp
  print (552 + 12*(i-1))
}

ipsl_bt_mn_2 <- array(dim = c(362, 332, 74, 12))

for (i in 1:12){
  ipsl_month <- ipsl_bt_array_2 [,,, month_seq + i]
  ipsl_bt_mn_2[,,,i] <- apply (ipsl_month, 1:3, mean, na.rm = TRUE)
  print (i)
}

save (ipsl_bt_mn_2, file = "Data/IPSL_bt_mn_2.RData")

rm (ipsl_bt_array_2)
rm (ipsl_bt_mn_2)
rm (ipsl_bt_tmp)
rm (ipsl_month)

ipsl_bt_array_3 <- array (dim = c(362, 332, 74, 84)) # only 7

for (i in 1:7){ # 27 total years
  start_bt <- c(1, 1, 2, 672 + 12*(i-1))
  ipsl_bt_tmp <- ncvar_get (ipsl, "thetao", 
                            start = start_bt,
                            count = count_bt)
  ipsl_bt_array_3[ , , , (12*(i-1) + 1):(12*i)] <- ipsl_bt_tmp
  print (672 + 12*(i)) # just to double check I land at the right place. want the last one to be 756
}

ipsl_bt_mn_3 <- array(dim = c(362, 332, 74, 12))

month_seq <- seq(0, 72, by = 12)

for (i in 1:12){
  ipsl_month <- ipsl_bt_array_3 [,,, month_seq + i]
  ipsl_bt_mn_3[,,,i] <- apply (ipsl_month, 1:3, mean, na.rm = TRUE)
  print (i)
}

save (ipsl_bt_mn_3, file = "Data/IPSL_bt_mn_3.RData")

rm (ipsl_bt_array_3)
rm (ipsl_bt_mn_3)
rm (ipsl_bt_tmp)
rm (ipsl_month)

# now load decadal means and take an overall mean
load ("Data/IPSL_bt_mn_1.RData")
load ("Data/IPSL_bt_mn_2.RData")
load ("Data/IPSL_bt_mn_3.RData")

ipsl_bt_mn <- array (dim = c (362, 332, 74, 12))

for (i in 1:12){
  ipsl_month <- array (
    c(ipsl_bt_mn_1 [,,,i],
      ipsl_bt_mn_2 [,,,i],
      ipsl_bt_mn_3 [,,,i]
    ),
    dim = c (362, 332, 74, 3)
  )
  ipsl_bt_mn[,,,i] <- apply (ipsl_month, 1:3, mean, na.rm = TRUE)
  print (i)
}

save (ipsl_bt_mn, file = "Data/IPSL_bt_mn.RData")

rm (list = c("ipsl_bt_mn_1", "ipsl_bt_mn_2", "ipsl_bt_mn_3", "ipsl_month"))

## find bottom 
ipsl_bt <- apply (ipsl_bt_mn, c(1,2,4), function(x) x[max(which(!is.na(x)))])
save (ipsl_bt, file = "Data/ipsl_bt.RData")
# plot to check

# can't figure out gridded lat/lon. rasterize??
library (raster)
ipsl_rot <- apply (t(ipsl_sst_mn[,,6]), 2, rev)
ipsl_r_tmp <- raster(ipsl_rot)
plot (ipsl_r_tmp)

ipsl_rot <- apply (t(ipsl_bt_test[,,7]), 2, rev)
ipsl_r_tmp <- raster(ipsl_rot)
plot (ipsl_r_tmp)

gfdl_r <- apply (t(gfdl_bt[,,10]), 2, rev)
gfdl_r_tmp <- raster(gfdl_sst[,,10])
plot (gfdl_r_tmp)
