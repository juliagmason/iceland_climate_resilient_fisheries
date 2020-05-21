## Load CMIP6 3D ocean potential temp NetCDFs and extract surface and bottom temperature
# 5/15/2020
# JGM


# The levels are not constant across models. For comparing to GINS, I should try to match as closely as possible. But for running the actual models with the survey data, should I just use the model data as is?


library(ncdf4)
library (raster)

# CNRM---
# take SST for now to compare with GFDL and AWI
cnrm_nc_files <- list.files (path = "Data", pattern = "Omon_CNRM", full.names = TRUE)

cnrm <- nc_open (cnrm_nc_files[1])
print (cnrm)

lat_cnrm <- ncvar_get (cnrm, "lat") # these are normal?
lon_cnrm <- ncvar_get (cnrm, "lon") # -180-180

# check depths
depth_cnrm <- ncvar_get (cnrm, "lev") # 75 levels, 0.5 to 5902. 

nc_close (cnrm)

# save these so I can delete the netcdf
save (lat_cnrm, file = "Data/cnrm_lat.RData")
save (lon_cnrm, file = "Data/cnrm_lon.RData")

# grab sst, use tidync later
cnrm_sst_mn_fun <- function (filename){
  
  cnrm <- nc_open (filename)
  
  # first file starts at 1985! so just need to clip the last one. 2010-2014, so need 36
  if (grepl ("2014", filename)) {
    start <- rep (1, 4)
    count <- c(cnrm$var[[7]]$varsize[1], 
               cnrm$var[[7]]$varsize[2],
               1, 36)
  } else {
    start <- rep (1, 4)
    count <- c(cnrm$var[[7]]$varsize[1],
               cnrm$var[[7]]$varsize[2], 
               1, cnrm$var[[7]]$varsize[4])
  }
  
  cnrm_sst_tmp <- ncvar_get (cnrm, "thetao", start = start, count = count)
  
  # change into a 4d array with months as 3rd dimension and years as 4th dimension
  cnrm_sst_mn_array <- array (cnrm_sst_tmp, 
                              dim = c(cnrm$var[[7]]$varsize[1], 
                                      cnrm$var[[7]]$varsize[2],
                                      12,
                                      count[4]/12))
  
  # take mean across 4th dimension
  cnrm_sst_mn <- apply (cnrm_sst_mn_array, c(1:3), mean)
  
  # clean up
  rm (list = c ("cnrm_sst_tmp", "cnrm_sst_mn_array"))
  nc_close (cnrm)
  
  return (cnrm_sst_mn)
}

# apply function across files in list
cnrm_sst_l <- lapply (cnrm_nc_files, cnrm_sst_mn_fun)

# put back into an array and take a mean of means for each month
cnrm_sst_array <- array (unlist (cnrm_sst_l), 
                         dim = c (cnrm$var[[7]]$varsize[1],
                                  cnrm$var[[7]]$varsize[2],
                                  12, 
                                  length (cnrm_nc_files)))

cnrm_sst_mn <- apply (cnrm_sst_array, 1:3, mean, na.rm = TRUE)

save (cnrm_sst_mn, file = "Data/cnrm_sst.RData")

ipsl_rot <- apply (t(cnrm_bt_mn[,,7]), 2, rev)
ipsl_r_tmp <- raster(ipsl_rot)
plot (ipsl_r_tmp)

# bottom temp. can't really do tidync with cnrm because the lat/lon are indices, so can't filter. default 60 per file
cnrm_bt_mn_fun <- function (filename){
  print (filename)
  cnrm <- nc_open (filename)
  
  # define start val and n_months within function? first file starts in 1985, so all I need is the last one. start val will be all the same. 
  n_months <- ifelse (grepl ("2014", filename), 36, 60) 
  
  start_bt <- c(1, 1, 2, 1)
  
  count_bt <- c(cnrm$var[[7]]$varsize[1],
                cnrm$var[[7]]$varsize[2],
                cnrm$var[[7]]$varsize[3] -1, # all levels but 1st
                1) # just one month
  
  # array to hold the bottom temp values for each month
  cnrm_bt_array_tmp <- array (dim = c(
    cnrm$var[[7]]$varsize[1], 
    cnrm$var[[7]]$varsize[2],
    n_months)
  )
  
  for (i in 1:n_months){ # number of months depends on file
    
    cnrm_bt_tmp <- ncvar_get (cnrm, "thetao", 
                              start = start_bt,
                              count = count_bt)
    cnrm_floor_tmp <- apply (cnrm_bt_tmp, c(1,2), function(x) x[max(which(!is.na(x)))]) # take deepest non-NA number in the 3rd dimension
    cnrm_bt_array_tmp[ , , i] <- cnrm_floor_tmp
    rm (cnrm_bt_tmp) # save some memory?
    print (i)
  }
  
  # change into a 4d array with months as 3rd dimension and years as 4th dimension
  cnrm_bt_mn_array <- array (cnrm_bt_array_tmp, 
                             dim = c(cnrm$var[[7]]$varsize[1], 
                                     cnrm$var[[7]]$varsize[2],
                                     12,
                                     n_months/12))
  
  # take mean across 4th dimension
  cnrm_bt_mn <- apply (cnrm_bt_mn_array, c(1:3), mean)
  
  # clean up
  rm (list = c("cnrm_bt_array_tmp", "cnrm_bt_mn_array"))
  nc_close (cnrm)
  
  return (cnrm_bt_mn)
  
}

cnrm_bt_list <- lapply (cnrm_nc_files, cnrm_bt_mn_fun)

#instead, make array and do it in a for loop
# cnrm_bt_array <- array (dim = c (1442, 1050, 12, length (cnrm_nc_files))) # 12 months, 6 files
# 
# for (i in 1:length (cnrm_nc_files)){
#   start_val <-  cnrm_start_vals[i]
#   n_months <- cnrm_n_months[i]
#   cnrm_bt_array [,,,i] <- cnrm_bt_mn_fun (filename =cnrm_nc_files[i],
#                                           n_months = n_months, 
#                                           start_val = start_val)
# }

cnrm_bt_array <- array (unlist (cnrm_bt_list), dim = c (1442, 1050, 12, 6))
cnrm_bt_mn <- apply (cnrm_bt_array, 1:3, mean, na.rm = TRUE)
save (cnrm_bt_mn, file = "Data/cnrm_bt.RData")

# GFDL---
# 3 files, 1970-1989 [start at 181st time step for 1985, take 60 total], 1990-2009, 2010-2014 [end at 36th time step for 2012]
gfdl_nc_files <- list.files (path = "Data", pattern = "Omon_GFDL", full.names = TRUE)

# view details and save lat & lon for first one
gfdl <- nc_open (gfdl_nc_files[1])
print (gfdl)

lat_gfdl <- ncvar_get (gfdl, "lat")
lon_gfdl <- ncvar_get (gfdl, "lon")

# save these so I can delete the netcdf
save (lat_gfdl, file = "Data/gfdl_lat.RData")
save (lon_gfdl, file = "Data/gfdl_lon.RData")

# check depths
depth_gfdl <- ncvar_get (gfdl, "lev") # 35 levels, 2.5 to 6500. 

nc_close (gfdl)

# SST: for each file, grab first depth level (sst) for the years/months I want, then take monthly average. 

gfdl_sst_mn <- function (filename){
  
  gfdl <- nc_open (filename)
  
  # start at 181 for first file
  if (grepl ("1970", filename)) {
    start <- c(1, 1, 1, 181)
    count <- c(gfdl$var[[1]]$varsize[1], 
               gfdl$var[[1]]$varsize[2],
               1, 60)
  } else if (grepl ("2010", filename)){
    start <- rep (1, 4)
    count <- c(gfdl$var[[1]]$varsize[1], 
               gfdl$var[[1]]$varsize[2], 
               1, 36)
  } else {
    start <- rep (1, 4)
    count <- c(gfdl$var[[1]]$varsize[1],
               gfdl$var[[1]]$varsize[2], 
               1, gfdl$var[[1]]$varsize[4])
  }
  
  gfdl_sst_tmp <- ncvar_get (gfdl, "thetao", start = start, count = count)
  
  # change into a 4d array with months as 3rd dimension and years as 4th dimension
  gfdl_sst_mn_array <- array (gfdl_sst_tmp, 
                             dim = c(gfdl$var[[1]]$varsize[1], 
                                     gfdl$var[[1]]$varsize[2],
                                     12,
                                     count[4]/12))
  
  # take mean across 4th dimension
  gfdl_sst_mn <- apply (gfdl_sst_mn_array, c(1:3), mean)
  
  # clean up
  rm (list = c ("gfdl_sst_tmp", "gfdl_sst_mn_array"))
  nc_close (gfdl)
  
  return (gfdl_sst_mn)
}

# apply function across files in list
gfdl_sst_l <- lapply (gfdl_nc_files, gfdl_sst_mn)

# put back into an array and take a mean of means for each month
gfdl_sst_array <- array (unlist (gfdl_sst_l), 
                         dim = c (1440, 1080, 12, 3))

gfdl_sst_mn <- apply (gfdl_sst_array, 1:3, mean, na.rm = TRUE)

save (gfdl_sst_mn, file = "Data/gfdl_sst.RData")

# bottom temp: take layers 2-35 and find deepest level 


# make a function that takes one month at a time, finds bottom, puts months together, and finds a monthly mean for each file. Will have to specify a different starting point and number of months for each file. 


gfdl_bt_mn_fun <- function (filename, n_months, start_val){
  
  gfdl <- nc_open (filename)
  
  count_bt <- c(gfdl$var[[1]]$varsize[1],
                gfdl$var[[1]]$varsize[2],
                gfdl$var[[1]]$varsize[3] -1, # all levels but 1st
                1) # just one month
  
  # array to hold the bottom temp values for each month
  gfdl_bt_array_tmp <- array (dim = c(
    gfdl$var[[1]]$varsize[1], 
    gfdl$var[[1]]$varsize[2],
    n_months)
  )
  
  for (i in 1:n_months){ # number of months depends on file
    start_bt <- c(1, 1, 2, start_val + i)
    gfdl_bt_tmp <- ncvar_get (gfdl, "thetao", 
                              start = start_bt,
                              count = count_bt)
    gfdl_floor_tmp <- apply (gfdl_bt_tmp, c(1,2), function(x) x[max(which(!is.na(x)))]) # take deepest non-NA number in the 3rd dimension
    gfdl_bt_array_tmp[ , , i] <- gfdl_floor_tmp
    rm (gfdl_bt_tmp) # save some memory?
    print (i)
  }
  
  # change into a 4d array with months as 3rd dimension and years as 4th dimension
  gfdl_bt_mn_array <- array (gfdl_bt_array_tmp, 
                             dim = c(gfdl$var[[1]]$varsize[1], 
                                     gfdl$var[[1]]$varsize[2],
                                     12,
                                     n_months/12))
  
  # take mean across 4th dimension
  gfdl_bt_mn <- apply (gfdl_bt_mn_array, c(1:3), mean)
  
  # clean up
  rm (list = c("gfdl_bt_array_tmp", "gfdl_bt_mn_array"))
  nc_close (gfdl)
  
  return (gfdl_bt_mn)
  
}

  
# can I just make a vector of start values and n_months? star val is 180, 0, 0; n_months is 60, 240, 36
gfdl_start_vals <- c(180, 0, 0)
gfdl_n_months <- c(60, 240, 36)

# gfdl_bt <- lapply (gfdl_nc_files, gfdl_bt_mn_fun, n_months = gfdl_n_months, start_val = gfdl_start_vals) # cannot allocate vector of size 3003.4Gb

#instead, make array and do it in a for loop
gfdl_bt_array <- array (dim = c (1440, 1080, 12, 3)) # 12 months, 3 files

for (i in 1:length (gfdl_nc_files)){
  start_val <-  gfdl_start_vals[i]
  n_months <- gfdl_n_months[i]
  gfdl_bt_array [,,,i] <- gfdl_bt_mn_fun (filename =gfdl_nc_files[i],
                                          n_months = n_months, 
                                          start_val = start_val)
}


gfdl_bt_mn <- apply (gfdl_bt_array, 1:3, mean, na.rm = TRUE)
save (gfdl_bt_mn, file = "Data/gfdl_bt.RData")

# AWI ----
# 4 files, 1981-1990 [start at 49, 72 steps], 1991-2000, 2000-2009, 2010-2014 [36 time steps]. 
# this one will be different--1d lat/lon

awi_nc_files <- list.files (path = "Data", pattern = "Omon_AWI", full.names = TRUE)

awi <- nc_open (awi_nc_files[1])
print (awi) # thetao is 830305 x 46 depth x 120 time

awi_thetao <- awi$var[[2]]

lat_awi <- ncvar_get (awi, "lat")
lon_awi <- ncvar_get (awi, "lon")

# not sure these will actually be useful? 830305x1
save (lat_awi, file = "Data/awi_lat.RData")
save (lon_awi, file = "Data/awi_lon.RData")

depth_awi <- ncvar_get (awi, "depth")

# SST - first level
awi_sst_mn_fun <- function (filename){
  
  awi <- nc_open (filename)
  
  # 4 files, 1981-1990 [start at 49, 72 steps], 1991-2000, 2001-2010, 2011-2014 [24 time steps].
  if (grepl ("1981", filename)) {
    start <- c(1, 1, 49)
    count <- c(awi$var[[2]]$varsize[1], 1, 72)
  } else if (grepl ("2010", filename)){
    start <- rep (1, 3)
    count <- c(awi$var[[2]]$varsize[1], 1, 24)
  } else {
    start <- rep (1, 3)
    count <- c(awi$var[[2]]$varsize[1],
               1, awi$var[[2]]$varsize[3])
  }
  
  awi_sst_tmp <- ncvar_get (awi, "thetao", start = start, count = count) # 830305 x n time steps
  
  # reshape to 3d array with coords as rows, months as columns, floors as years. 
  awi_sst_mn_array <- array (awi_sst_tmp, 
                            dim = c (830305, 12, dim (awi_sst_tmp)[2]/12))
  
  # take mean over floors
  awi_sst_mn <- apply (awi_sst_mn_array, c(1, 2), mean)
  
  # clean up
  nc_close (awi)
  rm (list = c("awi_sst_tmp", "awi_sst_mn_array"))
  
  # spit out monthly means for that file
  return (awi_sst_mn)
}

# attempt to plot?
#awi_grid <- expand.grid (lon = lon_awi, lat = lat_awi) # Error: cannot allocate vector of size 2568.2 Gb

# apply function across files in list
awi_sst_l <- lapply (awi_nc_files, awi_sst_mn_fun)

# put back into an array and take a mean of means for each month
awi_sst_array <- array (unlist (awi_sst_l), 
                         dim = c (830305, 12, length (awi_nc_files)))

awi_sst_mn <- apply (awi_sst_array, 1:2, mean, na.rm = TRUE)

save (awi_sst_mn, file = "Data/awi_sst.RData")

rm (list = c ("awi_sst_l", "awi_sst_array"))

# bottom temp, 46 total layers

awi_bt_mn_fun <- function (filename, n_months, start_val){
  
  awi <- nc_open (filename)
  
  count_bt <- c(awi$var[[2]]$varsize[1],
                awi$var[[2]]$varsize[2] -1, # all levels but 1st
                1) # just one month
  
  # array to hold the bottom temp values for each month
  awi_bt_array_tmp <- array (dim = c(awi$var[[2]]$varsize[1], n_months)) # 830305 x 72
  
  for (i in 1:n_months){ # number of months depends on file
    start_bt <- c(1, 2, start_val + i)
    awi_bt_tmp <- ncvar_get (awi, "thetao", 
                              start = start_bt,
                              count = count_bt) # 830305 x 45
    awi_floor_tmp <- apply (awi_bt_tmp, 1, function(x) x[max(which(!is.na(x)))]) # numeric vector 1x830305
    awi_bt_array_tmp[ , i] <- awi_floor_tmp
    rm (awi_bt_tmp) # save some memory?
    print (i)
  }
  
  # take mean across months. can just do by turning into 3d array
  awi_bt_mn_array <- array (awi_bt_array_tmp, 
                            dim = c (830305, 12, n_months/12))
  
  awi_bt_mn <- apply (awi_bt_mn_array, c(1, 2), mean)
  rm (list = c("awi_bt_array_tmp", "awi_bt_mn_array"))
  nc_close (awi)
  
  return (awi_bt_mn) # should be 830305 x 12
  
}

# Make a vector of start values and n_months for the 4 files. start val would be 48, 0, 0, 0; n_months is 72, 120, 120, 24
awi_start_vals <- c(48, 0, 0, 0)
awi_n_months <- c(72, 120, 120, 24)

# awi_bt <- lapply (awi_nc_files, awi_bt_mn_fun, n_months = awi_n_months, start_val = awi_start_vals) # cannot allocate vector of size 3003.4Gb

#instead, make array and do it in a for loop
awi_bt_array <- array (dim = c (830305, 12, length (awi_nc_files))) # 12 months, 4 files

for (i in 1:length (awi_nc_files)){
  start_val <-  awi_start_vals[i]
  n_months <- awi_n_months[i]
  awi_bt_array [,,i] <- awi_bt_mn_fun (filename =awi_nc_files[i],
                                          n_months = n_months, 
                                          start_val = start_val)
}



awi_bt_mn <- apply (awi_bt_array, 1:2, mean, na.rm = TRUE)
save (awi_bt_mn, file = "Data/awi_bt.RData")

# HadGEM ----
# 2 files, 1950-1999 (600 time steps) and 2000-2014 (180 time steps)
mohc_1 <- nc_open ("Data/thetao_Omon_HadGEM3-GC31-LL_historical_r1i1p1f3_gn_195001-199912.nc")
print (mohc_1)
mohc_thetao <- mohc_1$var[[7]] # 360 x 330 x 75 x 600

# save lat and lon
# lat_mohc <- ncvar_get (mohc_1, "latitude")
# lon_mohc <- ncvar_get (mohc_1, "longitude")
# 
# save (lat_mohc, file = "Data/mohc_lat.RData")
# save (lon_mohc, file = "Data/mohc_lon.RData")

depth_mohc <- ncvar_get (mohc_2, "lev")

mohc_2 <- nc_open ("Data/thetao_Omon_HadGEM3-GC31-LL_historical_r1i1p1f3_gn_200001-201412.nc")
# thetao is 360-330-75x180


# start with SST
# for first file to get to 1985, start would be 421
mohc_sst <- array (dim = c(360, 330, 336))
mohc_sst[,,1:180] <- ncvar_get (mohc_1, "thetao", start = c(1,1,1, 421), count = c(360, 330, 1, 180)) # 360x330x180
mohc_sst[,,181:336] <- ncvar_get (mohc_2, "thetao", start = c(1,1,1,1), count = c(360, 330, 1, 156)) # 360x330x156

# change into a 4d array with months as 3rd dimension and years as 4th dimension
mohc_sst_array <- array (mohc_sst, 
                            dim = c(mohc_1$var[[7]]$varsize[1], 
                                    mohc_1$var[[7]]$varsize[2],
                                    12, 28)) # 12 months, 28 years

# take mean across 4th dimension
mohc_sst_mn <- apply (mohc_sst_array, c(1:3), mean)
save (mohc_sst_mn, file = "Data/mohc_sst.RData")

# clean up
rm (list = c ("mohc_sst", "mohc_sst_array"))

# bottom temp. make a function like gfdl to go through both files
mohc_bt_mn_fun <- function (filename, n_months, start_val){
  
  mohc <- nc_open (filename)
  
  count_bt <- c(mohc$var[[7]]$varsize[1],
                mohc$var[[7]]$varsize[2],
                mohc$var[[7]]$varsize[3] -1, # all levels but 1st
                1) # just one month
  
  # array to hold the bottom temp values for each month
  mohc_bt_array_tmp <- array (dim = c(
    mohc$var[[7]]$varsize[1], 
    mohc$var[[7]]$varsize[2],
    n_months)
  )
  
  for (i in 1:n_months){ # number of months depends on file
    start_bt <- c(1, 1, 2, start_val + i)
    mohc_bt_tmp <- ncvar_get (mohc, "thetao", 
                              start = start_bt,
                              count = count_bt)
    mohc_floor_tmp <- apply (mohc_bt_tmp, c(1,2), function(x) x[max(which(!is.na(x)))]) # take deepest non-NA number in the 3rd dimension
    mohc_bt_array_tmp[ , , i] <- mohc_floor_tmp
    rm (mohc_bt_tmp) # save some memory?
    print (i)
  }
  
  # change into a 4d array with months as 3rd dimension and years as 4th dimension
  mohc_bt_mn_array <- array (mohc_bt_array_tmp, 
                             dim = c(mohc$var[[7]]$varsize[1], 
                                     mohc$var[[7]]$varsize[2],
                                     12,
                                     n_months/12))
  
  # take mean across 4th dimension
  mohc_bt_mn <- apply (mohc_bt_mn_array, c(1:3), mean)
  
  # clean up
  rm (list = c("mohc_bt_array_tmp", "mohc_bt_mn_array"))
  nc_close (mohc)
  
  return (mohc_bt_mn)
  
}


# apply across files. first start val is 420( starts at val + i), counts are 180 and 156

mohc_nc_files <- list.files (path = "Data", pattern = "Omon_Had", full.names = TRUE)
mohc_start_vals <- c(420, 0)
mohc_n_months <- c(180, 156)


#lapply has been  too big, so instead, make array and do it in a for loop
mohc_bt_array <- array (dim = c (360, 330, 12, 2)) # 12 months, 3 files

for (i in 1:length (mohc_nc_files)){
  start_val <-  mohc_start_vals[i]
  n_months <- mohc_n_months[i]
  mohc_bt_array [,,,i] <- mohc_bt_mn_fun (filename =mohc_nc_files[i],
                                          n_months = n_months, 
                                          start_val = start_val)
}


mohc_bt_mn <- apply (mohc_bt_array, 1:3, mean, na.rm = TRUE)
save (mohc_bt_mn, file = "Data/mohc_bt.RData")


# quick plot check
mohc_rot <- apply (t(mohc_bt_mn[,,7]), 2, rev)
mohc_r_tmp <- raster(mohc_rot)
plot (mohc_r_tmp)


nc_close (mohc_1)
nc_close (mohc_2)
rm (list = c ("mohc_bt_array", "mohc_1", "mohc_2", "mohc_r_tmp", "mohc_rot", "mohc_thetao"))

# IPSL----
# one file, 1950-2014. 780 time steps--I want 336 months total (1985-2012). Start at 421 and should end at 756.
ipsl <- nc_open ("Data/thetao_Omon_IPSL-CM6A-LR_historical_r1i1p1f1_gn_195001-201412.nc")
#print (ipsl)

#ipsl_thetao <- ipsl$var[[8]] # varsize 362, 332, 75, 780
 
# lat_ipsl <- ncvar_get (ipsl, "nav_lat")
# lon_ipsl <- ncvar_get (ipsl, "nav_lon")
# 
# # save these so I can delete the netcdf
# save (lat_ipsl, file = "Data/ipsl_lat.RData")
# save (lon_ipsl, file = "Data/ipsl_lon.RData")

depth_ipsl <- ncvar_get (ipsl, "olevel")
# Start with SST- 1st layer
ipsl_sst <- ncvar_get (ipsl, "thetao", start = c(1,1,1, 421), count = c(362, 332, 1, 336)) # 362 x 332 x 336


# change into a 4d array with months as 3rd dimension and years as 4th dimension
ipsl_sst_array <- array (ipsl_sst, 
                         dim = c(ipsl$var[[8]]$varsize[1], 
                                 ipsl$var[[8]]$varsize[2],
                                 12, 28)) # 12 months, 28 years

# take mean across 4th dimension
ipsl_sst_mn <- apply (ipsl_sst_array, c(1:3), mean)

save (ipsl_sst_mn, file = "Data/ipsl_sst.RData")

rm (list = c ("ipsl_sst", "ipsl_sst_array"))

# bottom temp. layer 75 is all NA, so I need to take all the layers or at least some of them to find the seafloor. Looks from a quick check of the first 10 years that the # of NAs is consistent across the files, so I should be able to take the bottom layer first, and then take a mean. 

# try taking one month at a time, finding bottom, and putting together in a list
ipsl_bt_array <- array (dim = c(362, 332, 336))
count_bt <- c(362, 332, 74, 1) # will start at depth 2, take one month at a time

for (i in 1:336){ 
  start_bt <- c(1, 1, 2, 420 + i)
  ipsl_bt_tmp <- ncvar_get (ipsl, "thetao", 
                            start = start_bt,
                            count = count_bt)
  ipsl_floor_tmp <- apply (ipsl_bt_tmp, c(1,2), function(x) x[max(which(!is.na(x)))]) # take deepest non-NA number in the 3rd dimension
  ipsl_bt_array[ , , i] <- ipsl_floor_tmp
  rm (ipsl_bt_tmp) # save some memory?
  print (i)
}

# take mean across months
# change into a 4d array with months as 3rd dimension and years as 4th dimension
ipsl_bt_mn_array <- array (ipsl_bt_array, 
                           dim = c(ipsl$var[[8]]$varsize[1], 
                                   ipsl$var[[8]]$varsize[2],
                                   12,
                                   28))

# take mean across 4th dimension
ipsl_bt_mn <- apply (ipsl_bt_mn_array, c(1:3), mean)

save (ipsl_bt_mn, file = "Data/ipsl_bt.RData")

nc_close(ipsl)

# plot to check

# can't figure out gridded lat/lon. rasterize??


ipsl_rot <- apply (t(ipsl_bt_mn[,,1]), 2, rev)
ipsl_r_tmp <- raster(ipsl_rot)
plot (ipsl_r_tmp)

gfdl_r <- apply (t(gfdl_bt[,,10]), 2, rev)
gfdl_r_tmp <- raster(gfdl_sst[,,10])
plot (gfdl_r_tmp)
