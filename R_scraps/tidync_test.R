## tidy nc test ##
# 5/18/2019

# https://ropensci.org/blog/2019/11/05/tidync/
library (tidync)
library (dplyr)

## Gfdl----
gfdlfile <- "Data/thetao_Omon_GFDL-CM4_historical_r1i1p1f1_gn_201001-201412.nc"

gfdl <- tidync(gfdlfile)
print (gfdl)

ncmeta::nc_vars (gfdlfile)
ncmeta::nc_var (gfdlfile, "thetao")
ncmeta::nc_atts (gfdlfile)
ncmeta::nc_atts (gfdlfile, "y") %>%
  tidyr::unnest (cols = c (value)) # y is coordinate of projection in degrees

ncmeta::nc_atts (gfdlfile, "lat") %>%
  tidyr::unnest (cols = c (value)) # lat is latitude, degrees_north

# get vector of depth
depths <- gfdl %>%
  activate ("D2") %>%
  hyper_array()
depths$lev

# match to gins depths
load ("Data/gins_depths.RData")
#depths_gins_index <- depths$lev[depths$lev %in% gins_depth]
# seems like I can only do consecutive extracts. but I can subset it afterward.
depths_gins_index <- which (depths$lev %in% gins_depth)

# unpack time
time_ex <- gfdl %>%
  activate ("D1") %>% # ime variable
  hyper_array()
#time_ex$time

tunit <- ncmeta::nc_atts (gfdlfile, "time") %>%
  tidyr::unnest (cols = c (value)) %>%
  dplyr::filter (name == "units")
print (tunit)

time_parts <- RNetCDF::utcal.nc(tunit$value, time_ex$time)
#ISOdatetime(time_parts[,"year"], 
            time_parts[,"month"], 
            time_parts[,"day"], 
            time_parts[,"hour"], 
            time_parts[,"minute"], 
            time_parts[,"second"])

# make index for subsetting time?
time_index <- time_ex$time[ which (between (time_parts[,1], 1985, 2012))]
# interesting..sneaky 2009 december in there...

# read in the temp data. not sure what's going on with lon, so will experiment!
lonrange <- c(-45, 15.25) # this seems to just work!
latrange <- c(50, 85.25)

#lonrange_shift <- lonrange - 120 # this is alaska...

gfdl_slice <- gfdl %>%
  hyper_filter (x = x >= lonrange[1] & x <= lonrange[2],
                y = y >= latrange[1] & y <= latrange[2],
                time = time %in% time_index)

gfdl_slice_data <- gfdl_slice %>% hyper_array()
trans <- attr (gfdl_slice_data, "transforms")

library (maps)
lon <- trans$x %>% dplyr::filter(selected)
lat <- trans$y %>% dplyr::filter(selected)

image(lon$x, lat$y, gfdl_slice_data[[1]][,,24,1])
maps::map("world", add = TRUE)



# what are the deepest levels for my subset here? 32. 
thetao_depths <- apply (gfdl_slice_data$thetao[,,depths_gins_index,1], c(1,2), function (x) max(which(!is.na(x)))) # match gins depths. should be the same across time steps
thetao_depths[which (thetao_depths < 0)] <- NA

gfdl_bt_depth_grid <- apply (thetao_depths, c(1,2), function (x) depths$lev[depths_gins_index[x]]) # match back to actual gins_depth 

# compare with gins----
gins <- tidync("../Documents/MATLAB/GINS/gins_8594_t13_04.nc")
print (gins)

depths_gins <- gins %>%
  activate ("D3") %>%
  hyper_array()
depths_gins$depth

t_an <- gins %>%
  activate ("t_an") %>%
  hyper_array()

gins_depth_grid <- apply (t_an$t_an, c(1,2), function (x) gins_depth [max(which(!is.na(x)))])
gins_depth_grid[which (gins_depth_grid < 0)] <- NA

gins_depth_grid[1:10, 1:10]

# how does this match up to gins one
trans_gins <- attr (t_an, "transforms")

lon_gins <- attr (t_an, "transforms")$lon 
lat_gins <- attr (t_an, "transforms")$lat

#gfdl has twice as many lats? the summary is pretty similar, but hard to say how close they are. would need to interpolate?
summary (array(gins_depth_grid))
summary (array (gfdl_bt_depth_grid))


## CNRM----
cnrmfile <- "Data/thetao_Omon_CNRM-CM6-1-HR_historical_r1i1p1f2_gn_198501-198912.nc"
cnrm <- tidync (cnrmfile)
print (cnrm) # y (D0) and x (D1) dimensions are just indices. but i know the values are in there from ncvar_Get!!?

ncmeta::nc_vars (cnrmfile)
ncmeta::nc_var(cnrmfile, "lat") # 2x15 tible, 
ncmeta::nc_dim(cnrmfile, "y") # error, invalid dimension ID or name for "lat", "D0"

ncmeta::nc_grids(cnrmfile) %>% tidyr::unnest(cols = c(variables))
# some combination of D1 [lon], D0 [lat] is lat and lon. but how do i extract them? thetao is d1, d0, d3 (lev), d5 (time)

ncmeta::nc_atts (cnrmfile, "lat") %>% tidyr::unnest(cols = c(value)) # this gives me the long names and that units are degrees_north

# lat_cnrm_activate <- cnrm %>%
#   activate ("lat") %>%
#   hyper_array()
# too big, and takes 4 dimension

# figure out time. D5 for CNRM
time_ex <- cnrm %>%
  activate ("D5") %>% # time variable
  hyper_array()
head(time_ex$time)

tunit <- ncmeta::nc_atts (cnrmfile, "time") %>%
  tidyr::unnest (cols = c (value)) %>%
  dplyr::filter (name == "units")

time_parts <- RNetCDF::utcal.nc(tunit$value, time_ex$time)

# subset years between 1985 and 2012, inclusive, and choose which month I want
time_subset <- time_ex$time[which (between (time_parts[,1], 1985, 2012))] # can't take just months I want, have to be continuous for filtering

# subset lat and lon
lonrange <- c(-45, 15.25) 
latrange <- c(50, 85.25)

# what is lat and lon?? they're just indices??

# suggested solution from tutorial
full_expand <- function(x, ...) {
ad <- active(x)
spl <- strsplit(ad, ",")[[1L]]
out <- hyper_tibble(x)

for (i in seq_along(spl)) {
  out <- dplyr::inner_join(out, activate(x, spl[i]) %>% hyper_tibble())
} 
out
}

full_expand(tidync(cnrmfile)) # too big with the full thing. 

# try with one time and depth slice?
cnrm_tiny_slice <- cnrm %>%
  hyper_filter (time = time ==49323.5,
                lev = lev <= 1) # first one is 0.5

full_expand (cnrm_tiny_slice) # error: activated grids by name or number

out <- hyper_tibble(cnrm_tiny_slice)
out <- dplyr::inner_join(out, activate(cnrm_tiny_slice, "D1,D0") %>% hyper_tibble()) # this works! lon is -180=180

# figure out which x and y indices correspond to lat and lon??

out %>%
  #filter (between (lat, -50, 85.25)) %>%
  filter (between (lat, -50, -49)) %>%
  select (y) %>%
  View() # different values...

lat_cnrm_activate <- cnrm_tiny_slice %>%
  activate ("D1,D0") %>%
  hyper_array() 


# plot check. lon, then lat
cnrm_slice_data <- cnrm_tiny_slice %>% hyper_array()
trans <- attr (cnrm_slice_data, "transforms")
image(trans$x$x, trans$y$y, cnrm_slice_data[[1]])

image(out$lon, out$lat, cnrm_slice_data[[1]])
maps::map("world", add = TRUE)

cnrm_subset <- lat_cnrm_activate$lat
summary (array(lat_cnrm_activate[[1]]))


cnrm_slice_sst <- cnrm %>%
  hyper_filter (
    # lon = lon >= lonrange[1] & lon <= lonrange[2],
    #             lat = lat >= latrange[1] & y <= latrange[2],
                time = time %in% time_subset,
                lev = lev == 1)

cnrm_slice_data <- cnrm_slice_sst %>%
  activate ("D1,D0") %>%
  hyper_array() %>%
  hyper_array()
trans <- attr (cnrm_slice_data, "transforms")

lon <- trans$x %>% dplyr::filter(selected)
lat <- trans$y %>% dplyr::filter(selected)

## well, what's the deal with awi----
awifile <- "Data/thetao_Omon_AWI-CM-1-1-MR_historical_r1i1p1f1_gn_201101-201412.nc"
awi <- tidync(awifile)
print (awi) # lat and lon is just ncells 1:830305
ncmeta::nc_vars(awifile) # has separate lat and lon, both as ndims 1

ncmeta::nc_var (awifile, "thetao") # 3dims

## gins test----
gins_data <- tidync(gins_files[1]) %>% hyper_array()
op <- par(mfrow = c(1,2))

for (i in 1:2) {
  image(gins_data[[i]][,,1], main = names(gins_data)[i])
  maps::map("world", add = TRUE)
}


