## Process GINS data and find bottom
# 5/15/2020
# JGM

library (ncdf4)

# I made this hard for myself because it sets .nc.mat
gins_files_all <- list.files (path = "../../MATLAB/GINS", pattern = ".nc", full.names = T)
gins_files_mat <- list.files (path = "../../MATLAB/GINS", pattern = ".mat", full.names = T)
gins_files <- setdiff (gins_files_all, gins_files_mat)

gins_tmp <- nc_open(gins_files[1])
print (gins_tmp)
gins_tan <- ncvar_get (gins_tmp, "t_an")

# I want to grab the winter, spring, summer, fall and average them. Then for each season I want to find the actual bottom temp. file structure is t13 = winter, t14 = spring, 15 is summer, 16 is fall. 

gins_w_files <- gins_files[which (grepl(13, gins_files) == TRUE)]
gins_sp_files <- gins_files[which (grepl(14, gins_files) == TRUE)]
gins_su_files <- gins_files[which (grepl(15, gins_files) == TRUE)]
gins_f_files <- gins_files[which (grepl(16, gins_files) == TRUE)]
gins_files_season <- list ("Winter" = gins_w_files, 
                           "Spring" = gins_sp_files, 
                           "Summer" = gins_su_files,
                           "Fall" = gins_f_files)

#https://stackoverflow.com/questions/39934159/get-value-of-last-non-na-row-per-column-in-data-table
# use <<- to have something show up in global environment
find_bottom <- function (filenames){
  gins <- lapply (filenames, nc_open)
  gins_t <- lapply (gins, function (x) ncvar_get(x, "t_an"))
  gins_arr <- array (unlist (gins_t), c (241, 141, 102, 3))
  gins_mn <- apply (gins_arr, 1:3, mean, na.rm = TRUE)
  gins_bottom <- apply (gins_mn, c(1,2), function(x) x[max(which(!is.na(x)))])
}

gins_bt <- lapply (gins_files_season, find_bottom) # labeled list with bottom temperature for each!

# plot to check
lat <- ncvar_get (gins_tmp, "lat")
lon <- ncvar_get (gins_tmp, "lon")

image (lon, lat, gins_bt$Summer)

save (gins_bt, file = "Data/GINS_bottom_temp.RData")
