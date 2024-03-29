## Build predictor variable df
# 6/26/2020
# JGM

# Build dataframe for attaching environmental predictor variables to survey lat lon

# pairing rasters to points: https://stackoverflow.com/questions/34294376/how-to-extract-data-from-a-rasterbrick

library (tidyverse)
library (raster)
library (lubridate)

# csv of MFRI tow information ----
# Created in Data_setup/Write_combined_MFRI_survey_csvs.R. Each row is a sample/tow, with information about depth, temp, date, tow. Species information has been removed. Goes through March 2020. 
mfri_samples <- read_csv ("Data/MFRI_comb_survey_samples.csv",
                          col_types = cols(
                            sample_id = col_factor(),
                            stat_sq = col_factor())) %>%
  # fix a few stat_sq typos with hyphens
  mutate (stat_sq = gsub ("-", "", stat_sq))

# make spatial points df for extracting environmental data
mfri_pts <- mfri_samples %>%
  dplyr::select (sample_id, date, lat, lon) %>%
  arrange (date)  #not originally in date order

coordinates(mfri_pts) <- ~lon + lat

# vector of unique dates to subset environmental brick layers
mfri_dates <- unique (mfri_pts$date)

# GINS salinity ----
# Salinity climatology from NOAA: https://www.nodc.noaa.gov/OC5/regional_climate/gin-seas-climate/about_gin.html
# this is not the right salinity data to be using since it's averaged over decades. I used it in my first model but shouldn't use for anything else. It only goes until 2012. 
# adding back in 9/22/2020 to document variable importance
gins_sal <- brick ("Data/GINS_bottom_sal.grd")

mfri_dates_gins <- unique (mfri_pts$date[which (mfri_pts$date < "2013-01-01")]) # 106 total dates; 73 before 2012

gins_dates <- paste (
  rep (c(1985, 1995, 2005), each = 12),
  sprintf ("%02d", 1:12),
  "01",
  sep = "-")

mfri_sal_df <- data.frame(sample_id = factor(),
                          gins_sal = numeric())


for (i in 1:length(mfri_dates_gins)) {
  x <- mfri_dates_gins[i]
  # subset mfri data for that date
  mfri <- mfri_pts[which (mfri_pts$date == x),]
  
  # match date by re-assigning year to appropriate decade
  y <- case_when (
    between (year(x), 1985, 1994) ~ paste0 ("1985", substr(x, 5, 10)),
    between (year(x), 1995, 2004) ~ paste0 ("1995", substr(x, 5, 10)),
    between (year(x), 2005, 2012) ~ paste0 ("2005", substr(x, 5, 10))
  )
  
  #index date
  y_in <- which (gins_dates == y)
  
  # subset salinity brick for that date and extract data for mfri lat/lon points
  gins_subset <- subset (gins_sal, y_in)
  gins_extract <- raster::extract (gins_subset, mfri, method = "simple")
  
  x_df <- data.frame (sample_id = mfri$sample_id, gins_sal = gins_extract)
  
  mfri_sal_df <- rbind (mfri_sal_df, x_df)
}

mfri_sal_df$sample_id <- as.factor(mfri_sal_df$sample_id)

# Habitat data from Bormicon regions ----
# 8/20/2020 update: 
# From Pamela: # I attached a table you can join with stat_sq column to get at this. We generally use 'Bormicon' regions, which were defined a while ago based on depth and cluster analyses (see link below), but not so much habitat data if I remember correctly (double check this!). That means they may be a bit y ~ x ~ y situation because I believe multi-species survey data were used to define them. The true 'habitat' studies taking place here (using side sonar and such) don't nearly cover enough area to be useful yet.
# https://www.hafogvatn.is/static/research/files/fjolrit-119.pdf  see page 330 - division and subdivision numbers correspond with attached table

# update 8/26/2020:
# From Pamela: We use the Gadget subdivisions but just call them 'Bormicon' because that is where the original justifications for the patterns came from (see figs. 9.1 vs. 9.4 in the document link)... they were just modified to be formed from statistical squares to make data analysis and subsampling easier (and that's why we still use the 'Gadget' rather than the original Bormicon regions). Yes, that stat_sq column has a lot of other 'junk' numbers that are for other parts of the database. But double-checking this made me realize that there is an error in the stations table that may have made it confusing - the 'areacell' column is the same as 'stat_sq', so I just remade the stations table 'tow_table.csv' with that column name corrected and the subdivision and division columns added. 

# I renamed to bormicon_table.csv. Checked, and just one bormicon region per stat_sq so can condense into a key table. 

# going to condense a few bormicon regions for ease of predicting. In my prediction raster I combined 112 and 111, and 109 and 110. 
# 115 is not in the prediction data at all. going to join with 113--eastern extension. 112 is not well represented in the data (only 41 points for data with sst min/max). Going to join with 111 as a northern extension. 110 also not well represented (only 36 points), but not super clear how to handle it--represents Greenland shelf. I think makes more sense to join with 111 and 112 than with 109 for a larger prediction, but the portion of 110 that's within the EEZ is contiguous with 109. Will convert to 109 for now. 

hab_table <- read.csv ("Data/Raw_data/bormicon_table.csv") %>%
  group_by(stat_sq) %>%
  summarize (bormicon_region = first(division),
             bormicon_subdivision = first (subdivision)) %>%
  mutate (stat_sq = as.factor(stat_sq),
          bormicon_region = case_when (bormicon_region == 115 ~ 113,
                                       bormicon_region == 112 ~ 111,
                                       bormicon_region == 110 ~ 109,
                                       TRUE ~ as.numeric(bormicon_region)
                                       )
          )

# mfri has a 4214 stat sq not in hab_table. I think I looked at it, definitely in 421 stat sq. division would be 101. 

# GLORYS sst min, max, stdev ----

# I wrote the .grd files with Data_setup/Rasterize_GLORYS.R

# https://resources.marine.copernicus.eu/?option=com_csw&view=details&product_id=GLOBAL_REANALYSIS_PHY_001_031
# temperature reanalysis product for more sophisticated temperature variables, e.g. annual max and min, as in Morely et al. 2018

# update 9/14/2020:
# for max and min, I need to find the layer that corresponds to the point date, then take a subset representing the preceding twelve layers. then I extract the cells corresponding to the points, and calculate the max or min. 

sst_max <- brick ("Data/glorys_sst_month_max.grd")
sst_min <- brick ("Data/glorys_sst_month_min.grd")
bt_max <- brick ("Data/glorys_bt_month_max.grd")
bt_min <- brick ("Data/glorys_bt_month_min.grd")


# vector of corresponding MFRI dates, starting one year ahead
mfri_dates_gl <- mfri_dates[which (between (mfri_dates, ymd("1994-01-01"), ymd("2018-12-01")))]

# set up vector of glorys dates (actually took values from the 16th, but just use 01 here to match)
glorys_dates <- seq (ymd("1993-01-01"), ymd("2018-12-01"), by = "months")

 

# make empty df
mfri_glorys_df <- data.frame()

for (x in mfri_dates_gl) {
  # subset mfri data
  mfri <- mfri_pts[which (mfri_pts$date == x),]
  
  # subset glorys brick with 12 preceding layers
  y <- which (glorys_dates == x)
  
  # take a subset representing date and 12 months prior; extract values for each lat/lon point, and calculate max of those values
  smax_extract <- apply (
    raster::extract (
      subset (sst_max, (y-11):y),
      mfri, method = "simple"), 
    1, function (x) max (x, na.rm = TRUE))
  
  smin_extract <- apply (
    raster::extract (
      subset (sst_min, (y-11):y),
      mfri, method = "simple"), 
    1, function (x) min (x, na.rm = TRUE))
  
  bmax_extract <- apply (
    raster::extract (
      subset (bt_max, (y-11):y),
      mfri, method = "simple"), 
    1, function (x) max (x, na.rm = TRUE))
  
  bmin_extract <- apply (
    raster::extract (
      subset (bt_min, (y-11):y),
      mfri, method = "simple"), 
    1, function (x) min (x, na.rm = TRUE))
  
  # combine into data frame
  x_df <- data.frame (sample_id = mfri$sample_id, 
                      sst_max = smax_extract,
                      sst_min = smin_extract,
                      bt_max = bmax_extract,
                      bt_min = bmin_extract)
  
  mfri_glorys_df <- rbind (mfri_glorys_df, x_df)
}

# there are 116 missing values that are too close to shore to be resolved I think

# SST std dev----

# full range of dates, don't need to go back a year

sst_dev <- brick ("Data/glorys_sst_dev.grd")
bt_dev <- brick ("Data/glorys_bt_dev.grd")

mfri_glorys_dev_df <- data.frame()

for (x in glorys_dates) {
  # subset mfri data
  mfri <- mfri_pts[which (mfri_pts$date == x),]
  
  # subset glorys brick 
  y <- which (glorys_dates == x)
  
  # grab mfri points
  sdev_extract <- raster::extract (
    subset(sst_dev, y),
    mfri, method = "simple")
  
  bdev_extract <- raster::extract (
    subset(bt_dev, y),
    mfri, method = "simple")
  
  dev_df <- data.frame (sample_id = mfri$sample_id,
                        sst_dev = sdev_extract,
                        bt_dev = bdev_extract)
  
  mfri_glorys_dev_df <- rbind (mfri_glorys_dev_df, dev_df)

  }
  
## rugosity ----
# https://www.rdocumentation.org/packages/raster/versions/2.6-7/topics/terrain
# using 0.05 cells for now, per Morely et al. 2018
library (marmap)
mfri_env_df <- read_csv ("Data/MFRI_predictor_df.csv",
                         col_types = cols(
                           sample_id = col_factor(),
                           stat_sq = col_factor(),
                           bormicon_region = col_factor(),
                           sst_dev = col_number(),
                           bt_dev = col_number(),
                           sst_max = col_number(),
                           sst_min = col_number(),
                           bt_max = col_number(),
                           bt_min = col_number()
                         ))

depth3 <- getNOAA.bathy(lon1 = -45, lon2 = 15,
                       lat1 = 50, lat2 = 85, 
                       resolution = 3)

depth1 <- getNOAA.bathy(lon1 = -45, lon2 = 15,
                        lat1 = 50, lat2 = 85, 
                        resolution = 1)

depth6 <- getNOAA.bathy(lon1 = -45, lon2 = 15,
                        lat1 = 50, lat2 = 85, 
                        resolution = 6)



depth_r <- as.raster(depth6)

f <- matrix(1, nrow=3, ncol=3)

rug_abs <- focal(depth_r, w=f, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8, pad=TRUE, padValue=NA)
rug_abs6 <- focal(depth_r, w=f, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8, pad=TRUE, padValue=NA)

rug_extract1 <- raster::extract (rug_abs, mfri_pts,  method = "simple")
rug_extract6 <- raster::extract (rug_abs6, mfri_pts,  method = "simple")

rug_df <- data.frame (
  sample_id = mfri_pts$sample_id,
  #rugosity = rug_extract,
  rugosity1 = rug_extract1,
  rugosity6 = rug_extract6
  
)

mfri_env_df <- mfri_env_df %>%
  left_join (rug_df, by = "sample_id")

# combine df ----

mfri_env_df <- mfri_samples %>%
  left_join (hab_table, by = "stat_sq") %>%
  # fix missing stat_sq, division should be 101
  mutate (bormicon_region = ifelse (stat_sq == "4214", 101, bormicon_region)) %>%
  left_join (mfri_sal_df, by = "sample_id") %>%
  left_join (mfri_glorys_df, by = "sample_id") %>%
  left_join (mfri_glorys_dev_df, by = "sample_id") %>%
  left_join (rug_df, by = "sample_id")



# write csv----
write.csv (mfri_env_df, file = "Data/MFRI_predictor_df.csv", row.names = FALSE)









### =========================
# explore missing values ----
### =========================

# inf glorys
gl_inf <- mfri_env_df[which (is.infinite(mfri_env_df$sst_max)),]

ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-30, -8), ylim = c (62,70), expand = FALSE ) +
  geom_point (aes (x = lon, y = lat),alpha = 0.7, data = gl_inf) +
  theme_bw() +
  theme (axis.text = element_blank())


mfri_inf <- mfri_pts[which (mfri_pts$sample_id %in% gl_inf$sample_id),]

# plot check for missing gins values
library (ncdf4)
gins_temp_files <- list.files (path = "../Documents/Python Scripts/", pattern = "GINS_temp_", full.names = T)

gins <- nc_open (gins_temp_files[1])
lat <- ncvar_get (gins, "lat")
lon <- ncvar_get (gins, "lon")
gins_bt <- ncvar_get (gins, "t_an")
nc_close(gins)

image(lat, lon, gins[[1]])
maps::map("world", add = TRUE)

plot(gins_bt,1)

# doesn't seem to be anything missing in here. 

mfri_env_df <- read_csv("Data/MFRI_predictor_df.csv")

# 8042 missing for both bottom and salinity, presumably the same ones?

# 7261 points are after 2012, so that explains those ones. 
bt_nas <- mfri_env_df$sample_id[which(is.na(mfri_env_df$gins_bt))]
sal_nas <- mfri_env_df$sample_id[which(is.na(mfri_env_df$gins_sal))]

which (! sal_nas %in% bt_nas) # 0; they're the same. 

missing_bt <- mfri_env_df %>%
  filter (is.na(gins_bt) & date < "2012-12-12")

missing_bt_index <- which (is.na(mfri_env_df$gins_bt))

# just do outline
library (maps)
library (mapdata)

#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
library (sf)
library (rnaturalearth)
library (rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")  

ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-30, -8), ylim = c (62,70), expand = FALSE ) +
  geom_point (aes (x = lon, y = lat),alpha = 0.7, data = missing_bt) +
  theme_bw() +
  theme (axis.text = element_blank())

ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-30, -8), ylim = c (62,70), expand = FALSE ) +
  geom_point (aes (x = lon, y = lat),alpha = 0.7, data = mfri_env_df) +
  theme_bw() +
  theme (axis.text = element_blank())

# these ones are quite close to shore, so maybe not getting gins. try to plot?
 missing_bt <- filter (mfri_env_df, month == 4)
# make spatial
coordinates(missing_bt) <- ~lon + lat

plot (gins_bt, 1, xlim = c(-30, -8), ylim = c(62,70))
points (missing_bt)

# do any of these points not have their own bt
length (which (is.na(missing_bt$bottom_temp))) # 53

## bormicon/stat sq issue 
gplot(data = world) +
       geom_sf() +
      coord_sf (xlim = c(-30, -8), ylim = c (62,70), expand = FALSE ) +
      geom_point (aes (x = lon, y = lat, col = season), 
                                   alpha = 0.7, 
                                     data = filter (mfri_samples, stat_sq == "4214")) + 
       theme_bw() +
       theme (axis.text = element_blank())
