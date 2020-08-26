## Build predictor variable df
# 6/26/2020
# JGM

# Start to build dataframe for attaching environmental predictor variables to sample lat lon

library (tidyverse)
library (raster)
library (lubridate)

# csv of MFRI tow information
mfri_samples <- read_csv ("Data/MFRI_comb_survey_samples.csv",
                          col_types = cols(
                            sample_id = col_factor(),
                            stat_sq = col_factor()))

# make spatial points df for extracting environmental data
mfri_pts <- mfri_samples %>%
  dplyr::select (sample_id, date, lat, lon) %>%
  arrange (date)  #not originally in date order

coordinates(mfri_pts) <- ~lon + lat

# environmental data raster bricks
# https://stackoverflow.com/questions/34294376/how-to-extract-data-from-a-rasterbrick

# bottom temp, bottom salinity from GINS climatology
gins_bt <- brick ("Data/GINS_bottom_temp.grd")
gins_sal <- brick ("Data/GINS_bottom_sal.grd")

# OISST 
oisst <- brick ("Data/OISST_MFRI.grd")

# Extract OISST ----

mfri_dates <- unique (mfri_pts$date) # 106

# set up vector of OISST dates
oisst_dates <- seq (ymd("1985-01-01"), ymd("2020-06-01"), by = "months")

# make empty df
mfri_oisst_df <- data.frame(sample_id = factor(),
                            oisst = numeric())

for (x in mfri_dates) {
  # subset mfri data
  mfri <- mfri_pts[which (mfri_pts$date == x),]
  
  # subset oisst brick. y is the oisst_dates index that matches x date
  y <- which (oisst_dates == x)
  oisst_subset <- subset (oisst, y)
  oisst_extract <- raster::extract (oisst_subset, mfri, method = "simple")
  
  x_df <- data.frame (sample_id = mfri$sample_id, oisst = oisst_extract)
  
  mfri_oisst_df <- rbind (mfri_oisst_df, x_df)
}

# wants sample_id to be ch for some reason...
mfri_oisst_df$sample_id <- as.factor(mfri_oisst_df$sample_id)

# Extract GINS bottom temp ----

# This one is harder--values are averaged by decade and only go to 2012

mfri_dates_gins <- unique (mfri_pts$date[which (mfri_pts$date < "2013-01-01")]) # 106 total dates; 73 before 2012

gins_dates <- paste (
  rep (c(1985, 1995, 2005), each = 12),
  sprintf ("%02d", 1:12),
  "01",
  sep = "-")


# make empty df
mfri_bt_df <- data.frame(sample_id = factor(),
                           gins_bt = numeric())

# doesn't like x in mfri_dates_in for some reason?
for (i in 1:length(mfri_dates_gins)) {
  x <- mfri_dates_gins[i]
  # subset mfri data
  mfri <- mfri_pts[which (mfri_pts$date == x),]
  
  # match date by re-assigning year to appropriate decade
  y <- case_when (
    between (year(x), 1985, 1994) ~ paste0 ("1985", substr(x, 5, 10)),
    between (year(x), 1995, 2004) ~ paste0 ("1995", substr(x, 5, 10)),
    between (year(x), 2005, 2012) ~ paste0 ("2005", substr(x, 5, 10))
  )
  
  #index date
  y_in <- which (gins_dates == y)
  
  # subset oisst brick
  gins_subset <- subset (gins_bt, y_in)
  gins_extract <- raster::extract (gins_subset, mfri, method = "simple")
  
  x_df <- data.frame (sample_id = mfri$sample_id, gins_bt = gins_extract)
  
  mfri_bt_df <- rbind (mfri_bt_df, x_df)
}

mfri_bt_df$sample_id <- as.factor(mfri_bt_df$sample_id)

# Extract GINS salinity ----
# make empty df
mfri_sal_df <- data.frame(sample_id = factor(),
                         gins_sal = numeric())


for (i in 1:length(mfri_dates_gins)) {
  x <- mfri_dates_gins[i]
  # subset mfri data
  mfri <- mfri_pts[which (mfri_pts$date == x),]
  
  # match date by re-assigning year to appropriate decade
  y <- case_when (
    between (year(x), 1985, 1994) ~ paste0 ("1985", substr(x, 5, 10)),
    between (year(x), 1995, 2004) ~ paste0 ("1995", substr(x, 5, 10)),
    between (year(x), 2005, 2012) ~ paste0 ("2005", substr(x, 5, 10))
  )
  
  #index date
  y_in <- which (gins_dates == y)
  
  # subset oisst brick
  gins_subset <- subset (gins_sal, y_in)
  gins_extract <- raster::extract (gins_subset, mfri, method = "simple")
  
  x_df <- data.frame (sample_id = mfri$sample_id, gins_sal = gins_extract)
  
  mfri_sal_df <- rbind (mfri_sal_df, x_df)
}

mfri_sal_df$sample_id <- as.factor(mfri_sal_df$sample_id)

# Attach env vars to data frame----

mfri_env_df <- mfri_samples %>%
  left_join (mfri_oisst_df, by = "sample_id") %>%
  left_join (mfri_bt_df, by = "sample_id") %>%
  left_join (mfri_sal_df, by = "sample_id")

# 8/20/2020 update: adding habitat data from Bormicon regions ----
# From Pamela: # I attached a table you can join with stat_sq column to get at this. We generally use 'Bormicon' regions, which were defined a while ago based on depth and cluster analyses (see link below), but not so much habitat data if I remember correctly (double check this!). That means they may be a bit y ~ x ~ y situation because I believe multi-species survey data were used to define them. The true 'habitat' studies taking place here (using side sonar and such) don't nearly cover enough area to be useful yet.
# https://www.hafogvatn.is/static/research/files/fjolrit-119.pdf  see page 330 - division and subdivision numbers correspond with attached table

# update 8/26/2020:
# From Pamela: We use the Gadget subdivisions but just call them 'Bormicon' because that is where the original justifications for the patterns came from (see figs. 9.1 vs. 9.4 in the document link)... they were just modified to be formed from statistical squares to make data analysis and subsampling easier (and that's why we still use the 'Gadget' rather than the original Bormicon regions). Yes, that stat_sq column has a lot of other 'junk' numbers that are for other parts of the database. But double-checking this made me realize that there is an error in the stations table that may have made it confusing - the 'areacell' column is the same as 'stat_sq', so I just remade the stations table 'tow_table.csv' with that column name corrected and the subdivision and division columns added. 

# I renamed to bormicon_table.csv

div_table <- read.csv ("Data/Raw_data/div_table.csv") %>%
  mutate (stat_sq = as.character(-1*stat_sq)) %>% # not sure why, but stat_sq has a leading hyphen...nope it's a sequence that starts with negative numbers. this isn't right
  rename (Bormicon_region = SUBDIVISION)

mfri_env_df <- read.csv ("Data/MFRI_predictor_df.csv") %>%
  left_join (div_table, by = "stat_sq")

# write csv----
write.csv (mfri_env_df, file = "Data/MFRI_predictor_df.csv", row.names = FALSE)

# explore missing values ----
# plot check for missing gins values
library (ncdf4)
gins_temp_files <- list.files (path = "../Documents/Python Scripts/", pattern = "GINS_temp_", full.names = T)

gins <- nc_open (gins_temp_files[1])
lat <- ncvar_get (gins, "lat")
lon <- ncvar_get (gins, "lon")
nc_close(gins)

image(lat, lon, gins_bt[[1]])
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

# these ones are quite close to shore, so maybe not getting gins. try to plot?

# make spatial
coordinates(missing_bt) <- ~lon + lat

plot (gins_bt, 1, xlim = c(-30, -8), ylim = c(62,70))
points (missing_bt)

# do any of these points not have their own bt
length (which (is.na(missing_bt$bottom_temp))) # 53
