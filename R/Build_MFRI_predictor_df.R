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
