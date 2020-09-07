## Plot habitat suitability time series
# 8/6/2020
# JGM

library (raster)
library (sf)
library (lubridate)

# iceland EEZ shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=5680
eez <- st_read("Data/eez.shp")

# vector of dates
projected_dates <- seq (ymd("2015-01-01"), ymd("2100-12-12"), by = "months")

# load prediction bricks
# eventually these will be in Models/prediction_bricks

# for now, have temporary saved file in Data
tmp_brick <- brick ("Models/Prediction_bricks/Gadus_morhua_245_temp.grd")

allvals <- getValues(tmp_brick)
summary (allvals[1])

pred_clip <- mask (tmp_brick, eez)

plot (tmp_brick, 1000)
plot (log(tmp_brick), 1000)

vals <- getValues (pred_clip[[1000]])

suit_cells <- data.frame()

for (i in 1:dim(pred_clip)[3]){
  vals <- getValues(pred_clip[[i]])
  
  suit_df <- data.frame (
    date = projected_dates[i],
    sum = sum(vals, na.rm = TRUE),
    mean = mean(vals, na.rm = TRUE),
    ncells0.5 = length (which (vals > 0.5)),
    ncells1 = length(which (vals > 1)),
    ncells10 = length (which (vals > 10))
  )
  
  suit_cells <- rbind (suit_cells, suit_df)
}


plot (sum ~ date, data = suit_cells)
plot (mean ~ date, data = suit_cells)
plot (ncells1 ~ date, data = suit_cells)
plot (ncells10 ~ date, data = suit_cells)

# how many cells in clip
vals.c <- length (which (!is.na(vals))) # 2259

suit_cells %>%
  group_by (year(date)) %>%
  summarize (prop_cells1 = sum(ncells1, na.rm = TRUE)/(12*vals.c)) %>% 
  rename (year = `year(date)`) %>%
  ggplot (aes (x = year, y = prop_cells1)) +
  geom_point() +
  geom_line() +
  ggtitle ("Proportion of cells with probability cod biomass > 1")
