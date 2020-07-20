### Plot climate projection deltas
# 7/20/2020
# JGM

# Makes a tidy data frame with mean and sd of SST and bottom temperature for each model and scenario

# I have a raster brick for each model's deltas, all interpolated to OISST. 

# I want to subset the brick for the values that correspond to the mfri survey/Iceland's waters (especially for MOHC which has some suspect cropping). This code makes a box with the boundaries of Iceland's EEZ; may want to eventually clip to the actual shapefile, or decide on a bounding box. 

library (raster)
library (lubridate)
library (sf)
library (stringr)

# iceland EEZ shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=5680
eez <- st_read("Data/eez.shp")

# plot to check
ggplot() + 
  geom_sf(data = eez, size = 3, color = "black", fill = "cyan1") + 
  ggtitle("eez test plot") + 
  coord_sf()

# create vector of dates. should be jan 2015 - december 2100
projected_dates <- seq (ymd("2015-01-01"), ymd("2100-12-12"), by = "months")

# list of my saved raster bricks
brick_files <- list.files ("Data/CMIP6_deltas/", pattern = "deltas.grd")

# make empty data frame
projected_deltas <- data.frame()

for (file in brick_files) {
  
  deltas <- brick(paste0("Data/CMIP6_deltas/", file))
  
  # crop raster to EEZ extent
  delta_crop <- crop (deltas, eez) # it's doing a box, not the shapefile
  
  # grab mean and sd values, and create tidy data frame
  # https://gis.stackexchange.com/questions/200394/calculate-mean-value-for-each-raster-and-convert-output-array-to-data-frame
  
  delta_df <- data.frame (date = projected_dates,
                          mean = cellStats (delta_crop, "mean"),
                          sd = cellStats (delta_crop, "sd"),
                          model = strsplit (file, "_")[[1]][1],
                          ssp = strsplit (file, "_")[[1]][2],
                          var = strsplit (file, "_")[[1]][3]
                          )
  
  projected_deltas <- rbind (projected_deltas, delta_df)
  
}

save (projected_deltas, file = "Data/CMIP6_deltas/deltas_mn_sd_table.RData")


# plot monthly values (hard to see)
projected_deltas %>%
  group_by(year(date), model, ssp, var) %>%
  summarize(annual_mn = mean(mean)) %>%
  rename (year = `year(date)`) %>% 
  ggplot (aes (x = year, y =annual_mn, shape = var, col = ssp)) +
  geom_point() +
  geom_line() +
  facet_wrap (~model)

# plot annual values 
png ("Figures/CMIP_temp_deltas_ts.png")
projected_deltas %>%
  group_by(year(date), model, ssp, var) %>%
  summarize(annual_mn = mean(mean)) %>%
  rename (year = `year(date)`) %>% 
  ggplot (aes (x = year, y =annual_mn, col = model), alpha = 0.4) +
  geom_point() +
  geom_line() +
  stat_summary(aes(group=var), fun.y=mean, geom="line", colour="black", lwd = 2) +
  facet_grid (ssp ~ var)+
  theme_bw()
dev.off()

projected_deltas %>%
  group_by(date, model, ssp, var) %>%
  summarize(annual_mn = mean(mean)) %>%
  #rename (year = `year(date)`) %>% 
  ggplot (aes (x = date, y =annual_mn, col = model), alpha = 0.4) +
  geom_point() +
  geom_line() +
  stat_summary(aes(group=var), fun.y=mean, geom="line", colour="black", lwd = 2) +
  facet_wrap (~var) +
  theme_bw()
