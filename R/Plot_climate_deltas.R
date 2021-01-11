### Plot climate projection deltas
# 7/20/2020
# JGM

# Makes a tidy data frame with mean and sd of SST and bottom temperature for each model and scenario

# I have a raster brick for each model's deltas and projections, all interpolated to OISST. 

# I want to subset the brick for the values that correspond to the mfri survey/Iceland's waters (especially for MOHC which has some suspect cropping). This code makes a box with the boundaries of Iceland's EEZ; may want to eventually clip to the actual shapefile, or decide on a bounding box. 
library (tidyverse)
library (raster)
library (lubridate)
library (sf)


# iceland EEZ shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=5680
eez <- st_read("Data/eez.shp")

# plot to check
ggplot() + 
  geom_sf(data = eez, size = 3, color = "black", fill = "cyan1") + 
  ggtitle("eez test plot") + 
  coord_sf()

# create vector of dates. should be jan 2015 - december 2100
# when including CM 2.5, only 80 years, not 86, 2001-2080
#projected_dates <- seq (ymd("2015-01-01"), ymd("2100-12-12"), by = "months")

# list of my saved raster bricks
brick_files <- list.files ("Data/CMIP6_delta_projections/", pattern = "projection.grd")


# make empty data frame
projection_means <- data.frame()

for (file in brick_files) {
  
  projections <- brick(paste0("Data/CMIP6_delta_projections/", file))
  
  #crop CMIP6 files to smaller time period
  #if (dim (projections)[3]  == 1032)  {projections  <-  projections[[73:1032]] }
  # actually should crop cm 2.6, because that should cut off at 2080? take off the first 15 years. # assuming CM 2.6 is 960, so assuming 2001 -2080. 
  if (dim (projections)[3] != 1032) {
    
    projections <- projections[[169:960]]
    projected_dates <- seq (ymd("2015-01-01"), ymd("2080-12-12"), by = "months")
    
    } else {projected_dates <- seq (ymd("2015-01-01"), ymd("2100-12-12"), by = "months")}


  # crop raster to EEZ extent
  proj_ISL <- mask (projections, eez) # it's doing a box, not the shapefile
  
  # grab mean and sd values, and create tidy data frame
  # https://gis.stackexchange.com/questions/200394/calculate-mean-value-for-each-raster-and-convert-output-array-to-data-frame
  
  means_df <- data.frame (date = projected_dates,
                          mean = cellStats (proj_ISL, "mean"),
                          sd = cellStats (proj_ISL, "sd"),
                          model = strsplit (file, "_")[[1]][1],
                          ssp = strsplit (file, "_")[[1]][2],
                          var = strsplit (file, "_")[[1]][3]
                          )
  
  projection_means <- rbind (projection_means, means_df)
  
}

save (projection_means, file = "Data/CMIP6_delta_projections/projection_mn_sd_table_CM26.RData")

### load and plot ----
load ("Data/CMIP6_delta_projections/projection_mn_sd_table_CM26.RData")

# plot annual values 
png ("Figures/CMIP_CM26_temp_projections_ts_widescreen.png", width = 16, height = 9, units = "in", res = 300)
projection_means %>%
  mutate (var = factor(var, levels = c("sst", "bt"))) %>%
  #filter (model == "gfdl") %>%
  group_by(year(date), model, ssp, var) %>%
  summarize(annual_mn = mean(mean)) %>%
  rename (year = `year(date)`) %>% 
  ggplot (aes (x = year, y =annual_mn, col = model, shape = var), alpha = 0.4) +
  geom_point() +
  geom_line() +
  stat_summary(aes(group=var), fun.y=mean, geom="line", colour="black", lwd = 1.5) +
  facet_wrap (~ssp)+
  scale_shape_discrete (guide = 'none') +
  #scale_color_discrete (name = "Model", labels = c("CM 2.6, 0.1 deg", "CNRM, 25km", "GFDL, 25km", "IPSL, 100km", "MOHC, 100km")) +
  scale_color_discrete (name = "Model", labels = c("GFDL CM 2.6", "CNRM-CERFACS", "NOAA GFDL", "IPSL", "MOHC HadGEM3")) +
  theme_bw() +
  theme (
    axis.text = element_text (size = 18),
    axis.title = element_text (size = 20),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 20),
    legend.title = element_text (size = 20),
    legend.text = element_text (size = 18)
  ) +
  labs (y = "Degrees C, annual mean",
        x = "") +
  ggtitle ("Climate model projections (delta + climatology) for Iceland's EEZ")
dev.off()

projection_means %>%
  filter (ssp == 585, var == "sst") %>%
  ggplot (aes (x = date, y = mean)) +
  #geom_point() +
  geom_line() +
  facet_grid (rows = vars(model)) +
  theme_bw()

projection_means %>%
  filter (ssp == 585, var == "sst") %>%
  ggplot (aes (x = date, y = sd)) +
  #geom_point() +
  geom_line() +
  facet_grid (rows = vars(model)) +
  theme_bw()


projection_means %>%
  filter (ssp == 585, var == "sst") %>%
  #filter (model == "gfdl") %>%
  group_by(year(date), model) %>%
  summarize(annual_sd = mean(sd)) %>%
  rename (year = `year(date)`) %>% 
  ggplot (aes (x = year, y = annual_sd)) +
  #geom_point() +
  geom_line() +
  facet_grid (rows = vars(model)) +
  theme_bw()

# what is the projected mean warming?

# should I use the slope of the trend, or difference in means historical vs future?
summary (lm (mean ~ date, data = filter (projection_means, ssp == 245, var == "sst")))
# y = 6.3 + 4.733e-05x
4.733e-05 * 1032 # 0.05

projection_means %>% filter (ssp == 245, var == "sst", year(date) %in% c(2015:2020)) %>% summarise (mean = mean(mean)) # 7.15
projection_means %>% filter (ssp == 245, var == "sst", year(date) %in% c(2091:2100)) %>% summarise (mean = mean(mean)) # 8.56

projection_means %>% filter (ssp == 585, var == "sst", year(date) %in% c(2015:2020)) %>% summarise (mean = mean(mean)) # 6.8
projection_means %>% filter (ssp == 585, var == "sst", year(date) %in% c(2091:2100)) %>% summarise (mean = mean(mean)) # 10.3

projection_means %>% 
  filter (year(date) %in% c(2015:2020, 2091:2100)) %>%
  mutate (period = ifelse (year(date) %in% 2015:2020, "past", "future")) %>%
  group_by (ssp, var, period) %>%
  summarize (mean = mean(mean)) %>% 
  pivot_wider (names_from = period, values_from = mean) %>% 
  mutate (diff = future - past)

summary (lm (mean ~ date, data = filter (projection_means, ssp == 585, var == "sst")))
# y = 4.506 + 1.163e-04x

# what are the predicted increases in iceland's waters? ----
# moved deltas to external hard drive

## Calculate avg warming deltas
# 12/23/2020


library (tidyverse)
library (raster)
library (lubridate)
library (sf)


# iceland EEZ shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=5680
eez <- st_read("Data/eez.shp")

# create vector of dates. should be jan 2015 - december 2100
# when including CM 2.5, only 80 years, not 86, 2001-2080
#projected_dates <- seq (ymd("2015-01-01"), ymd("2100-12-12"), by = "months")

# list of my saved raster bricks
delta_files <- list.files ("Data/CMIP6_deltas/", pattern = "deltas.grd")


# make empty data frame
delta_means <- data.frame()

for (file in delta_files) {
  
  deltas <- brick(paste0("Data/CMIP6_deltas/", file))
  
  #crop CMIP6 files to smaller time period
  #if (dim (projections)[3]  == 1032)  {projections  <-  projections[[73:1032]] }
  # actually should crop cm 2.6, because that should cut off at 2080? take off the first 15 years. # assuming CM 2.6 is 960, so assuming 2001 -2080. 
  if (dim (deltas)[3] != 1032) {
    
    deltas <- deltas[[169:960]]
    projected_dates <- seq (ymd("2015-01-01"), ymd("2080-12-12"), by = "months")
    
  } else {projected_dates <- seq (ymd("2015-01-01"), ymd("2100-12-12"), by = "months")}
  
  
  # crop raster to EEZ extent
  proj_ISL <- mask (deltas, eez) # it's doing a box, not the shapefile
  
  # grab mean and sd values, and create tidy data frame
  # https://gis.stackexchange.com/questions/200394/calculate-mean-value-for-each-raster-and-convert-output-array-to-data-frame
  
  means_df <- data.frame (date = projected_dates,
                          mean = cellStats (proj_ISL, "mean"),
                          sd = cellStats (proj_ISL, "sd"),
                          model = strsplit (file, "_")[[1]][1],
                          ssp = strsplit (file, "_")[[1]][2],
                          var = strsplit (file, "_")[[1]][3]
  )
  
  delta_means <- rbind (delta_means, means_df)
  
}

save (delta_means, file = "Data/CMIP6_deltas/delta_mn_sd_table_CM26.RData")

delta_means %>%
  filter (year(date) %in% 2061:2080) %>%
  group_by (ssp, var, model) %>%
  summarise (mean = mean(mean))

delta_means %>%
  filter (year(date) %in% 2061:2080) %>%
  group_by (ssp, var) %>%
  summarise (mean = mean(mean))
