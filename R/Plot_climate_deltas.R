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

load ("Data/CMIP6_delta_projections/projection_mn_sd_table_CM26.RData")

delta_df %>%
  #group_by(ssp, var) %>%
  #summarize(annual_mn = mean(mean)) %>%
  #rename (year = `year(date)`) %>% 
  ggplot (aes (x = date, y =mean), alpha = 0.4) +
  geom_point() +
  geom_line() +
  #stat_summary(aes(group=var), fun.y=mean, geom="line", colour="black", lwd = 2) +
  facet_grid (ssp ~ var)+
  theme_bw()

# plot monthly values (hard to see)
projection_means %>%
  group_by(year(date), model, ssp, var) %>%
  summarize(annual_mn = mean(mean)) %>%
  rename (year = `year(date)`) %>% 
  ggplot (aes (x = year, y =annual_mn, shape = var, col = ssp)) +
  geom_point() +
  geom_line() +
  facet_wrap (~model)

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
  stat_summary(aes(group=var), fun.y=mean, geom="line", colour="black", lwd = 2) +
  facet_wrap (~ssp)+
  scale_shape_discrete (guide = 'none') +
  scale_color_discrete (name = "Model", labels = c("CM 2.6, 0.1 deg", "CNRM, 25km", "GFDL, 25km", "IPSL, 100km", "MOHC, 100km")) +
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
  group_by(date, model, ssp, var) %>%
  summarize(annual_mn = mean(mean)) %>%
  #rename (year = `year(date)`) %>% 
  ggplot (aes (x = date, y =annual_mn, col = model), alpha = 0.4) +
  geom_point() +
  geom_line() +
  stat_summary(aes(group=var), fun.y=mean, geom="line", colour="black", lwd = 2) +
  facet_wrap (~var) +
  theme_bw()
