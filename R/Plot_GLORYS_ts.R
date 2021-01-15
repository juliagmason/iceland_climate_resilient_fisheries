## Time series of GLORYS backcast
# 12/9/2020
# JGM

library (tidyverse)
library (raster)
library (sf)
library (lubridate)


# iceland EEZ shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=5680
eez <- st_read("Data/eez.shp")

# GLORYS north atlantic raster bricks. monthly observations (16th day of each month) from 1993-2018
gl_sst <- brick ("Data/glorys_sst_16_sample.grd") # 140-240-312, for 1993-2018. 
gl_bt <- brick ("Data/glorys_bt_16_sample.grd")

# clip Iceland's waters
sst_clip <- mask (gl_sst, eez)
bt_clip <- mask (gl_bt, eez)

# quick plot for MFRI pres
png ("Figures/GLORYS_SST_map_MFRI.png", width = 6, height = 6, units = "in", res = 300)
plot (sst_clip, 1,
      xlim = c (-32, -3), ylim = c (60, 69),
      zlim = c(-2, 10),
cex.main = 2, 
main = "GLORYS SST")
map('worldHires',add=TRUE, col='grey90', fill=TRUE)
dev.off()

# vector of dates
glorys_dates <- seq (ymd("1993-01-16"), ymd("2018-12-16"), by = "months")

# extract mean value for each month and convert to df
glorys_df <- data.frame (
  date = rep (glorys_dates, 2),
   # cellStats will return one value for each layer
   mean = c(cellStats (sst_clip, "mean"), cellStats(bt_clip, "mean")), 
   sd = c(cellStats (sst_clip, "sd"), cellStats (bt_clip, "sd")),
   var = c (rep ("SST", 312), rep ("BT", 312))
)

# plot

glorys_annual <- glorys_df %>%
  group_by (year(date), var) %>%
  summarise (mean = mean(mean))%>%
  rename (date = `year(date)`) %>%
  # https://stackoverflow.com/questions/30255833/convert-four-digit-year-values-to-a-date-type
  mutate (var = factor(var, levels = c("SST", "BT")))#, 
          #date = as.Date (paste(date, 12, 16, sep = "-")))

png ("Figures/GLORYS_temp_ts_widescreen.png", width = 16, height = 9, units = "in", res = 300)

glorys_df %>%
  mutate (var = factor(var, levels = c("SST", "BT"))) %>%
 
  ggplot (aes (x = date, y = mean, shape = var), alpha = 0.4) +
  geom_point() +
  geom_line() +
  # add annual trend
  geom_line (aes (x = date, y = mean), data = glorys_annual, lwd = 2) +
  #geom_smooth() +
  scale_shape_discrete (guide = 'none') +
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
  ggtitle ("GLORYS temperature reanalysis for Iceland's EEZ")
dev.off()


# try to do glorys and cmip6??
load ("Data/CMIP6_delta_projections/projection_mn_sd_table_CM26.RData")

cmip <- projection_means %>%
  mutate (var = factor(toupper(var), levels = c("SST", "BT"))) %>%
  #filter (model == "gfdl") %>%
  group_by(year(date), model, ssp, var) %>%
  summarize(annual_mn = mean(mean)) %>%
  rename (year = `year(date)`) 


ggplot () +
  geom_point (aes (x = date, y = mean, shape = var), data = glorys_annual) +
  geom_line (aes (x = date, y = mean, shape = var), data = glorys_annual) +
  geom_point (aes (x = year, y =annual_mn, col = model, shape = var), data = cmip) +  
  geom_line (aes (x = year, y =annual_mn, col = model, shape = var), data = cmip) +  
  facet_wrap (~ssp)
  

# use IPCC colors?
#https://www.ipcc.ch/site/assets/uploads/2019/04/IPCC-visual-style-guide.pdf
#https://stackoverflow.com/questions/41811653/rgb-with-ggplot2-in-r
ipcc_red <- rgb (153, 0, 2, maxColorValue = 255)
ipcc_blue <- rgb (112, 160, 205, maxColorValue = 255)

png ("Figures/Temp_ts_GLORYS_CMIP_BT_only.png" , width = 16, height = 9, units = "in", res = 300)

png ("Figures/Temp_ts_GLORYS_CMIP_SST_only.png" , width = 16, height = 9, units = "in", res = 300)
ggplot () +
  geom_point (aes (x = date, y = mean), data = filter (glorys_annual, var == "SST")) +
  geom_line (aes (x = date, y = mean), data = filter (glorys_annual, var == "SST")) +
  #stat_summary(aes(group=ssp, x = year, y =annual_mn, lty = ssp), fun = mean, geom="line", colour="black", lwd = 1.5, data = cmip) +
  geom_point (aes (x = year, y =annual_mn, shape = model, col = ssp), data = filter(cmip, var == "SST") , alpha = 0.5) +  
  geom_line (aes (x = year, y =annual_mn, shape = model, col = ssp), data = filter(cmip, var == "SST"), alpha = 0.5) +  
  geom_smooth (aes (x = year, y = annual_mn, col = ssp), data = filter(cmip, var == "SST"), method = "loess") +
  
  #facet_grid (rows = vars(var), scales = "free_y") + 
  theme_bw() +
  labs (y = "Degrees C, annual mean", x = "") +
  theme (
    axis.text = element_text (size = 18),
    axis.title = element_text (size = 20),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 20),
    legend.title = element_text (size = 20),
    legend.text = element_text (size = 18)
  ) +
  scale_x_continuous (breaks = c (2000, 2020, 2040, 2060, 2080, 2100)) +
  scale_color_manual (values = c (ipcc_blue, ipcc_red), name = "Scenario") +
  ggtitle ("Mean annual surface temperature in Iceland's EEZ, GLORYS reanalysis and CMIP6 projections")
dev.off()

