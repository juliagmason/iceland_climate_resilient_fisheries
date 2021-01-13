### Plot climate projection deltas
# 7/20/2020
# JGM

# Makes a tidy data frame with mean and sd of SST and bottom temperature for each model and scenario

# I have a raster brick for each model's deltas and projections, all interpolated to OISST. 

# I want to subset the brick for the values that correspond to the mfri survey/Iceland's waters (especially for MOHC which has some suspect cropping). 


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
  proj_ISL <- mask (projections, eez) 
  
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

## plot warming map for 2000-2018 and 2061-2080----


library (raster)
library (rasterVis)

library(maps) # for plotting country shapefile
library(mapdata)
library(maptools)
library (sp)
library (sf)

library (gridExtra)
library (RColorBrewer)

# for diverge0
# https://stackoverflow.com/questions/33750235/plotting-a-raster-with-the-color-ramp-diverging-around-zero
devtools::source_gist('306e4b7e69c87b1826db')

# just plot deltas ----

# plot deltas (red/blue) binned by decades for SST and BT
# for mean, could just bring in all the deltas for  a scenario, cut into decade chunks, and take overall mean (calc)
# for sd, would first need to do mean for each model, and then take sd for those means, as I did in plot_prediction_maps


#just break into decade chunks and calculate all decades
calc_CM_delta_mean <- function (CM, scenario, var) {
  
  br <- brick (list.files(path = "Data/CMIP6_deltas/", pattern = paste(CM, scenario, var, "deltas.grd", sep = "_"), full.names = TRUE))
  
  # mask to iceland EEZ
  br_mask <- mask (br, eez)
  
  # break into appropriate decades and calculate mean
  # https://stackoverflow.com/questions/31798389/sum-nlayers-of-a-rasterstack-in-r
  
  # cmip6 is 1032 layers, representing 2015-2100. the years I'm interested in would be 2021-2040, 2041-2060, 2061-2080, and 2081-2100, so would want to start at 73. CM28 has 960 layers, 2001-2080, would want to start at 241
  if (CM == "CM26") {
    #subset appropriate years, starting at 2021
    br_sub <- br_mask[[241:960]]
    dec_index <- rep(1:3, each = 20*12)
    
    # calculate mean for each period
    br_dec_mean <- stackApply (br_sub, dec_index, mean, na.rm = TRUE) # this should have 3 layers
    
    
    
  } else { 
    br_sub <- br_mask[[73:1032]]
    dec_index <- rep(1:4, each = 20*12)
    
    # calculate mean for each period
    br_dec_mean <- stackApply (br_sub, dec_index, mean, na.rm = TRUE)
    } # end ifelse

} # end function 

# then would use overlay to get the mean of multiple models. Or, if I use lapply with calc_CM_delta_mean, I would get one big list that I would convert into a brick. then I would need to calculate the mean and sd of every 4th layer. 

plot_delta_mean_sd_maps <- function (var, scenario) {
  
  # CM_list depends on scenario and dec_index
  if (scenario == 245) {
    CM_list <- CM_list <- c("gfdl", "cnrm", "ipsl", "mohc")
    
    # index to take mean of models for the different periods
    dec_avg_index <- rep(1:4, 4)
    
  } else { 
    CM_list <- c("gfdl", "cnrm", "ipsl", "mohc", "CM26")
    dec_avg_index <- rep (1:4, 5)[-20]
   
  
  }
  
  # get decadal means for the variable and scenario
  CMs_ls <- lapply (CM_list, calc_CM_delta_mean, scenario = scenario, var = var) # returns a list
  
  # convert to brick
  CMs_br <- brick (CMs_ls) # should have 19 layers for 585 and 16 for 245, because CM 2.6 doesn't have 2080
  
  
  # calculate mean and sd for each period. returns a brick with 4 layers
  CMs_mn <- stackApply (CMs_br, dec_avg_index, mean, na.rm = TRUE)
  CMs_sd <- stackApply (CMs_br, dec_avg_index, sd, na.rm = TRUE)
  
  # make levelplot object for mean--want to center color ramp at 0, so takes two steps
  p_mean <- levelplot (CMs_mn, margin = F,  
                  xlim = c (-32, -3), ylim = c (60, 69),
                  xlab = NULL, ylab = NULL,
                  #col.regions = colorRampPalette (rev(brewer.pal (9, "RdBu"))),
                  main = paste0(toupper(var), " delta, mean, ", scenario),
                  names.attr = c ("2021-2040", "2041-2060", "2061-2080", "2081-2100"))
  
  # save to file
  
  png (paste0("Figures/Deltas_map_", var, "_", scenario, "_mean.png"), width = 16, height = 9, units = "in", res = 300)
  print(
  diverge0(p_mean, ramp =  colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))) # best I can figure out for reversing RdBu for now
  )
  dev.off()
  
  # plot sd with different color scale
  png (paste0("Figures/Deltas_map_", var, "_", scenario, "_sd.png"), width = 16, height = 9, units = "in", res = 300)
  print(
  levelplot (CMs_sd, margin = F,  
              xlim = c (-32, -3), ylim = c (60, 69),
              xlab = NULL, ylab = NULL,
              col.regions = colorRampPalette (brewer.pal (9, "PuBuGn")),
              main = paste0(toupper(var), " delta, SD, ", scenario),
              names.attr = c ("2021-2040", "2041-2060", "2061-2080", "2081-2100"))
  )
  dev.off()
  
  
} # end function

plot_delta_mean_sd_maps (var = "bt", scenario = 245)

var_cross <- expand_grid (scenario = c(245, 585), var = c("bt", "sst")) %>% as.list()

pmap (var_cross, plot_delta_mean_sd_maps)


# add country polygon for levelplot:
# https://stackoverflow.com/questions/17582532/r-overlay-plot-on-levelplot

load ("Data/prediction_raster_template.RData")
ext <- as.vector(extent(sst_245_mn))

boundaries <- maps::map('worldHires', fill=TRUE,
                        xlim=ext[1:2], ylim=ext[3:4],
                        plot=FALSE)

## read the map2SpatialPolygons help page for details
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
                             proj4string=CRS(projection(sst_245_mn)))

levelplot (sst_245_mn_mask, margin = F,  
           xlim = c (-32, -3), ylim = c (60, 69),
           xlab = NULL, ylab = NULL,
           col.regions = colorRampPalette (rev(brewer.pal (9, "RdBu"))),
           main = "Mean SST delta, 2061-2080") #+ 
  #layer(sp.polygons(bPols, fill = "white")) # bPols not working anymore...

# what is the projected mean warming? ----

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
