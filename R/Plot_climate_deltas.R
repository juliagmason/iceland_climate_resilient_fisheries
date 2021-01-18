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


# ==================
# Calculate projected mean and sd for Iceland's EEZ for each time step ----
# ==================

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
  
  # CMIP6 projections have 1032 time steps (2015-2100) and CM 2.6 has 960 (2001-2080). Crop off the first 15 years for CM 2.6 
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

# plot projection TS with 2 panels for scenarios----
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


## Plot projection TS with GLORYS backcast ----

# GLORYS north atlantic raster bricks. monthly observations (16th day of each month) from 1993-2018
gl_sst <- brick ("Data/glorys_sst_16_sample.grd") # 140-240-312, for 1993-2018. 
gl_bt <- brick ("Data/glorys_bt_16_sample.grd")

# clip Iceland's waters
sst_clip <- mask (gl_sst, eez)
bt_clip <- mask (gl_bt, eez)

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

glorys_annual <- glorys_df %>%
  group_by (year(date), var) %>%
  summarise (mean = mean(mean))%>%
  rename (date = `year(date)`) %>%
  # https://stackoverflow.com/questions/30255833/convert-four-digit-year-values-to-a-date-type
  mutate (var = factor(var, levels = c("SST", "BT")))

cmip <- projection_means %>%
  mutate (var = factor(toupper(var), levels = c("SST", "BT")),
          model = toupper(model)) %>%
  group_by(year(date), model, ssp, var) %>%
  summarize(annual_mn = mean(mean)) %>%
  rename (year = `year(date)`) 

# plot with SST and BT as panels, models as shapes ----
# use IPCC colors for scenarios
#https://www.ipcc.ch/site/assets/uploads/2019/04/IPCC-visual-style-guide.pdf
#https://stackoverflow.com/questions/41811653/rgb-with-ggplot2-in-r
ipcc_red <- rgb (153, 0, 2, maxColorValue = 255)
ipcc_blue <- rgb (112, 160, 205, maxColorValue = 255)

ts_plot <- ggplot () +
  geom_point (aes (x = date, y = mean), data = glorys_annual) +
  geom_line (aes (x = date, y = mean), data = glorys_annual) +
  #stat_summary(aes(group=ssp, x = year, y =annual_mn, lty = ssp), fun = mean, geom="line", colour="black", lwd = 1.5, data = cmip) +
  geom_point (aes (x = year, y = annual_mn, shape = model, col = ssp), data = cmip , alpha = 0.5) +  
  geom_line (aes (x = year, y = annual_mn, shape = model, col = ssp), data = cmip, alpha = 0.5) +  
  geom_smooth (aes (x = year, y = annual_mn, col = ssp), data = cmip, method = "loess") +
  
  facet_grid (rows = vars(var), scales = "free_y") + 
  theme_bw() +
  labs (y = "Degrees C, annual mean", x = "") +
  theme (
    axis.text = element_text (size = 12),
    axis.title = element_text (size = 14),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 14),
    #legend.position = "none"
    legend.title = element_text (size = 20),
    legend.text = element_text (size = 18)
  ) +
  scale_x_continuous (breaks = c (2000, 2020, 2040, 2060, 2080, 2100)) +
  scale_color_manual (values = c (ipcc_blue, ipcc_red), name = "Scenario") 
  ggtitle ("Mean annual surface temperature in Iceland's EEZ, GLORYS reanalysis and CMIP6 projections")


png ("Figures/CMIP_Glorys_ts_mockup.png", width = 12, height = 8, units = "in", res = 300)
print (ts_plot)
dev.off()

## plot delta warming maps----

library (tidyverse)
library (sf) # for eez shapefile

library (raster)
library (rasterVis)

library (grid)
library (gridExtra)

# iceland EEZ shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=5680
eez <- st_read("Data/eez.shp")

# using several functions to get here. Unfortunately can't figure out how to do more sophisticated raster layer naming/facetting to do this within a list column.

# return a raster layer where each lat/lon cell is the mean temperature over the period provided
calc_CM_delta_mean <- function (CM, scenario, var, period) {
  
  # CM is lowercase 4-digit climate model code
  # scenario is 245 or 585
  # var is sst or bt
  # period should be the first year, e.g. 2021, 2061
  
  br <- brick (list.files(path = "Data/CMIP6_deltas/", pattern = paste(CM, scenario, var, "deltas.grd", sep = "_"), full.names = TRUE))
  
  # mask to iceland EEZ
  br_mask <- mask (br, eez)
  
  # break into appropriate decades and calculate mean

  # cmip6 is 1032 layers, representing 2015-2100. the years I'm interested in would be 2021-2040, 2041-2060, 2061-2080, and 2081-2100, so would want to start at 73. CM28 has 960 layers, 2001-2080, would want to start at 241

  if (CM == "CM26") {
    
    # make a years index to select the appropriate starting layer based on period of interest
    CM_yrs <- rep(2001:2080, each = 12)
    start_layer <- which (CM_yrs == period)[1]
    
    #subset appropriate years, starting at 2021
    br_sub <- br_mask[[start_layer:(start_layer + 239)]]
    
    # calculate mean for period
    br_mean <- calc (br_sub, mean, na.rm = TRUE) 
    
    
    
  } else { 
    
    CM_yrs <- rep(2015:2100, each = 12)
    start_layer <- which (CM_yrs == period)[1]

    #subset appropriate years, starting at 2021
    br_sub <- br_mask[[start_layer:(start_layer + 239)]]

    # calculate mean for period
    br_mean <- calc (br_sub, mean, na.rm = TRUE) 
    
    } # end ifelse

} # end function 

tmp <- calc_CM_delta_mean(CM = "gfdl", scenario = 245, var = "sst", period = 2061)

# then would use overlay to get the mean of multiple models. Or, if I use lapply with calc_CM_delta_mean, I would get one big list that I would convert into a brick. 

# returns one layer that provides either the mean or sd of all the climate models
calc_ensemble_delta <- function (scenario, var, period, metric) {
  # scenario is 245 or 585
  # var is "sst" or "bt"
  # period should be the first year, e.g. 2021, 2061
  # metric is either "mean" or "sd"
  
  # CM_list depends on scenario 
  if (scenario == 245) {
    CM_list <- CM_list <- c("gfdl", "cnrm", "ipsl", "mohc")

    
  } else { 
    
    CM_list <- c("gfdl", "cnrm", "ipsl", "mohc", "CM26")
    
  } # end ifelse
  
  # get decadal means for the variable and scenario
  CM_input <- expand_grid (
    CM = CM_list,
    scenario = scenario,
    var = var, 
    period = period
  ) %>% 
    
    # need to remove CM26 missing period and/or scenario.
    filter (!(CM == "CM26" & period == 2081), 
            !(CM == "CM26" & scenario == 245)) %>%
    as.list () 
    
  CMs_ls <- pmap(CM_input, calc_CM_delta_mean) # returns a list
  
  # convert to brick
  CMs_br <- brick (CMs_ls)
  
  # not sure how to use functions as arguments...
  if (metric == "mean") {
    func = function (x) mean (x, na.rm = TRUE)
  } else if (metric == "sd") {
    func = function (x) sd (x, na.rm = TRUE)
  }
  
  # calculate metric of interest, either mean or SD. should be one layer 
  CMs_metric <- calc (CMs_br, func)

  
} # end function

tmp2 <- calc_ensemble_delta(scenario = 245, var = "sst", period = 2061, metric = "mean"); beep()


# plot mean or sd 2panel scenarios side by side for grid.arrange ----
# hard coded for 2061 
plot_delta_map_fun <- function (var, period, metric) {
  
  # apply ensemble function for both scenarios
  delta_ls <- lapply (c(245, 585), calc_ensemble_delta, var = var, metric = metric, period = period)
  
  # convert to brick
  delta_br <- brick (delta_ls) # should just have two layers
  
  # fix names for labeller
  # labels_names <- setNames(raster_names, unique(NDVI_HARV_stack_df$variable))
  scen_labs <- setNames ( c("SSP 2-4.5", "SSP 5-8.5"), names (delta_br))
 

  # set colorbar
  
  # for 2061: mean -2.55 - 8.46; sd 0 - 4.17
  if (metric == "mean") {
    # colors from RdBu R colorbrewer
    low_val <- "#2166AC"
    mid_val <- "#F7F7F7"
    high_val <- "#B2182B"
    mdpt <- 0
    lim <- c(-2.6, 8.5)
  } else if (metric == "sd") {
    low_val <- "#F7FCFD"
    high_val <- "#00441B"
    mid_val <- "#66C2A4"
    mdpt <- median (getValues (delta_br), na.rm = TRUE)
    lim <- c(0, 4.2)

  }
  
  # plot
  gplot (delta_br) +
    geom_raster (aes (fill = value)) +
    borders (fill = "grey90", col = "black", 
             xlim = c (-32, -3), ylim = c (60, 69)) +
    coord_quickmap (xlim = c (-32, -3), ylim = c (60, 69)) +
    facet_wrap (~variable, labeller = labeller (variable = scen_labs)) +
    scale_fill_gradient2 (low = low_val, mid = mid_val, high = high_val, midpoint = mdpt, na.value = "white", limits = lim) +
    labs (fill = expression(paste(degree, 'C'))) +
    theme_bw () #+
    # better to set this in grid.arrange for different figures
    # theme (
    #   axis.text = element_text (size = 12),
    #   axis.title = element_blank (),
    #   strip.text = element_text (size = 14),
    #   legend.text = element_text (size = 12),
    #   legend.title = element_text (size = 14)
    # ) 
  

  
} # end function

system.time(p_sst <- plot_delta_map_fun ("sst", 2061, "mean"));beep() # about 90s
p_bt <- plot_delta_map_fun ("bt", 2061, "mean")


plots_2060_ls <- expand_grid (var = c("sst", "bt"),
                              period = 2061,
                              metric = c("mean", "sd")) %>%
  as.list()

plots_2060 <- pmap (plots_2060_ls, plot_delta_map_fun); beep()

grid.arrange (plots_2060[[1]] + ggtitle ("SST") + theme(legend.position = "none"), plots_2060[[3]] + theme (axis.text.y = element_blank()) + ggtitle ("BT"), ncol = 2, top = textGrob("Mean", gp = gpar(fontsize = 20), vjust = 0.7))

png ("Figures/Deltas_map_2061_bt.png", width = 4, height = 4, units = "in", res = 300)
grid.arrange (plots_2060[[3]] + theme (
  axis.text = element_text (size = 8),
  axis.title = element_blank (),
  strip.text = element_text (size = 10),
  legend.text = element_text (size = 8),
  legend.title = element_text (size = 10)
),
plots_2060[[4]]+ theme (
  axis.text = element_text (size = 8),
  axis.title = element_blank (),
  strip.text = element_text (size = 10),
  legend.text = element_text (size = 8),
  legend.title = element_text (size = 10)
) , nrow = 2, top = textGrob("Bottom temperature", gp = gpar(fontsize = 12), vjust = 0.7))
dev.off()

png ("Figures/Deltas_map_2061_sst.png", width = 4, height = 4, units = "in", res = 300)
grid.arrange (plots_2060[[1]] + theme (
  axis.text = element_text (size = 8),
  axis.title = element_blank (),
  strip.text = element_text (size = 10),
  legend.text = element_text (size = 8),
  legend.title = element_text (size = 10)
),
plots_2060[[2]]+ theme (
  axis.text = element_text (size = 8),
  axis.title = element_blank (),
  strip.text = element_text (size = 10),
  legend.text = element_text (size = 8),
  legend.title = element_text (size = 10)
) , nrow = 2, top = textGrob("Surface temperature", gp = gpar(fontsize = 12), vjust = 0.7))
dev.off()


# supplemental figure--plot ensemble mean and sd for all periods ----

plot_ensemble_map_ts <- function (var, metric) {
  # var is "sst" or "bt"
  # metric is "mean" or "sd"
  
  delta_plot_input <- expand_grid (
    var = var,
    metric = metric,
    scenario = c (245, 585),
    period = c(2021, 2041, 2061, 2081)) %>%
    as.list()
  
  # calculate ensemble mean or sd layer for each period and scenario
  # this takes ~10 minutes
  ens_ls <- pmap(delta_plot_input, calc_ensemble_delta)
  
  # convert to brick, should have 8 layers
  ens_br <- brick (ens_ls)
  
  # set names for labeller to indicate the prediction period
  period_labs <- setNames (c("SSP 2-4.5: 2021-2040", "2041-2060", "2061-2080", "2081-2100", "SSP 5-8.5: 2021-2040", "2041-2060", "2061-2080", "2081-2100"), names (ens_br))
  
  # for 2061: mean -2.55 - 8.46; sd 0 - 4.17
  if (metric == "mean") {
    # colors from RdBu R colorbrewer
    low_val <- "#2166AC"
    mid_val <- "#F7F7F7"
    high_val <- "#B2182B"
    mdpt <- 0

  } else if (metric == "sd") {
    low_val <- "#F7FCFD"
    high_val <- "#00441B"
    mid_val <- "#66C2A4"
    mdpt <- median (getValues (ens_br), na.rm = TRUE)

  }
  
  # plot and save
  png (paste("Figures/SI_ens_delta_map", var, metric, "allperiods.png", sep = "_"), width = 8, height = 4, res = 150, unit = "in")
  gplot (ens_br) +
    geom_raster (aes (fill = value)) +
    borders (fill = "grey90", col = "black", 
             xlim = c (-32, -3), ylim = c (60, 69)) +
    coord_quickmap (xlim = c (-32, -3), ylim = c (60, 69)) +
    facet_wrap (~variable, ncol = 4, 
                labeller = labeller (variable = period_labs)) +
    # colors from RdBu R colorbrewer
    scale_fill_gradient2 (midpoint = mdpt, low = low_val, mid = mid_val, high = high_val, na.value = "white") +
    labs (fill = expression(paste(degree, 'C'))) +
    ggtitle (paste0 ("Ensemble deltas ", metric, ", ", toupper (var))) +
    theme_bw () +
    theme (axis.title = element_blank()) 
  
  dev.off()
  
}

system.time(plot_ensemble_map_ts(var= "bt", metric = "sd"))
# works internally but not as function....

# supplemental figure--plot maps for each CM, all periods----

plot_CM_delta_map_ts <- function (CM, var) {
  # CM is 4 letter lowercase code
  # var is "bt" or "sst
  
  delta_plot_input <- expand_grid (
    CM = CM,
    var = var,
    scenario = c (245, 585),
    period = c(2021, 2041, 2061, 2081)) %>%
    as.list()
  
  # apply calc_delta function
  CM_ls <- pmap (delta_plot_input, calc_CM_delta_mean)
  CM_br <- brick (CM_ls)
  
  # set names for labeller to indicate the prediction period
  period_labs <- setNames (c("SSP 2-4.5: 2021-2040", "2041-2060", "2061-2080", "2081-2100", "SSP 5-8.5: 2021-2040", "2041-2060", "2061-2080", "2081-2100"), names (CM_br))

  # plot
  png (paste("Figures/SI_CM_delta_map_allperiods", CM, var, ".png", sep = "_"), width = 8, height = 4, res = 150, unit = "in")
  gplot (CM_br) +
    geom_raster (aes (fill = value)) +
    borders (fill = "grey90", col = "black", 
             xlim = c (-32, -3), ylim = c (60, 69)) +
    coord_quickmap (xlim = c (-32, -3), ylim = c (60, 69)) +
    facet_wrap (~variable, ncol = 4, 
                labeller = labeller (variable = period_labs)) +
    # colors from RdBu R colorbrewer
    scale_fill_gradient2 (midpoint = 0, low = "#2166AC", mid = "#F7F7F7", high = "#B2182B", na.value = "white") +
    labs (fill = expression(paste(degree, 'C'))) +
    ggtitle (paste(toupper (CM), var)) +
    theme_bw () +
    theme (axis.title = element_blank()) 
  
  dev.off()
  
} # end function



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

## Calculate avg warming deltas ----
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
