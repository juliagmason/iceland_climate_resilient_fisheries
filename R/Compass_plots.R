## Radial compass plots
# 8/21/2020
# JGM

library (geosphere) # for distHaversine and bearing
library (raster)
library (tidyverse)
library (beepr) # for alerting me when code is done
library (SDMTools) # for calculating circular average. Not available for R version 4.0.2, downloaded from https://www.rforge.net/SDMTools/git.html
#library (spatstat) # for weighted quantiles



# Code from Morely et al. 2018, calculating % habitat change as change in sum of prediction values

# trying something different, with purr. Still think it's faster to separate historical and future
hist_centroids_fun <- function (sci_name) {
  # load raster brick for historical period
  hist_br <- brick (file.path ("Models/Prediction_bricks", 
                               paste(sci_name, "Borm_14_alltemp", "2000_2018.grd", sep = "_"))
  )
  
  # take mean so have one value per period
  hist_mn <- calc (hist_br, mean) # calc is faster than StackApply
  
  # spatial points df so I have a value corresponding to each lat/lon combo
  df_pre <- data.frame(rasterToPoints(hist_mn))
  
  
  # subset above a certain threshold for warm and cool edge. for cod, 0.05 is 67th percentile and 0.5 is 70th percentile. go with 0.5? Tried this, changing to 0.05 because was zero for a lot of species
  df_pre_suit <- filter (df_pre, layer > 0.05)
  
  # eventually want something about circular movement around iceland??
  data.frame (
    species = sci_name,
    hist_lat = weighted.mean(df_pre$y, df_pre$layer), # layer is the thermpred prediction value
    hist_lon =  weighted.mean(df_pre$x, df_pre$layer),
    #warm and cool edges, Fredston-Herman 2020. should probably get this from obs, not backcast? 5th and 95th latitude quantile
    # might not be that helpful because i'm limiting to EEZ, and might run into borm problems
    # weighted quantile is making these very central. Fredston used 5th and 95th where occurs above 80% probability, but not sure how to capture that with thermpred. 
    hist_warm = quantile (df_pre_suit$y, probs = 0.05, na.rm = TRUE),
    hist_cold  = quantile (df_pre_suit$y,  probs = 0.95, na.rm = TRUE)
  )
}


load ("Models/spp_Smooth_latlon.RData") 
borm_spp <- spp_Smooth_latlon %>%
  filter (!sci_name_underscore == "Myoxocephalus_scorpius")

system.time( hist_centroids <- map_df (borm_spp$sci_name_underscore, hist_centroids_fun)); beep() # 98s

future_centroids_fun <- function (sci_name, CM, scenario) {
  # load raster brick for historical period
  pred_br <- brick (file.path ("Models/Prediction_bricks", 
                               paste(sci_name, "Borm_14_alltemp", CM, scenario, "2061_2080.grd", sep = "_")
                               )
                    )
  
  #take mean so have one value per period
  pred_mn <- calc (pred_br, mean) # calc is faster
  df_post <- data.frame(rasterToPoints(pred_mn))
  
  df_post_suit <- filter (df_post, layer > 0.05)
  
  # eventually want something about circular movement around iceland??
  # calculate mean lon and lat, store as data.frame to match with climate models
  # compile in dataframe
  CM_df <- data.frame (
    species = sci_name,
    model = CM, 
    scenario = scenario,
    pred_lat = weighted.mean(df_post$y, df_post$layer),
    pred_lon = weighted.mean(df_post$x, df_post$layer),
    pred_warm = quantile (df_post_suit$y,  probs = 0.05, na.rm = TRUE),
    pred_cold = quantile (df_post_suit$y,  probs = 0.95, na.rm = TRUE)
  )
}


# list of all the possible combinations of species, CM, and scenario. I couldn't figure out how to use cross and filter to get rid of the CM2.6 and 245 combination
CM_list <- c("gfdl", "cnrm", "ipsl", "mohc", "CM26")

cm_expand <- expand_grid (sci_name = borm_spp$sci_name_underscore,
                          CM = CM_list,
                          scenario = c(245, 585)) %>%
  filter (!(CM == "CM26" & scenario == 245)) %>% 
  as.list() # convert to list to feed to pmap

system.time( pred_centroids <- pmap_dfr (cm_expand, future_centroids_fun)); beep() # 382s landed, 1206.62  full. 923 secodn time

# combine and save 
# start with just centroids, do warm/cold later
centroid_change <- pred_centroids %>%
  left_join (hist_centroids, by = "species") %>%
  mutate (dist = distHaversine (cbind(hist_lon, hist_lat),
                                cbind(pred_lon, pred_lat)),
          bearing = bearing (cbind(hist_lon, hist_lat),
                             cbind(pred_lon, pred_lat)
                             ) # bearing is -180-180
  )

# save for now
save (centroid_change, file = "Data/centroids_Borm14_alltemp_allscenarios.RData")


# fancy mutate attempts...
# centroid_change <- pred_centroids %>%
#   pivot_longer (cols = starts_with ("pred"),
#                 names_prefix = "pred_", 
#                 names_to = "var",
#                 values_to = "pred") %>%
#   left_join (pivot_longer (hist_centroids, 
#                            cols = !species,
#                            names_prefix = "hist_",
#                            names_to = "var", 
#                            values_to = "hist"),
#              by = c("species", "var")) %>%
#   mutate ()


#################################################################################################

# load and plot ----

# spp I ran models for
load ("Models/spp_Smooth_latlon.RData") 
borm_spp <- spp_Smooth_latlon %>%
  filter (!sci_name_underscore == "Myoxocephalus_scorpius")


borm_MASE <- read_csv ("Models/GAM_performance_Borm_14_alltemp.csv")
spp_list  <- read_csv ("Data/species_eng.csv",
                       col_types = cols(
                         Spp_ID = col_factor()
                       )) %>%
  rename (species = sci_name_underscore) %>% #rename to match species column
  # only grab first option for species name (more legible graphs)
  # https://stackoverflow.com/questions/42565539/using-strsplit-and-subset-in-dplyr-and-mutate
  mutate(Common_name = sapply(str_split(Common_name, ","), function(x) x[1]))


load ("Data/centroids_Borm14_alltemp_allscenarios.RData")

centroid_mean_change <- centroid_change %>%
  group_by (species, scenario) %>%
  summarise (mean_dist = mean(dist),
             sd_dist = sd (dist),
             mean_bearing = mean(bearing),
             sd_bearing = sd(bearing)
  ) %>%
  left_join (borm_MASE, by = "species") %>%
  left_join (spp_list, by = "species") %>%
  # this replaces apostrophes with chinese characters??
  #https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
  #mutate (Common_name = strsplit (Common_name, ",")[[1]][1]) %>%
  # just cut out not suitable
  filter (MASE_GAM < 1 & DM_GAM_p < 0.05) %>%
  mutate (
    Therm_pref = case_when (
      mean_TB > 0 ~ "Warm",
      between (mean_TB, -3, 0) ~ "Cool",
      mean_TB < 0 ~ "Cold"
    ),
    scen_long = case_when (
      scenario == 245 ~ "Optimistic scenario",
      scenario == 585 ~ "Worst case scenario"
    ),
    # convert bearing to 0-360
    bearing = ifelse (mean_bearing < 0, mean_bearing + 360, mean_bearing))

# not seeing much of a pattern. is there a difference in circular average?
# overall CA
cir_avg <- centroid_mean_change %>%
  mutate (dist_km = mean_dist/1000) %>%
  group_by (scenario, Therm_pref) %>%
  summarize (CA = circular.averaging (direction = bearing),
             dist = mean (dist_km))

png ("Figures/Compass_Borm14alltemp_TB_commnames.png", width = 16, height = 9, units = "in", res = 300)
set.seed(15)

centroid_mean_change %>%
  mutate (dist_km = mean_dist/1000) %>%
  #mutate (Common_name = str_replace_all(Common_name, "[^[:alnum:]]", " ")) %>%
  # ggplot() +aes (x = bearing,
  #             y = dist_km, 
  #             colour = Therm_pref,
  #             label = species)) +
  ggplot() +
  coord_polar(start = 0) +
  geom_segment(aes (y = 0,x = bearing,
                    xend = bearing, 
                    yend = dist_km,
                    col = Therm_pref
  ), 
  size = 1.2,
  alpha = 0.7
  ) +  
  geom_text(aes(label = Common_name,#species, 
                x = bearing,
                y = 150,  

                # not sure what angle is doing here but it seems right
                angle = ifelse (bearing < 180,
                                -bearing + 90,
                                -bearing + 270)
                ),
            col = "black",
            alpha = 0.7,
           # position = position_jitter(width = 4, height = 5),
            size = 3.5, 
            vjust = 0.1) +
  # add circular average?
  geom_point (aes (x = CA, y = dist, col = Therm_pref), data = cir_avg, shape = 24, size = 5) +
  facet_wrap (~ scenario) +
  labs(y= "Distance (km)") + 
  # scale_x_continuous(breaks= c(22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5), limits = c(0,360)) + 
  scale_x_continuous(breaks= c(0, 90, 180, 270), limits = c(0,360)) + 
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  labs (color = "") + 
  theme_bw() +
  theme (
    axis.text.x = element_text (size = 18),
    axis.text.y = element_text (size = 18),
    axis.title = element_text (size = 20),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 20),
    legend.text = element_text (size = 20),
  ) +
  ggtitle ("Centroid shift bearing and distance, 2000-2018 vs. 2060-2081")

dev.off()




# plot just 585 with depth, TB, steno, like campana figure 5----
steno_cir_avg <- centroid_mean_change %>%
  mutate (dist_km = mean_dist/1000,
           Sten_cat = case_when (
              mean_Steno > 0 ~ "Warm",
              between (mean_Steno, -3, 0) ~ "Cool",
              mean_Steno < 0 ~ "Cold"
            )) %>%
  group_by (scenario, Therm_pref) %>%
  summarize (CA = circular.averaging (direction = bearing),
             dist = mean (dist_km))
png ("Figures/Compass_Borm14alltemp_steno_585_commnames.png", width = 8, height = 9, units = "in", res = 300)
set.seed(15)

centroid_mean_change %>%
  filter (scenario == 585) %>%
  mutate (dist_km = mean_dist/1000) %>%
  
  ggplot(aes (x = bearing,
              y = mean_dist, 
              colour = mean_Steno,
              label = species)) +
  coord_polar(start = 0) +
  geom_segment(aes (y = 0,
                    xend = bearing, 
                    yend = dist_km), 
               size = 1.2) +  
  # geom_text(aes(label = Common_name,#species, 
  #               x = bearing,
  #               y = 150000,  
  #               
  #               # not sure what angle is doing here but it seems right
  #               angle = ifelse (bearing < 180,
  #                               -bearing + 90,
  #                               -bearing + 270)
  #               ),
  #           col = "black",
  #           position = position_jitter(width = 0.5, height = 50),
  #           size = 3.5, 
  #           vjust = 0.1) +

  labs(y = "", x = "") +

  scale_x_continuous(breaks= c(0, 90, 180, 270), limits = c(0,360)) + 
  scale_color_binned (breaks = c (2, 4, 6), low = "orange", high = "blue" ) +

  labs (color = "Stenothermal Index") + 
  theme_bw() +
  theme (
    axis.text.x = element_text (size = 18),
    axis.text.y = element_text (size = 18),
    axis.title = element_text (size = 20),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 20),
    legend.text = element_text (size = 18),
    legend.title = element_text (size = 20),
    legend.position = c(0.75, 0.25)
  
  ) 
dev.off()


png ("Figures/Compass_Borm14alltemp_depth_585_commnames.png", width = 8, height = 9, units = "in", res = 300)
set.seed(15)

centroid_mean_change %>%
  filter (scenario == 585) %>%
  mutate (dist_km = mean_dist/1000) %>%
  
  ggplot(aes (x = bearing,
              y = mean_dist, 
              colour = mean_depth,
              label = species)) +
  coord_polar(start = 0) +
  geom_segment(aes (y = 0,
                    xend = bearing, 
                    yend = dist_km), 
               size = 1.2) +  
  # geom_text(aes(label = Common_name,#species, 
  #               x = bearing,
  #               y = 150000,  
  #               
  #               # not sure what angle is doing here but it seems right
  #               angle = ifelse (bearing < 180,
  #                               -bearing + 90,
  #                               -bearing + 270)
  # ),
  # col = "black",
  # position = position_jitter(width = 0.5, height = 50),
  # size = 3.5, 
  # vjust = 0.1) +
  
  labs(y = "", x = "") + 
  scale_x_continuous(breaks= c(0, 90, 180, 270), limits = c(0,360)) + 
  scale_color_binned (breaks = c (200, 400), low = "lightblue", high = "darkblue" ) +
  
  labs (color = "Depth") + 
  theme_bw() +
  theme (
    axis.text.x = element_text (size = 18),
    axis.text.y = element_text (size = 18),
    axis.title = element_text (size = 20),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 20),
    legend.text = element_text (size = 18),
    legend.title = element_text (size = 20),
    legend.position =  c(0.75, 0.25)
  ) 
dev.off()


#######################


#################################################################################################

# https://eliocamp.github.io/codigo-r/2018/09/multiple-color-and-fill-scales-with-ggplot2/
new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}
library(viridis)

png ("Figures/Compass_Borm14alltemp_Steno.png", width = 16, height = 9, units = "in", res = 300)

centroids_df %>%
  left_join (borm_MASE, by = "species") %>%
  left_join (spp_list, by = "species") %>%
  # just cut out not suitable
  filter (MASE_GAM < 1 & DM_GAM_p < 0.05) %>%
  mutate (
    Therm_pref = case_when (
      mean_TB > 0 ~ "Warm",
      between (mean_TB, -3, 0) ~ "Cool",
      mean_TB < 0 ~ "Cold"
    ),
    # convert bearing to 0-360
    bearing = ifelse (bearing < 0, bearing + 360, bearing)) %>%
  #filter (dist < 100) %>%
  ggplot(aes (x = bearing,
              y = dist, 

              label = species)) +
  coord_polar(start = 0) +
  geom_segment(aes (y = 0,
                    #colour = mean_Steno,
                    xend = bearing, 
                    yend = dist
  ), 
  size = 1.2) + 
  scale_color_viridis_c ("Stenothermal index") +
  new_scale("color") +
  geom_text(aes(label = species, 
                x = bearing,
                y = 200,  
                col = Therm_pref,
                # not sure what angle is doing here but it seems right
                angle = ifelse (bearing < 180,
                                -bearing + 90,
                                -bearing + 270
                )
  ),
  #col = "black",
  size = 3.5, 
  vjust = 0.1) +
  facet_wrap (~ scenario) +
  labs(y= "Maximum distance/decade (km)") + #, 
  #title=paste("Spring, North, Circular Average = ", 
  #round(CA[1], 2))) +
  scale_x_continuous(breaks= c(22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5), limits = c(0,360)) + 
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  labs (color = "") + 
  theme_bw() +
  
  theme(plot.title = element_text(size = rel(1.5)), 
        axis.title = element_text(size = rel(1.25)), 
        legend.text = element_text(size = rel(1.25) ), 
        axis.text = element_text(size = rel(1)), 
        legend.title= element_text(size = rel(1.25) )) +
  ggtitle ("Centroid shift bearing and distance, 2000-2018 vs. 2060-2081")
dev.off()



# Smooth_latlon 2000-2018 and 2061-2080 ----

# load smooth_latlon spp
load ("Models/spp_Smooth_latlon.RData")

# diagnostics to designate based on certainty ----
smoothLL_MASE <- read_csv ("Models/GAM_diagnostics_all_spp.csv") %>%
  rename (species = Species)

# quota status ----
quota_status <- read_csv ("Data/species_eng.csv") %>%
  select (sci_name_underscore, Quota) %>%
  rename (species = sci_name_underscore)

# circular average ----
# this is from Kristin's code

# overall CA
centroids_df %>%
  group_by (scenario) %>%
  summarize (CA = circular.averaging (direction = bearing)) # seems to figure out 0-360 automatically?



## Code test with simulated data----
# generate random simulated data for 10 species
test_df <- data.frame (
  dist = runif (10, 25, 100),
  bearing = runif (10, 0, 360),
  spp = spp_list$Common_name[1:10],
  group = as.factor(sample (1:3, 10, replace = T))
)


# code from Kristin's PlosOne paper to do this with ggplot
# Kristin code


CA <- circular.averaging(direction=test_df$bearing) # will need to find some other way of calculating circular average

library (tidyverse)
# use geom_segment, not geom_line
# https://stackoverflow.com/questions/10515703/ggplot2-polar-plot-arrows

ggplot(test_df,
       aes (x = bearing,
            y = dist, 
            #group = spp, 
            colour = group), 
       label = spp
) +
  coord_polar(start=0) +
  geom_segment(aes (y = 0,
                    xend = bearing, 
                    yend = dist
  ), 
  size=1.2) +  
  geom_text(aes(label = spp, 
                x = bearing,
                y = 100,  
                # not sure what angle is doing here but it seems right
                angle = ifelse (bearing < 180,
                                -bearing + 90,
                                -bearing + 270
                )
  ),
  size=5, 
  vjust=0.1) +
  labs(y= "Maximum distance/decade (km)") + #, 
  #title=paste("Spring, North, Circular Average = ", 
  #round(CA[1], 2))) +
  scale_x_continuous(breaks= c(22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5), limits = c(0,360)) + 
  theme_bw() +
  
  theme(plot.title = element_text(size = rel(1.5)), 
        axis.title = element_text(size = rel(1.25)), 
        legend.text = element_text(size = rel(1.25) ), 
        axis.text = element_text(size = rel(1.5)), 
        legend.title= element_text(size = rel(1.25) ))#+ 
#scale_color_manual(values = colorit_cluster)