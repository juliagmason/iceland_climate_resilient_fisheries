# Calculate centroid change rug
# 6/9/21
# JGM

## Did this on CISER; don't have prediction bricks on local machine yet

library (geosphere) # for distHaversine and bearing
library (raster)
library (tidyverse)
library (beepr) # for alerting me when code is done
library (SDMTools) # for calculating circular average. Not available for R version 4.0.2, downloaded from https://www.rforge.net/SDMTools/git.html
# install.packages('SDMTools',,'http://www.rforge.net/')
#library (spatstat) # for weighted quantiles

spp_list  <- read_csv ("Data/species_eng.csv",
                       col_types = cols(
                         Spp_ID = col_factor()
                       )) %>%
  rename (species = sci_name_underscore) %>% #rename to match species column
  # Common names have several options separated by commas, only grab first option for more legible graphs
  # https://stackoverflow.com/questions/42565539/using-strsplit-and-subset-in-dplyr-and-mutate
  mutate(Common_name = sapply(str_split(Common_name, ","), function(x) x[1]), 
         Common_name = gsub ("Atlantic", "Atl", Common_name))


################################################################
# Calculate change in centroid of distribution ----
# Code from Morely et al. 2018, calculating % habitat change as change in sum of prediction values

# trying something different, with purr. Still think it's faster to separate historical and future
hist_centroids_fun <- function (sci_name) {
  # load raster brick for historical period
  if (sci_name %in% c("Molva_molva", "Hippoglossoides_platessoides")) {
    hist_br <- brick (file.path ("Models/Prediction_bricks", 
                                 paste0("Rug_nb_depth_crop", sci_name, "_Rug_nb_", "2000_2018.grd"))
    )
  } else {
    
    hist_br <- brick (file.path ("Models/Prediction_bricks", 
                                 paste(sci_name, "Rug_nb", "2000_2018.grd", sep = "_"))
    )
    
  }
  
  
  # take mean so have one value per period
  hist_mn <- calc (hist_br, median) # calc is faster than StackApply
  
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


# load("Models/nb_rug_stats.RData")
# rug_suit <- rug_nb_df %>%
#   filter (MASE_suit == 1)

load ("Models/spp_Smooth_latlon.RData")
borm_spp <- filter (spp_Smooth_latlon, 
                    ! sci_name_underscore %in% c("Myoxocephalus_scorpius", "Amblyraja_hyperborea", "Rajella_fyllae,", "Lumpenus_lampretaeformis", "Ammodytes_marinus", "Mallotus_villosus", "Micromesistius_poutassou", "Argentina_silus", "Scomber_scombrus", "Clupea_harengus", "Squid"))

system.time( hist_centroids <- map_df (borm_spp$sci_name_underscore, hist_centroids_fun)); beep() # 114s

future_centroids_fun <- function (sci_name, CM, scenario) {
  # load raster brick for historical period
  if (sci_name %in% c("Molva_molva", "Hippoglossoides_platessoides")) {
    pred_br <- brick (file.path ("Models/Prediction_bricks", 
                                 paste0("Rug_nb_depth_crop", sci_name, "_Rug_nb_", CM, "_", scenario, "_2061_2080.grd")
    )
    )

  } else {
    
    pred_br <- brick (file.path ("Models/Prediction_bricks", 
                                 paste(sci_name, "Rug_nb", CM, scenario, "2061_2080.grd", sep = "_")
                                 )
    )
    
  }
  

  
  #take mean so have one value per period
  pred_mn <- calc (pred_br, median) # calc is faster
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

system.time( pred_centroids <- pmap_dfr (cm_expand, future_centroids_fun)); beep() # 1066.70 

# combine and save 
# start with just centroids, do warm/cold later
centroid_change_dc <- pred_centroids %>%
  left_join (hist_centroids, by = "species") %>%
  mutate (dist = distHaversine (cbind(hist_lon, hist_lat),
                                cbind(pred_lon, pred_lat)),
          bearing = bearing (cbind(hist_lon, hist_lat),
                             cbind(pred_lon, pred_lat)
          ) # bearing is -180-180
  )

# save for now
save (centroid_change_dc, file = "Data/centroids_Rug_allscenarios_DepthCrop.RData")

#####################################################################################################
load ("Data/centroids_Rug_allscenarios_DepthCrop.RData")
# doesn't have squid but does have myctophidae??

# remove unsuitable species
centroid_change_dc <- centroid_change_dc %>%
  filter (!species %in% c("Squid", "Myctophidae", "Squalus_acanthias"))
# Plot
centroid_mean_change <- centroid_change_dc %>%
  #filter (species %in% rug_suit$species) %>%
  group_by (species, scenario) %>%
  summarise (mean_dist = mean(dist),
             sd_dist = sd (dist),
             mean_bearing = mean(bearing),
             sd_bearing = sd(bearing)
  ) %>%
  left_join (spp_list, by = "species") %>%
  # characterize thermal bias as 3 broad niches from Campana et al. 2020
  mutate (
    Therm_pref = case_when (
      mean_TB > 0 ~ "Warm",
      between (mean_TB, -3, 0) ~ "Cool",
      mean_TB < 0 ~ "Cold"
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

# scenario names for labeller
scen_labs <- setNames ( c("SSP 2-4.5", "SSP 5-8.5"), c(245, 585))

# plot with no species labels ----
fig5_compass <- centroid_mean_change %>%
  mutate (dist_km = mean_dist/1000) %>%
  
  ggplot() +
  coord_polar(start = 0) +
  geom_segment(aes (y = 0,x = bearing,
                    xend = bearing, 
                    yend = dist_km,
                    col = Therm_pref
  ), 
  size = 1.2#,
  #alpha = 0.7
  ) +
# geom_text(aes(label = Common_name,#species,
#               x = bearing,
#               #y = ifelse (dist_km < 100, 180, dist_km + 150),
#               y = 180,
# 
#               # not sure what angle is doing here but it seems right
#               angle = ifelse (bearing < 180,
#                               -bearing + 90,
#                               -bearing + 270),
#               col = Therm_pref),
#           #col = "black",
#           alpha = 0.7,
#          # position = position_jitter(width = 4, height = 5),
#           size = 2.5,
#           vjust = 0.1) +
# add circular average?
geom_point (aes (x = CA, y = dist, col = Therm_pref), data = cir_avg, shape = 24, size = 5) +
  facet_wrap (~ scenario, labeller = labeller (scenario = scen_labs)) +
  labs(y= "Distance (km)") + 
  # scale_x_continuous(breaks= c(22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5), limits = c(0,360)) + 
  scale_x_continuous(breaks= c(0, 90, 180, 270), limits = c(0,360)) + 
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  labs (color = "", x ="Bearing") + 
  theme_bw() +
  theme (
    axis.text.x = element_text (size = 12),
    axis.text.y = element_text (size = 12),
    axis.title = element_text (size = 14),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 14),
    legend.text = element_text (size = 12)
  ) 
#ggtitle ("Centroid shift bearing and distance, 2000-2018 vs. 2060-2081")

png ("Figures/Fig5_Compass_Rug_nolab.png", width = 16, height = 9, units = "mm", res = 500)
set.seed(15)
print (fig5_compass)
dev.off()

png ("Figures/Fig5_Compass_Rug.png", width = 16, height = 9, units = "in", res = 300)
set.seed(15)
print (fig5_compass)
dev.off()


SI_labels <- centroid_mean_change %>%
  mutate (dist_km = mean_dist/1000,
          Common_name = case_when (
            Common_name == "Lycodes eudipleurostictus" ~ "Doubleline eelpout",
            Common_name == "Lycodes seminudus" ~ "Longear eelpout",
            Common_name == "Rabbitfish (rat fish)" ~ "Rabbitfish",
            TRUE ~ Common_name
          )) %>%
  
  ggplot() +
  coord_polar(start = 0) +
  geom_segment(aes (y = 0,x = bearing,
                    xend = bearing, 
                    yend = dist_km,
                    col = Therm_pref), 
               size = 1,
               alpha = 0.7
  ) +  
  geom_text(aes(label = Common_name,#species,
                x = bearing,
                y = 180,
                
                # not sure what angle is doing here but it seems right
                angle = ifelse (bearing < 180,
                                -bearing + 90,
                                -bearing + 270)),
            col = "black",
            alpha = 0.7,
            #position = position_jitter(width = 4, height = 5),
            size = 2,
            vjust = 0.1) +
  #   # add circular average?
   geom_point (aes (x = CA, y = dist, col = Therm_pref), data = cir_avg, shape = 24, size = 5) +
  facet_grid (Therm_pref ~ scenario, labeller = labeller (scenario = scen_labs)) +
  labs(y= "Distance (km)") + 
  # scale_x_continuous(breaks= c(22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5), limits = c(0,360)) + 
  scale_x_continuous(breaks= c(0, 90, 180, 270), limits = c(0,360)) + 
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  labs (color = "", x ="Bearing") + 
  theme_bw() +
  theme (
    axis.text.x = element_text (size = 12),
    axis.text.y = element_text (size = 12),
    axis.title = element_text (size = 14),
    plot.title = element_text (size = 16),
    strip.text = element_text (size = 14),
    legend.position = "none"
  ) +
  ggtitle ("Centroid shift bearing and distance, 2000-2018 vs. 2060-2081, by thermal preference")

ggsave ("Figures/FigSI3_Compass_Rug_facet_labels.png", width = 170, height = 220, units = "mm", dpi = 300)


# plot max predicted centroid change. this is only distance, how could i do a wedge for bearing?
# Plot
centroid_max_change <- centroid_change_dc %>%
  #filter (species %in% rug_suit$species) %>%
  group_by (species, scenario) %>%
  summarise (max_dist = max(dist),
             sd_dist = sd (dist),
             mean_bearing = mean(bearing),
             sd_bearing = sd(bearing)
  ) %>%
  left_join (spp_list, by = "species") %>%
  # characterize thermal bias as 3 broad niches from Campana et al. 2020
  mutate (
    Therm_pref = case_when (
      mean_TB > 0 ~ "Warm",
      between (mean_TB, -3, 0) ~ "Cool",
      mean_TB < 0 ~ "Cold"
    ),
    # convert bearing to 0-360
    bearing = ifelse (mean_bearing < 0, mean_bearing + 360, mean_bearing))


SI_labels_max <- centroid_max_change %>%
  mutate (dist_km = max_dist/1000,
          Common_name = case_when (
            Common_name == "Lycodes eudipleurostictus" ~ "L. eudipleur.",
            Common_name == "Lycodes seminudus" ~ "L. seminudus",
            Common_name == "Rabbitfish (rat fish)" ~ "Rabbitfish",
            TRUE ~ Common_name
          )) %>%
  
  ggplot() +
  coord_polar(start = 0) +
  geom_segment(aes (y = 0,x = bearing,
                    xend = bearing, 
                    yend = dist_km,
                    col = Therm_pref), 
               size = 1,
               alpha = 0.7
  ) +  
  geom_text(aes(label = Common_name,#species,
                x = bearing,
                y = 300,

                # not sure what angle is doing here but it seems right
                angle = ifelse (bearing < 180,
                                -bearing + 90,
                                -bearing + 270)),
            col = "black",
            alpha = 0.7,
            #position = position_jitter(width = 4, height = 5),
            size = 2,
            vjust = 0.1) +
  #   # add circular average?
  # geom_point (aes (x = CA, y = dist, col = Therm_pref), data = cir_avg, shape = 24, size = 5) +
  facet_grid (Therm_pref ~ scenario, labeller = labeller (scenario = scen_labs)) +
  labs(y= "Distance (km)") + 
  # scale_x_continuous(breaks= c(22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5), limits = c(0,360)) + 
  scale_x_continuous(breaks= c(0, 90, 180, 270), limits = c(0,360)) + 
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  labs (color = "", x ="Bearing") + 
  theme_bw() +
  theme (
    axis.text.x = element_text (size = 12),
    axis.text.y = element_text (size = 12),
    axis.title = element_text (size = 14),
    plot.title = element_text (size = 16),
    strip.text = element_text (size = 14),
    legend.position = "none"
  ) +
  ggtitle ("Max centroid shift distance")+
  ylim(0, 400)

centroid_min_change <- centroid_change_dc %>%
  #filter (species %in% rug_suit$species) %>%
  group_by (species, scenario) %>%
  summarise (min_dist = min(dist),
             sd_dist = sd (dist),
             mean_bearing = mean(bearing),
             sd_bearing = sd(bearing)
  ) %>%
  left_join (spp_list, by = "species") %>%
  # characterize thermal bias as 3 broad niches from Campana et al. 2020
  mutate (
    Therm_pref = case_when (
      mean_TB > 0 ~ "Warm",
      between (mean_TB, -3, 0) ~ "Cool",
      mean_TB < 0 ~ "Cold"
    ),
    # convert bearing to 0-360
    bearing = ifelse (mean_bearing < 0, mean_bearing + 360, mean_bearing))


SI_labels_min <- centroid_min_change %>%
  mutate (dist_km = min_dist/1000,
          Common_name = case_when (
            Common_name == "Lycodes eudipleurostictus" ~ "L. eudipleur.",
            Common_name == "Lycodes seminudus" ~ "L. seminudus",
            Common_name == "Rabbitfish (rat fish)" ~ "Rabbitfish",
            TRUE ~ Common_name
          )) %>%
  
  ggplot() +
  coord_polar(start = 0) +
  geom_segment(aes (y = 0,x = bearing,
                    xend = bearing, 
                    yend = dist_km,
                    col = Therm_pref 
                    ),
               size = 1,
               alpha = 0.7,
               
  ) +  
  geom_text(aes(label = Common_name,#species,
                x = bearing,
                y = 280,
                
                # not sure what angle is doing here but it seems right
                angle = ifelse (bearing < 180,
                                -bearing + 90,
                                -bearing + 270)),
            col = "black",
            alpha = 0.7,
            #position = position_jitter(width = 4, height = 5),
            size = 2,
            vjust = 0.1) +
  #   # add circular average?
  # geom_point (aes (x = CA, y = dist, col = Therm_pref), data = cir_avg, shape = 24, size = 5) +
  facet_grid (Therm_pref ~ scenario, labeller = labeller (scenario = scen_labs)) +
  labs(y= "Distance (km)") + 
  # scale_x_continuous(breaks= c(22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5), limits = c(0,360)) + 
  scale_x_continuous(breaks= c(0, 90, 180, 270), limits = c(0,360)) + 
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  labs (color = "", x ="Bearing") + 
  theme_bw() +
  theme (
    axis.text.x = element_text (size = 12),
    axis.text.y = element_text (size = 12),
    axis.title = element_text (size = 14),
    plot.title = element_text (size = 16),
    strip.text = element_text (size = 14),
    legend.position = "none"
  ) +
  ggtitle ("Min centroid shift distance") +
  ylim(0, 400)

library (gridExtra)
library (patchwork)

png ("Figures/FigSI3_Compass_Rug_facet_labels_MIN_MAX.png", width = 16, height = 9, units = "in", res = 300)
print(SI_labels_min + SI_labels_max )
dev.off()

# do a graph with segment at min and max? ----

cent_minmax <- centroid_change_dc %>%
  group_by (species, scenario) %>%
  summarise (max_dist = max(dist)/1000,
             min_dist = min(dist)/1000,
             med_dist = median(dist)/1000,
             mean_bearing = mean(bearing),
             max_bearing = max(bearing),
             min_bearing = min(bearing)
  ) %>%
  left_join (spp_list, by = "species") %>%
  # characterize thermal bias as 3 broad niches from Campana et al. 2020
  mutate (
    Therm_pref = case_when (
      mean_TB > 0 ~ "Warm",
      between (mean_TB, -3, 0) ~ "Cool",
      mean_TB < 0 ~ "Cold"
    ),
    # convert bearing to 0-360
    bearing = ifelse (mean_bearing < 0, mean_bearing + 360, mean_bearing))


SI_labels_minmax <- cent_minmax %>%
  mutate (
          Common_name = case_when (
            Common_name == "Lycodes eudipleurostictus" ~ "Doubleline eelpout",
            Common_name == "Lycodes seminudus" ~ "Longear eelpout",
            Common_name == "Rabbitfish (rat fish)" ~ "Rabbitfish",
            TRUE ~ Common_name
          )) %>%
  
  ggplot() +
  coord_polar(start = 0) +
  geom_segment(aes (y = min_dist,x = bearing,
                    xend = bearing, 
                    yend = max_dist,
                    col = Therm_pref 
  ),
  size = 0.7,
  alpha = 1,
  
  ) + 
  # geom_segment(aes (y = max_dist, x = min_bearing,
  #                   xend = max_bearing, 
  #                   yend = max_dist,
  #                   col = Therm_pref 
  # ),
  # size = 0.75,
  # alpha = 0.7,
  # lty = 3
  # 
  # ) + 
  geom_point (aes (y = med_dist, x = bearing, col = Therm_pref), pch = 16, size = 1) +
  geom_text(aes(label = Common_name,#species,
                x = bearing,
                y = 280,
                
                # not sure what angle is doing here but it seems right
                angle = ifelse (bearing < 180,
                                -bearing + 90,
                                -bearing + 270)),
            col = "black",
            alpha = 1,
            #position = position_jitter(width = 4, height = 5),
            size = 2,
            vjust = 0.1) +
  #   # add circular average
   geom_point (aes (x = CA, y = dist), data = cir_avg, shape = 17, size = 2, col = "black") +
  facet_grid (Therm_pref ~ scenario, labeller = labeller (scenario = scen_labs)) +
  labs(y= "Distance (km)") + 
  # scale_x_continuous(breaks= c(22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5), limits = c(0,360)) + 
  scale_x_continuous(breaks= c(0, 90, 180, 270), limits = c(0,360)) + 
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  labs (color = "", x ="Bearing") + 
  theme_bw() +
  theme (
    axis.text.x = element_text (size = 12),
    axis.text.y = element_text (size = 12),
    axis.title = element_text (size = 14),
    plot.title = element_text (size = 16),
    strip.text = element_text (size = 14),
    legend.position = "none"
  ) +
  ggtitle ("Centroid shift bearing and distance,\nmin and max") +
  ylim(0, 400)

ggsave ("Figures/Compass_Rug_facet_minmax.png", width = 170, height = 220, units = "mm", dpi = 300)
ggsave ("Figures/Compass_Rug_facet_minmax.eps", width = 170, height = 220, units = "mm", dpi = 300)
#################################################################################################

# statistical relationships btw therm pref and centroid change ----
load ("Data/centroids_Borm14_alltemp_allscenarios.RData")
load ("Models/spp_Borm_suit.RData")

# calculate distance between warm and cold edges too
edge_change_med <- centroid_change %>%
  filter (species %in% borm_suit) %>%
  mutate (warm_change = pred_warm - hist_warm,
          cold_change = pred_cold - hist_cold) %>%
  group_by (species, scenario) %>%
  summarize (med_warm = median (warm_change, na.rm = TRUE),
             med_cold = median (cold_change, na.rm = TRUE)) %>%
  left_join (spp_list, by = "species")

cent_change_mn <- centroid_change %>%
  filter (species %in% borm_suit) %>%
  group_by (species, scenario) %>%
  summarize (mean_dist = mean(dist),
             mean_bearing = mean(bearing)) %>%
  mutate (bearing_360 = ifelse (mean_bearing < 0,
                                mean_bearing + 360,
                                mean_bearing)) %>%
  left_join (spp_list, by = "species")


# is there a rltp between edge change and TB/steno?
summary (lm (med_cold ~ mean_TB, data = filter (edge_change_med, scenario == 245)))
# p = 0.5 for warm, but y = 0.09x + 0.03, adj r2 0.13, p = 0.02 for med_cold

summary (lm (med_cold ~ mean_TB, data = filter (edge_change_med, scenario == 585)))
# p = 0.08 for warm, y = 0.09x + 0.11, adj r2 = 0.19, p = 0.004 for cold

# this means that warmer species see their cold edge push upward. 

png ("Figures/scatter_TB_coldedge.png")
edge_change_med %>%
  ggplot (aes (x = mean_TB, y = med_cold)) +
  geom_point () +
  geom_text (aes(label = Common_name), data = filter (edge_change_med, abs (med_cold) > 0.5)) +
  geom_vline (xintercept = 0, lty = 2) +
  geom_hline (yintercept = 0, lty = 2) +
  geom_smooth (method = lm) +
  facet_wrap (~scenario) +
  theme_bw()

dev.off()

# would expect larger Steno to be smaller habitat change
summary (lm (med_cold ~ mean_Steno, data = filter (edge_change_med , scenario == 245)))
# warm, p = 0.6, cold p = 0.2 

summary (lm (med_cold ~ mean_Steno, data = filter (edge_change_med , scenario == 585)))
# warm p = 0.9, cold p = 0.3. not better with interaction or both terms. 

summary (lm (med_warm ~ mean_depth, data = filter (edge_change_med , scenario == 245)))
# ad 0.03, p 0.13 cold, ad -0.02, p 0.71 warm

summary (lm (med_warm ~ mean_depth, data = filter (edge_change_med , scenario == 585)))
# ad 0.03, p = 0.13 cold, ad -0.02, p = 0.60 warm

# plot amount of cold and warm change
centroid_change %>%
  filter (species %in% borm_suit) %>%
  mutate (warm_change = pred_warm - hist_warm,
          cold_change = pred_cold - hist_cold) %>%
  ggplot (aes (x = warm_change, y = fct_reorder(species, warm_change))) +
  geom_boxplot () +
  facet_wrap (~scenario) +
  theme_bw ()

cent_hab <- centroid_change %>%
  filter (species %in% borm_suit) %>%
  mutate (warm_change = pred_warm - hist_warm,
          cold_change = pred_cold - hist_cold) %>%
  left_join (hab_change, by = c ("species", "model", "scenario"))

cent_hab %>%
  ggplot (aes(x = log10_foldchange, y = warm_change, col = mean_TB)) +
  geom_point () +
  geom_smooth() +
  theme_bw()

cent_hab %>%
  ggplot (aes(x = log10_foldchange, y = cold_change, col = mean_TB)) +
  geom_point () +
  geom_smooth() +
  theme_bw()

# species that lost habitat were likely to see their warm edge increase. no rltp with species that incresed habitat. that means a northward movment, cropping bottom. 

# species that gained habitat see their cold edge increase, pushing upward. not a lot of change in species that lost habitat, except a cluster of 5. G argentatus and l. gracilis cnrm 245

summary (lm (warm_change ~ log10_foldchange, data = cent_hab))
# adj r2 0.14, y = 0.07x - 0.35 log change, p <<< 0

summary (lm (warm_change ~ log10_foldchange, data = filter(cent_hab, warm_change < 0)))
# sig, but positive rltp
summary (lm (warm_change ~ log10_foldchange, data = filter(cent_hab, warm_change > 0)))
# sig, but negative rltp


# centroid change
# neither distance nor bearing significant for steno, TB, depth
summary (lm (mean_bearing ~ mean_TB * mean_depth, data = filter (cent_change_mn, scenario == 245)))

# TB * depth is significant for bearing, not depth. adj r2 0.14. TB very negative, depth pos, interaction positive. means that the effect of TB on bearing change increases with unit increase in depth. deeper species see a more pronounced thermal relationship with bearing. 

# https://biologyforfun.wordpress.com/2014/04/08/interpreting-interaction-coefficient-in-r-part1-lm/

#This means that warmer species move more west (negative bearing, this is with -180-180 bearings), which isn't actually what I'm seeing in the compass plots. 
summary (lm (bearing_360 ~ mean_TB, data = filter (cent_change_mn, scenario == 245)))

# also has a negative rltp with 360 bearing??

# also, there's a significant negative rltp btween mean_TB and mean_depth

# # SDMTools patchstat test ----
library (SDMTools)

# https://rdrr.io/cran/SDMTools/man/PatchStat.html
# https://gis.stackexchange.com/questions/210149/patch-stat-r-for-loop-question

# take mean first? can it do a raster brick?
br_hist <- brick (file.path ("Models/Prediction_bricks", hist_files[i]))
br_245 <- brick (file.path ("Models/Prediction_bricks", pred_files_245[i]))
br_585 <- brick (file.path ("Models/Prediction_bricks", pred_files_585[i]))

hist_ps <- PatchStat (subset(br_hist, 1), latlon = TRUE) # works, but only gives me patchID

hist_ccl_test <- ConnCompLabel()



## Kristin rgeos method ----



# # Kleisner 2017 selects all points with values greater than one sd above the mean, then computes area of those kernels. but what mean?? For now, will take overall mean of historical period--just one value. calc is significantly faster than overlay for this (1.4 vs 9s for test brick). 
# 
# # rgeos gArea vs. geosphere areaPolygon??
# # https://gis.stackexchange.com/questions/264517/why-does-projection-not-affect-answer-calculated-by-rgeosgarea
# 
# hist_mn <- cellStats(calc (br, fun = mean), stat = "mean")
# hist_sd <- cellStats(calc (br, fun = mean), stat = "sd")
# 
# # suitable habitat value is mean + one sd
# suit_val <- hist_mn + hist_sd
# 
# # https://stackoverflow.com/questions/45428129/how-to-subset-a-raster-based-on-grid-cell-values
# 
# # convert brick to points df, subset values > suit_val
# hist_suit <- rasterToPolygons(br[[100]], fun = function (x){ x > suit_val})
# projection (hist_suit) <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
# 
# # http://taromieno.netlify.com/post/raster_to_polygons_2018/
# hist_suit_sp <- as (br, 'SpatialPolygonsDataFrame') # this works for brick, but can't do subset?
# 
# # https://www.rdocumentation.org/packages/taRifx/versions/1.0.3/topics/subsetSPDF
# x <- subsetSPDF (hist_suit_sp, x > suit_val)
# 
# # maybe this would be helpful?
# # https://cengel.github.io/R-spatial/spatialops.html
# 
# hist_area <- gArea (hist_suit) # this gives 10.7, no idea what the units are
# 
# library (geosphere)
# area_geosphere <- areaPolygon(hist_suit)/1e6 # this gives 174 values (for 174 cells > suit_val)
# 
# # just record number of cells above suit val for now?
# dim(hist_suit)[1]
# 
# 
# 
# # get values of each layer of brick
# hist_vals <- getValues (br[[1]]) # 33600
# length (which (hist_vals > suit_val))


