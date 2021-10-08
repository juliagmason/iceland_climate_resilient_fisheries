#Radial compass plots of centroid distribution distance and bearing change
# Figure 5 in manuscript

library (tidyverse)
library (SDMTools) # for calculating circular average. Not available for R version 4.0.2, downloaded from https://www.rforge.net/SDMTools/git.html


spp_list  <- read_csv ("Data/species_eng.csv",
                       col_types = cols(
                         Spp_ID = col_factor()
                       )) %>%
  rename (species = sci_name_underscore) %>% #rename to match species column
  # Common names have several options separated by commas, only grab first option for more legible graphs
  # https://stackoverflow.com/questions/42565539/using-strsplit-and-subset-in-dplyr-and-mutate
  mutate(Common_name = sapply(str_split(Common_name, ","), function(x) x[1]), 
         Common_name = gsub ("Atlantic", "Atl", Common_name))


# Load centroid df from Step4_Calculate_centroid_change.R
load ("Data/centroids_Rug_allscenarios_tw.RData")

# take mean distance and bearing among the climate models
centroid_mean_change <- centroid_change_dc %>%
  # remove species deemed unsuitable for temperature predictions
  filter (!species %in% c("Squalus_acanthias", "Icelus_bicornis")) %>%
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

# overall circular average
cir_avg <- centroid_mean_change %>%
  mutate (dist_km = mean_dist/1000) %>%
  group_by (scenario, Therm_pref) %>%
  summarize (CA = circular.averaging (direction = bearing),
             dist = mean (dist_km))

# scenario names for labeller
scen_labs <- setNames ( c("SSP 2-4.5", "SSP 5-8.5"), c(245, 585))

# calculate minimum and maximum projected distance shift
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



cent_labels_minmax <- cent_minmax %>%
  mutate (
    # further shorten names for readability
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

  geom_point (aes (y = med_dist, x = bearing, col = Therm_pref), pch = 16, size = 1) +
  geom_text(aes(label = Common_name,#species,
                x = bearing,
                y = 280,
                
                # not sure what angle is doing here but it seems to be working
                angle = ifelse (bearing < 180,
                                -bearing + 90,
                                -bearing + 270)),
            col = "black",
            #alpha = 1,
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
  #ggtitle ("Centroid shift bearing and distance,\nmin and max") +
  ylim(0, 400)

ggsave ("Figures/Compass_Rug_facet_minmax.png", width = 170, height = 220, units = "mm", dpi = 300)
ggsave ("Figures/Compass_Rug_facet_minmax.eps", width = 170, height = 220, units = "mm", dpi = 300)
