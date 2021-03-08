#Radial compass plots of centroid distribution distance and bearing change
# Figure 5 in manuscript

library (tidyverse)


# Load centroid df from Step4_Calculate_centroid_change.R
load ("Data/centroids_Borm14_alltemp_allscenarios.RData")

# load 47 suitable species
load ("Models/spp_Borm_suit.RData")

centroid_mean_change <- centroid_change %>%
  filter (species %in% borm_suit) %>%
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

png ("Figures/Fig5_Compass_TB_nolab.png", width = 170, height = 350, units = "mm", res = 500)
set.seed(15)
print (fig5_compass)
dev.off()

ggsave ("Figures/Fig5_Compass_TB_nolab.eps", fig5_compass, width = 170, height = 100, units = "mm", dpi = 300)

# split into TB, with labels, for supplement ---
# eps ("Figures/Fig5SI_Compass_TB_facet_labels.eps", width = 6.5, height = 9, units = "in", res = 500)

set.seed(15)

SI_labels <- centroid_mean_change %>%
  mutate (dist_km = mean_dist/1000,
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
                y = 120,
                
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
  ggtitle ("Centroid shift bearing and distance, 2000-2018 vs. 2060-2081, by thermal preference")

ggsave ("Figures/FigSI3_Compass_TB_facet_labels.pdf", width = 170, height = 220, units = "mm", dpi = 300)


###########################################################################
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




#################################################################################################

# try with two different scales??

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