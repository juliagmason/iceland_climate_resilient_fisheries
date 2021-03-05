## Plot habitat change vs. amount landed

# 9/25/2020
# JGM

library (tidyverse)
library (ggrepel) # for cleaner geom_text labels

# spp list to match landed and sci name
spp_list <- read.csv("Data/species_eng.csv")

# habitat change data. both saved as hab_change
load ("Data/pred_hist_hab_df.RData")

# this has quota type and year, tonnes only
land_ds <- read.csv("Data/Landings_annual_all_sectors.csv")

# this is community quota but I can't figure out where I got it....
land_cq <- read.csv ("Data/comm_quota_landings_annual.csv")



# check most important species overall, by tonnes
land_spp <- land_ds %>%
  filter (year > 2010) %>%
  group_by (species) %>%
  summarize (mn_cat = mean (tonnes, na.rm = TRUE),
             sd_cat = sd(tonnes, na.rm = TRUE)) %>%
  arrange (desc (mn_cat))


cq_spp <- land_cq %>%
  filter (year > 2010) %>%
  group_by (Species) %>%
  summarize (mn_cat = mean (tonnes, na.rm = TRUE),
             sd_cat = sd(tonnes, na.rm = TRUE)) %>%
  arrange (desc (mn_cat))


# divide out by sectors ----

# this is all quota types, all species, with tonnes and value
lands_qu <- read.csv("Data/Landings_annual_spp_quota.csv")

# combine hook and line with small boats, per carothers 2017. also going to combine trawlers with large ITQ?
spp_by_qu <- lands_qu %>%
  filter (year >= 2010) %>%
  mutate (Quota.class = case_when (
    Quota.class == "Hook and line boats with catch quota" ~ "Small boats with catch quota", 
    Quota.class == "Trawlers" ~ "Vessels with catch quota",
    TRUE ~ Quota.class)
  ) %>%
  group_by (Quota.class, Species, metric) %>%
  summarize (mn = mean (amount, na.rm = TRUE),
             sd = sd(amount, na.rm = TRUE)) %>% 
  pivot_wider (names_from = metric,
               values_from = c(mn, sd)
  ) %>%
  filter (! is.na (mn_Tonnes)) %>%
  rename (Landed_name = Species) %>% # match to spp list
  left_join (spp_list, by = "Landed_name") #%>%
  rename (species = sci_name_underscore) # match to hab change
#arrange (desc (mn_cat))

# shortened quota names
# https://www.datanovia.com/en/blog/how-to-change-ggplot-facet-labels/#:~:text=(dose%20~%20supp)-,Change%20the%20text%20of%20facet%20labels,labeller%20function%20label_both%20is%20used.
quot_labs <- c("Coastal fisheries", "Small boat ITQ", "Large boat ITQ")
names (quot_labs) <- c("Coastal fisheries", "Small boats with catch quota", "Vessels with catch quota")


# plot sector volume/value vs habitat change----

# borm 14
borm_MASE <- read_csv ("Models/GAM_performance_Borm_14_alltemp.csv")

#load ("Data/perc_hab_change_Morely_Borm_14_alltemp.RData")

# define outside so I can selectively label points
hab_change_by_sector <-pred_hist_hab %>% 
  mutate (perc_change = (pred_mean - hist_mean) / hist_mean * 100) %>%
  group_by (species, scenario) %>%
  summarise (mn_change = mean(perc_change),
             sd_change = sd (perc_change)) %>%
  right_join (spp_by_qu, by = "species") %>%
  filter (scenario ==585, mn_Tonnes > 0)


png (file = "Figures/Percent_hab_change_by_sector_importance_Borm_14.png", width = 16, height = 9, units = "in", res = 300)

  ggplot (hab_change_by_sector,
          aes (x = log(mn_Tonnes),
               y = mn_change)
          ) +
  geom_point (aes(size = mn_X1000.ISK)) +
    geom_text_repel (aes (label = Common_name)) +
  # geom_text_repel (data = subset(hab_change_by_sector, 
  #                                mn_X1000.ISK > 300000 | abs(mn_change) > 100 | (Quota.class == "Coastal fisheries") | (Quota.class == "Small boats with catch quota" & mn_X1000.ISK > 50000) | (Quota.class == "Small boats with catch quota" & mn_change > 50) | (Quota.class == "Small boats with catch quota" & mn_change > 0)),
  #            aes (label = Common_name)) +

  facet_grid (scenario ~ Quota.class,
              labeller = labeller (Quota.class = quot_labs)) +
  geom_hline (yintercept = 0, lty = 2) +
  geom_vline (xintercept = 0, lty = 2) +
  theme_bw () +
  labs (x = "log (mean annual tonnes landed)",
        y = "Percent habitat change",
        size = "Mean annual value, \n1000 ISK") +
  ggtitle ("Percent habitat change by fisheries sector") +
  theme (
    axis.text = element_text (size = 16),
    axis.title = element_text (size = 18),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 18),
    legend.text = element_text (size = 16),
    legend.title = element_text (size = 18)
  )
dev.off()


# SLL ----
# fill based on certainty
smoothLL_MASE <- read_csv ("Models/GAM_diagnostics_all_spp.csv") %>%
  rename (species = Species)  %>%
  mutate (Model_suitable = ifelse (
    MASE_GAM < 1 & DM_GAM_p < 0.05, 
    "1", 
    "0"
  ))

png (file = "Figures/Percent_hab_change_by_overall_value_SmoothLatlon.png", width = 16, height = 9, units = "in", res = 300)
lands_qu %>%
  filter (year > 2010) %>%
  group_by (Species) %>%
  filter (metric == "X1000.ISK") %>% 
  summarize (mn_val = mean (amount/1000, na.rm = TRUE),
             sd_val = mean(amount/1000, na.rm = TRUE)) %>% 
  rename (Landed_name = Species) %>% # match to spp list
  left_join (spp_list, by = "Landed_name") %>%
  rename (species = sci_name_underscore) %>% # match to hab change
  left_join (hab_change, by = "species") %>%
  left_join (smoothLL_MASE, by = "species") %>%
  filter (!is.na(scenario)) %>%
    ggplot (aes (x = log(mn_val), y = perc_change_Morely)) +
    geom_point () +
  geom_text (aes(label = species, color = as.factor(Model_suitable))) +
  scale_color_manual (values = c("darkgray", "black"), label = c("Model unsuitable", "Model suitable")) +
  labs (color = "",
        y = "Percent habitat change",
        x = "log mean annual landed value") +
  #geom_errorbar (aes (xmin = mn_val - sd_val, xmax = mn_val + sd_val), alpha = 0.3) +
  geom_hline (yintercept = 0, lty = 2) +
  facet_wrap (~scenario) +
  # xlim (0, 10000000) +
  ylim (-100, 700) + # for sll, this just removes 4 rows, lophias and molva
  theme_bw () +
  ggtitle ("Percent habitat change by species value, Smooth lat/lon") +
  theme (
    axis.text = element_text (size = 16),
    axis.title = element_text (size = 18),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 18)
  )

dev.off()

# Temp/depth ----
TD_MASE <- read_csv ("Models/Temp_Depth_GAM_diagnostics.csv") %>%
  #rename (species = Species)  %>%
  mutate (Model_suitable = ifelse (
    MASE_GAM < 1 & DM_GAM_p < 0.05, 
    "1", 
    "0"
  ))

png (file = "Figures/Percent_hab_change_by_overall_value_TempDepth.png", width = 16, height = 9, units = "in", res = 300)
lands_qu %>%
  filter (year > 2010) %>%
  group_by (Species) %>%
  filter (metric == "X1000.ISK") %>% 
  summarize (mn_val = mean (amount/1000, na.rm = TRUE),
             sd_val = mean(amount/1000, na.rm = TRUE)) %>% 
  rename (Landed_name = Species) %>% # match to spp list
  left_join (spp_list, by = "Landed_name") %>%
  rename (species = sci_name_underscore) %>% # match to hab change
  left_join (hab_change, by = "species") %>%
  left_join (TD_MASE, by = "species") %>%
  filter (!is.na(scenario)) %>%
  ggplot (aes (x = log(mn_val), y = perc_change_Morely)) +
  geom_point () +
  geom_text (aes(label = species, color = as.factor(Model_suitable))) +
  scale_color_manual (values = c("darkgray", "black"), label = c("Model unsuitable", "Model suitable")) +
  labs (color = "",
        y = "Percent habitat change",
        x = "log mean annual landed value") +
  #geom_errorbar (aes (xmin = mn_val - sd_val, xmax = mn_val + sd_val), alpha = 0.3) +
  geom_hline (yintercept = 0, lty = 2) +
  facet_wrap (~scenario) +
  # xlim (0, 10000000) +
  ylim (-100, 105) + # for TD, 100 removes merlangius and whiffiagonis, molva 585 is 101
  theme_bw () +
  ggtitle ("Percent habitat change by species value, Depth/Temp") +
  theme (
    axis.text = element_text (size = 16),
    axis.title = element_text (size = 18),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 18)
  )

dev.off()


# plot 

# too busy with color by quota class
png (file = "Figures/Percent_hab_change_by_sector_importance_SLL.png", width = 16, height = 9, units = "in", res = 300)

# png (file = "Figures/Percent_hab_change_by_sector_importance_DepthTemp.png", width = 16, height = 9, units = "in", res = 300)
spp_by_qu %>%
  left_join (hab_change, by = "species") %>%
  filter (!is.na(scenario), mn_Tonnes > 0) %>%
  ggplot (aes (x = log(mn_Tonnes),
               y = perc_change_Morely,
               size = mn_X1000.ISK)
  ) +
  geom_point () +
  #geom_errorbar (aes(xmin = mn_cat - sd_cat, xmax = mn_cat + sd_cat)) +
  # geom_text (aes (label = species),
  #            #check_overlap = TRUE,
  #            hjust = 0,
  #            nudge_y = 0.5) +
  facet_grid (scenario ~ Quota.class,
              labeller = labeller (Quota.class = quot_labs)) +
  geom_hline (yintercept = 0, lty = 2) +
  geom_vline (xintercept = 0, lty = 2) +
  xlim (-2, 15) + # for SLL
  ylim (-100, 500) +
  # xlim (-2, 15) +
  # ylim (-100, 100) + # for depth temp
  theme_bw () +
  labs (x = "log (mean annual tonnes landed)",
        y = "Percent habitat change",
        size = "Value, 1000 ISK") +
  ggtitle ("Smoothed lat/lon model, habitat change by fisheries sector") +
  theme (
    axis.text = element_text (size = 16),
    axis.title = element_text (size = 18),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 18)
  )
dev.off()

cf_spp <- lands_sm %>%
  filter (metric == "Tonnes", Quota.class == "Coastal fisheries") %>%
  group_by (Species)  %>% 
  summarize (mn_cat = mean (amount, na.rm = TRUE),
             sd_cat = sd(amount, na.rm = TRUE)) %>%
  filter (! is.na (mn_cat)) %>%
  rename (Landed_name = Species) %>% # match to spp list
  left_join (spp_list, by = "Landed_name") %>%
  rename (species = sci_name_underscore) # match to hab change
  #arrange (desc (mn_cat))

cf_spp %>%
  left_join (hab_change, by = "species") %>% 
  filter (!is.na(scenario)) %>%
  ggplot (aes (x = log(mn_cat), 
               y = perc_change_Morely)) +
  geom_point () +
  geom_errorbar (aes(xmin = mn_cat - sd_cat, xmax = mn_cat + sd_cat)) +
  geom_text (aes (label = species),
             #check_overlap = TRUE,
             hjust = 0,
             nudge_y = 0.5) +
  facet_wrap (~scenario) +
  geom_hline (yintercept = 0, lty = 2) +
  geom_vline (xintercept = 0, lty = 2) +
  xlim (-5, 10) +
  ylim (-100, 100)
  