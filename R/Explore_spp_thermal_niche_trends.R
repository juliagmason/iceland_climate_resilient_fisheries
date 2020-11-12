# Explore species resilience: thermal trends vs habitat change
# 10/29/2020
# JGM
library (tidyverse)

spp_ts <-  read_csv("Data/spp_niche_annual_trends.csv")
spp_list <- read_csv ("Data/species_eng.csv")

spp_slopes <- spp_ts %>%
  group_by (species) %>%
  filter (length (unique (year)) > 10,
          # get rid of inverts and "other"
          !species %in% c(40, 41,43, 45, 46, 55)) %>% 
  ungroup() %>%
  filter (!is.na (wtmean)) %>%
  group_by (species, season, var) %>%
  summarise (slope = lm (wtmean ~ year)$coefficients[2])

# # 54 is a big outlier, very few points
spp_slopes_by_var <- spp_slopes %>%
  filter (season == "spring", species != 54) %>%
  pivot_wider (names_from = var,
               values_from = slope)
# plot absolute change in bottom temp vs absolute change in latitude
spp_slopes_by_var %>% 
  ggplot (aes (x = abs(bottom.temp), 
               y = abs(lat))) +
  geom_text (aes (label = species))


  # not significant
summary (lm (abs(lat) ~ abs(bottom.temp), data = spp_slopes_by_var))


# general pronounced inverse relationship between temperature slope and lat slope. would expect more curved, where big change in temperature would be small change in lat. 

# change in bottom temp vs. steno index. expect small steno = small change in temp, but big change in lat
spp_slopes_by_var %>%
  rename (Spp_ID = species) %>% 
  left_join (spp_list, by = "Spp_ID") %>% 
  ggplot (aes (x = mean_Steno, y = abs(bottom.temp))) +
  geom_text (aes (label = Spp_ID))

# expect small steno = small change in temp, but big change in lat
spp_slopes_by_var %>%
  rename (Spp_ID = species) %>% 
  left_join (spp_list, by = "Spp_ID") %>% 
  ggplot (aes (x = mean_Steno, y = lat)) +
  geom_text (aes (label = Spp_ID, col = mean_TB))


spp_slopes_therm <- spp_slopes_by_var %>%
  rename (Spp_ID = species) %>% 
  left_join (spp_list, by = "Spp_ID")

summary (lm (mean_Steno ~ abs(lat), data = spp_slopes_therm))
summary (lm (mean_Steno ~ abs(bottom.temp), data = spp_slopes_therm))


## explore past slopes vs. projected
load ("Data/perc_hab_change_Morely_Borm_14_alltemp.RData")
load ("Data/centroids_Borm_14_alltemp.RData")

# hab change vs. bottom temp slope
hab_change %>%
  filter (scenario == 585) %>%
  group_by (species) %>%
  summarise (mn_hab_change = mean(perc_change)) %>%
  rename (sci_name_underscore = species) %>%
  left_join (spp_list, by = "sci_name_underscore") %>%
  rename (species = Spp_ID) %>%
  left_join (spp_slopes_by_var, by = "species") %>%
  ggplot (aes (x = abs(bottom.temp), y = mn_hab_change)) +
  geom_text (aes (label = species))

# hab change vs. lat change
hab_change %>%
  filter (scenario == 585) %>%
  group_by (species) %>%
  summarise (mn_hab_change = mean(perc_change)) %>%
  rename (sci_name_underscore = species) %>%
  left_join (spp_list, by = "sci_name_underscore") %>%
  rename (species = Spp_ID) %>%
  left_join (spp_slopes_by_var, by = "species") %>%
  ggplot (aes (x = abs(lat), y = mn_hab_change)) +
  geom_text (aes (label = species))

# 3 variables?
hab_change %>%
  filter (scenario == 585) %>%
  group_by (species) %>%
  summarise (mn_hab_change = mean(perc_change)) %>%
  rename (sci_name_underscore = species) %>%
  left_join (spp_list, by = "sci_name_underscore") %>%
  rename (species = Spp_ID) %>%
  left_join (spp_slopes_by_var, by = "species") %>%
  ggplot (aes (x = abs(lat), y = abs(bottom.temp),
               col = mn_hab_change)) +
  geom_text (aes (label = species))
# maybe yes? the ones with the smallest lat/bottom temp have the most habitat change??


# just hab change vs tb and steno
hab_change %>%
  filter (scenario == 585) %>%
  group_by (species) %>%
  summarise (mn_hab_change = mean(perc_change)) %>%
  rename (sci_name_underscore = species) %>%
  left_join (spp_list, by = "sci_name_underscore") %>%
  ggplot (aes (x = mean_TB, y = mn_hab_change)) +
  geom_text (aes (label = Spp_ID))
# this is more interesting. warmer species much more likely to see big habitat change. 

hab_change %>%
  filter (scenario == 585) %>%
  group_by (species) %>%
  summarise (mn_hab_change = mean(perc_change)) %>%
  rename (sci_name_underscore = species) %>%
  left_join (spp_list, by = "sci_name_underscore") %>%
  ggplot (aes (x = mean_Steno, y = mn_hab_change)) +
  geom_text (aes (label = Spp_ID))
# this also makes sense. most narrow species more likely to see big change, although it's mostly positive change. 

hab_change %>%
  filter (scenario == 585) %>%
  group_by (species) %>%
  summarise (mn_hab_change = mean(perc_change)) %>%
  rename (sci_name_underscore = species) %>%
  left_join (spp_list, by = "sci_name_underscore") %>%
  ggplot (aes (x = mean_Steno, y = mn_hab_change, col = mean_TB)) +
  geom_text (aes (label = Spp_ID))
# those narrower species are warmer water species, so that makes sense. 