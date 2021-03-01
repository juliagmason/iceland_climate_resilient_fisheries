## Characterize thermal bias, based on Campana et al. 2020

# Thermal bias is median catch-weighted temperature of species, all years - median bottom water temp of all stations. 
# Steno index: range of 5th adn 95th percentile catch-weighted temperature, all years


library (tidyverse)
library (spatstat) # for weighted median and quantiles

# survey points, from Write_combined_MFRI_survey_csvs.R
mfri_abun <- read_csv ("Data/MFRI_comb_survey.csv",
                       col_types = cols(
                         sample_id = col_factor(),
                         species = col_factor(),
                         stat_sq = col_factor()
                       )
) %>%
  filter (!(year == 2011 & season == "autumn"),
          !(year < 2000 & season == "autumn")) %>% # remove autumn 2011 [labor strike] and pre-2000 autumn [sites weren't standard yet]
  
  # tweaked this from in the other gam_fitting etc. scripts so I don't have to bring in mfri_pred
  select (-length_cm) %>% # this is the only remaining column that varies within sample_id
  group_by (sample_id, species) %>%
  mutate (n_tot = sum(n_per_nautmile),
          kg_tot = sum(kg_per_nautmile),
          n_log = log (n_tot),
          kg_log = log (kg_tot), 
          .keep = "unused") %>% 
  
  ungroup() %>%
  distinct ()  # now should have one row per species, per sample



# median bottom temp for each season
med_seas <- mfri_abun %>%
  group_by (season) %>%
  summarize (med = median (bottom_temp, na.rm = TRUE))

# calculate thermal index for each species. Since temperature distributions are different in the two seasons and Campana only did autumn, separate by season and take a mean
spp_therm <- mfri_abun %>%
  filter (!is.na (bottom_temp)) %>%
  group_by (species, season) %>%
  summarise (n_obs = n(),
             Therm = weighted.median(bottom_temp, w = kg_tot, na.rm = TRUE),
             Steno = weighted.quantile (bottom_temp, w = kg_tot, probs = 0.95, na.rm = TRUE) - weighted.quantile (bottom_temp, w = kg_tot, probs = 0.05, na.rm = TRUE),
             Depth = weighted.median (tow_depth_begin, w = kg_tot, na.rm = TRUE)
  ) %>%
  left_join (med_seas, by = "season") %>%
  mutate (TB = Therm - med) %>%
  group_by(species) %>%
  summarise (mean_TB = weighted.mean (TB, w = n_obs),
             mean_Steno = weighted.mean(Steno, w = n_obs),
             mean_depth = weighted.mean(Depth, w = n_obs)) %>%
  mutate (Spp_ID = as.numeric(as.character(species)), .keep = "unused") # rename to match other tables

write.csv (spp_therm, file = "Data/spp_thermal_affinity.csv", row.names = FALSE)



# cross checking with Campana et al. 2020----
# campana only did years 1996-2018, only autumn?
spp_therm_campana <- mfri_abun %>%
  left_join (mfri_pred, by = "sample_id") %>%
  filter (between (year, 1996, 2018), season == "autumn") %>% 
  group_by (species) %>%
  summarise (
    TB = (weighted.median(bottom_temp, w = kg_tot, na.rm = TRUE) - med_bw),
    Steno = weighted.quantile (bottom_temp, w = kg_tot, probs = 0.95, na.rm = TRUE) - weighted.quantile (bottom_temp, w = kg_tot, probs = 0.05, na.rm = TRUE),
    Depth = weighted.mean (tow_depth_begin, w = kg_tot, na.rm = TRUE)
  ) %>%
  arrange (as.numeric(species))
