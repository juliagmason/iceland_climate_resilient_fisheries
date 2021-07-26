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

# calculate thermal index for each species. Since temperature distributions are different in the two seasons and Campana only did autumn, separate by season and take a weighted mean
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

# Do any species have a different thermal bias in spring vs. autumn? ----
load ("Models/spp_Smooth_latlon.RData") 
borm_spp <- filter (spp_Smooth_latlon, 
                    ! sci_name_underscore %in% c("Myoxocephalus_scorpius", "Amblyraja_hyperborea", "Rajella_fyllae,", "Lumpenus_lampretaeformis", "Ammodytes_marinus", "Mallotus_villosus", "Micromesistius_poutassou", "Argentina_silus", "Scomber_scombrus", "Clupea_harengus"))


spp_tb_season <- mfri_abun %>%
  filter (!is.na (bottom_temp),
          species %in% borm_spp$Spp_ID) %>%
  group_by (species, season) %>%
  summarise (n_obs = n(),
             Therm = weighted.median(bottom_temp, w = kg_tot, na.rm = TRUE),
             Steno = weighted.quantile (bottom_temp, w = kg_tot, probs = 0.95, na.rm = TRUE) - weighted.quantile (bottom_temp, w = kg_tot, probs = 0.05, na.rm = TRUE),
             Depth = weighted.median (tow_depth_begin, w = kg_tot, na.rm = TRUE)
  ) %>%
  left_join (med_seas, by = "season") %>%
  mutate (TB = Therm - med,
          Therm_pref = case_when (
            TB > 0 ~ "Warm",
            between (TB, -3, 0) ~ "Cool",
            TB < 0 ~ "Cold"
          )) %>%  #filter (species %in% c(12, 28, 23, 9,  87, 62, 79, 80, 31, 65, 58)) %>% View()
  group_by (species) %>%
  summarise (n_tb = length (unique (Therm_pref))) %>%
  filter (n_tb > 1) %>% #View()
  pull (species)

spp_sci_names <- read_csv ("Data/species_eng.csv",
                                       col_types = cols(
                                         Spp_ID = col_factor()
                                       )) %>%
  select (Spp_ID, sci_name_underscore, Common_name) %>%
  rename (species = Spp_ID)

spp_therm_2seas <- mfri_abun %>%
  filter (!is.na (bottom_temp),
          species %in% spp_tb_season) %>%
  group_by (species, season) %>%
  summarise (n_obs = n(),
             Therm = weighted.median(bottom_temp, w = kg_tot, na.rm = TRUE),
             ) %>%
  left_join (med_seas, by = "season") %>%
  mutate (TB = Therm - med,
          Therm_pref = case_when (
            TB > 0 ~ "Warm",
            between (TB, -3, 0) ~ "Cool",
            TB < 0 ~ "Cold")
          ) %>%
  left_join (spp_sci_names, by = "species")
  
save (spp_therm_2seas, file = "Data/spp_diff_therm_pref_season_borm55.Rdata")

# how many of these are in the model?
load("Models/spp_Smooth_latlon.RData")
spp_tb_season[which (spp_tb_season %in% spp_Smooth_latlon$Spp_ID)] # 11spp: 9, 12, 23, 28, 31, 58, 62, 65, 79, 80, 87

spp_therm %>% filter (Spp_ID %in% c(12, 28, 23, 9,  87, 62, 79, 80, 31, 65, 58)) %>%
  mutate (Spp_ID = as.factor (Spp_ID)) %>%
  left_join (spp_Smooth_latlon, by = "Spp_ID")

# A. radiata (12), -1.57

# how many in borm suit?
load ("Models/spp_Borm_suit.RData")

# need to pair with spp_ID...
spp_borm_id <- spp_Smooth_latlon %>%
  filter (sci_name_underscore %in% borm_suit)
spp_tb_season[which (spp_tb_season %in% spp_borm_id$Spp_ID)] # 8 spp: 9, 12, 28, 23, 62, 79, 65, 58



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
