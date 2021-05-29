## JUST LOOK AT HOW MANY BORMICON REGIONS EACH SPP HAS
# 5/27/2021
# JGM

library (tidyverse)

# species list
# This relates species IDs to scientific names
spp_list <- read_csv ("Data/species_eng.csv",
                      col_types = cols(
                        Spp_ID = col_factor()
                      ))

spp_join <- spp_list %>%
  dplyr::select (Spp_ID, sci_name_underscore) %>%
  mutate (Spp_ID = as.factor(Spp_ID))

load ("Models/spp_Smooth_latlon.RData")


mfri_abun <- read_csv ("Data/MFRI_comb_survey.csv",
                       col_types = cols(
                         sample_id = col_factor(),
                         species = col_factor(),
                         stat_sq = col_factor()
                       )
) %>% 
  # remove autumn 2011 [labor strike] and pre-2000 autumn [sites weren't standard yet]
  filter (!(year == 2011 & season == "autumn"),
          !(year < 2000 & season == "autumn")
  ) %>% 
  # get one biomass value for each species and survey tow
  group_by (sample_id, species) %>%
  summarize (n_tot = sum(n_per_nautmile),
             kg_tot = sum(kg_per_nautmile)
  ) %>%
  # calculate log biomass
  mutate (n_log = log(n_tot),
          kg_log = log(kg_tot)) 

# predictor variables
# I compiled these in Build_MFRI_predictor_df.R. Each row is a survey tow, should match mfri_pa
mfri_pred_borm <-  read_csv ("Data/MFRI_predictor_df.csv",
                             col_types = cols(
                               sample_id = col_factor(),
                               stat_sq = col_factor(),
                               bormicon_region = col_factor(),
                               sst_dev = col_number(),
                               bt_dev = col_number(),
                               sst_max = col_number(),
                               sst_min = col_number(),
                               bt_max = col_number(),
                               bt_min = col_number()
                             )
) %>%
  # remove autumn 2011 [labor strike] and pre-2000 autumn [sites weren't standard yet]
  filter (!(year == 2011 & season == "autumn"),
          !(year < 2000 & season == "autumn")) %>%
  dplyr::select (sample_id, year, tow_depth_begin, bottom_temp, surface_temp, bormicon_region, sst_max, sst_min, bt_max, bt_min, rugosity) %>%
  # as of 12/17/2020, also filter out NAs for borm_14 model predictors to speed things up, and so I can replace relevant fake zeroes. now should have 19759 rows
  drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max"))


borm_abun <- mfri_abun %>%
  left_join (mfri_pred_borm, by = "sample_id") %>%
  group_by (species) %>%
  summarise (n_borm = length (unique (bormicon_region))) %>%
  rename (Spp_ID = species) %>%
  left_join (spp_join, by = "Spp_ID") %>%
  filter (sci_name_underscore %in% spp_Smooth_latlon$sci_name_underscore) %>%
  relocate (sci_name_underscore, .before = "Spp_ID")

write.csv (borm_abun, file = "Data/spp_n_bormicon_regions.csv", row.names = FALSE)
