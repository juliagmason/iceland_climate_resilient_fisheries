## Full spp presence/absence table
# 6/22/2020
# JGM

## full spread of species presence/absence for each sample. Use comb_ds, not comb_samples 

library (tidyverse)

comb_ds <- read_csv("Data/MFRI_comb_survey.csv")
spp_list <- read_csv("Data/species_eng.csv",
                     col_types = cols(Spp_ID = col_factor())) %>%
  rename (species = Spp_ID) %>% # rename to match comb_ds
mutate (comb_name = ifelse (is.na (Scientific_name),
                                   Common_name,
                            Scientific_name
                            )# some have common name but not sci name, so combine here to cover my bases
        )

# matching by sci_name does drop off 5 species. get these back when I do by combined common/sci name

sampled_spp <- data.frame (species = unique (comb_ds$species)) # 91 spp

sample_spread <- comb_ds %>%
  left_join (spp_list, by = "species") %>%
  mutate (sci_name = gsub(" ", "_", comb_name)) %>% # make appropriate column name
  dplyr::select (sample_id, sci_name) %>%
  distinct_at (c("sample_id", "sci_name"), .keep_all = TRUE) %>%
  mutate (Presence = 1) %>%
  pivot_wider (names_from = sci_name, values_from = Presence)

sample_spread[is.na (sample_spread)] = 0

write.csv (sample_spread, file = "Data/PA_MFRI.csv", row.names = FALSE)  
