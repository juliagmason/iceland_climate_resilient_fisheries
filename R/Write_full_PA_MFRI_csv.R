## Full spp presence/absence table
# 6/22/2020
# JGM

## full spread of species presence/absence for each sample. Use comb_ds, not comb_samples. 

# will have samples/tows as rows and species names as columns, with 1 for presence and 0 for absence. 

library (tidyverse)

comb_ds <- read_csv("Data/MFRI_comb_survey.csv", 
                    col_types = cols(
                      sample_id = col_factor(),
                      species = col_factor(),
                      stat_sq = col_factor()
                      )
)

spp_list <- read_csv("Data/species_eng.csv",
                     col_types = cols(Spp_ID = col_factor())) %>%
  rename (species = Spp_ID) %>% # rename to match comb_ds
  
mutate (comb_name = ifelse (is.na (Scientific_name),
                                   Common_name,
                            Scientific_name
                            )# some have common name but not sci name, so combine here to cover my bases
        )

# matching by sci_name does drop off 5 species. get these back when I do by combined common/sci name

sampled_spp <- data.frame (species = unique (comb_ds$species)) # 90 spp

 
sample_spread <- comb_ds %>%
  # populate with full list of species for each sample
  left_join (spp_list, by = "species") %>%
  # make appropriate column name without a space
  mutate (sci_name = gsub(" ", "_", comb_name)) %>% 
  # get rid of unnecessary columns
  dplyr::select (sample_id, sci_name) %>%
  # get rid of repeated species measurements
  distinct_at (c("sample_id", "sci_name"), .keep_all = TRUE) %>%
  mutate (Presence = 1) %>%
  pivot_wider (names_from = sci_name, values_from = Presence)

# just replace NAs with zero in base R
sample_spread[is.na (sample_spread)] <-  0

write.csv (sample_spread, file = "Data/PA_MFRI.csv", row.names = FALSE)  


### figuring out repeated species...
# we lose one species. what is it?
spp_91 <- gsub(" ", "_", spp_list$comb_name[which (spp_list$species %in% unique(comb_ds$species))])

which (! spp_91 %in% spp_pa_yr$species)
# had to go through and look . sebastes mentella is repeated! 11 and 61
length (which (comb_ds$species == 11)) # 16
length (which (comb_ds$species == 61)) # 53833
