## Full spp presence/absence table
# 6/22/2020
# JGM

## full spread of species presence/absence for each sample. Use comb_ds, not comb_samples. 

# will have samples/tows as rows and species names as columns, with 1 for presence and 0 for absence. 

library (tidyverse)

# Combined spring and autumn MFRI surveys, created in Data_setup/Write_combined_MFRI_survey_csvs.R
# Each row/observation is a length class of a species in a sample tow. Sample_id is the tow number, species is a numeric code. There are 22 columns with information about the tow (date, speed, depth, temp, etc.)
comb_ds <- read_csv("Data/MFRI_comb_survey.csv", 
                    col_types = cols(
                      sample_id = col_factor(),
                      species = col_factor(),
                      stat_sq = col_factor()
                      )
)

# Species key to match numeric species codes with scientific names. Created in Data_setup/Write_species_eng.R
spp_list <- read_csv("Data/species_eng.csv",
                     col_types = cols(Spp_ID = col_factor())
                     ) %>%
  rename (species = Spp_ID) %>% # rename numeric spp_ID code column to match comb_ds
  mutate (comb_name = ifelse (is.na (Scientific_name),
                              Common_name,
                              Scientific_name
                              ) # some have common name but not sci name, so combine here to cover my bases. Matching just by sci_name drops off 5 species. 
        )


# List of species sampled in the survey
sampled_spp <- data.frame (species = unique (comb_ds$species)) # 90 spp


# Create a presence/absence matrix where each row is a survey tow and each column is a species.  
sample_spread <- comb_ds %>%
  # populate with full list of species for each sample
  left_join (spp_list, by = "species") %>%
  # make appropriate column name without a space
  mutate (sci_name = gsub(" ", "_", comb_name)) %>% 
  # get rid of unnecessary columns
  dplyr::select (sample_id, sci_name) %>%
  # get rid of repeated species measurements based on length class; I only need to know if the species was present or absent
  distinct_at (c("sample_id", "sci_name"), .keep_all = TRUE) %>%
  # Create a "Presence" column
  mutate (Presence = 1) %>%
  # make columns for each species
  pivot_wider (names_from = sci_name, values_from = Presence)

# just replace NAs with zero in base R to indicate absences
sample_spread[is.na (sample_spread)] <-  0

write.csv (sample_spread, file = "Data/PA_MFRI.csv", row.names = FALSE)  
