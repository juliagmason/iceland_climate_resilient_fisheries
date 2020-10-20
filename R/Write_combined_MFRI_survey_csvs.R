## write combined datasets
# 6/22/2020
# JGM

library (tidyverse)
library (lubridate)

# write and save combined spring and autumn datasets
spr_ds <- read_csv ("Data/Raw_data/springsurvey.csv",
                    col_types = cols(
                      sample_id = col_factor(),
                      species = col_factor(),
                      stat_sq = col_factor() # staves off trailing ch parsing error
                      
                    )) %>%
  mutate (date = make_date (year, month))

aut_ds <- read_csv ("Data/Raw_data/autumnsurvey.csv",
                    col_types = cols(
                      sample_id = col_factor(),
                      species = col_factor(),
                      stat_sq = col_factor() # staves off trailing ch parsing error
                      
                    ))%>%
  mutate (date = make_date (year, month))

# combined dataset
comb_ds <- rbind (spr_ds, aut_ds) 
comb_ds$season = c(rep ("spring", nrow (spr_ds)), rep("autumn", nrow (aut_ds)))

# fix repeated sebastes mentella
# Valtysson: these are distinct
#comb_ds$species[which (comb_ds$species == 11)] <- 61

# fix Lycodes species groupings
# Species 91 (Lycodes rossi) has been combined with another, species 59 (Lycodes reticulatus). Originally they were thought to be two species but are now considered the same.
# Rossi only caught before 1990, reticulatus cauth later
comb_ds$species[which (comb_ds$species == 91)] <- 59

write.csv (comb_ds, file = "Data/MFRI_comb_survey.csv", row.names = FALSE)

# also create datasets with just station points, for temp and gear info
spr_samples <- spr_ds %>%
  distinct_at ("sample_id", .keep_all = TRUE)

aut_samples <- aut_ds %>%
  distinct_at ("sample_id", .keep_all = TRUE)

#combine
comb_samples <- rbind (spr_samples, aut_samples) %>%
  dplyr::select (-c(species, length_cm, n_per_nautmile, kg_per_nautmile)) # remove species info
comb_samples$season = c(rep ("spring", nrow (spr_samples)), rep("autumn", nrow (aut_samples)))

write.csv (comb_samples, file = "Data/MFRI_comb_survey_samples.csv", row.names = FALSE)
