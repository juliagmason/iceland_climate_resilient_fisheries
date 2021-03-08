# Leaflet interactive map
# 11/20/20
# JGM

library (tidyverse)
library (leaflet)

# only care about non-zero data, but keep lat/lon/date so I don't have to bring in mfri_pred
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
  


# species list, add information about those species
spp_list <- read_csv ("Data/species_eng.csv",
                      col_types = cols(
                        Spp_ID = col_factor()
                      )) %>%
  # pad scientific_name column, which I'll use for my popup labels
  mutate (Scientific_name = ifelse (is.na (Scientific_name), Common_name, Scientific_name))




# make a nested dataframe. First, sum up repeated species within each sample. Then, group and next by species, so you have each tow/sample that caught that species as a list column.
mfri_by_spp <- mfri_abun %>%
  group_by (species) %>%
  nest() %>%
  # add information about each species from spp_list. have to rename the "species" column, which is the numeric ID
  rename (Spp_ID = species) %>%
  left_join (spp_list, by = "Spp_ID") %>%
  # I'm mainly interested in the center of distribution right now. Find the biomass-weighted mean lat and lon, to use for plotting 
  # # http://www.rebeccabarter.com/blog/2019-08-19_purrr/#list-columns-and-nested-data-frames
  # I also want to have categories for warm/cool/cold, based on Campana et al. 
  mutate (lat_centroid = map_dbl (data, ~{weighted.mean (.x$lat, log(.x$kg_tot + 1), na.rm = TRUE)}),
          lon_centroid= map_dbl (data, ~{weighted.mean (.x$lon, log(.x$kg_tot + 1), na.rm = TRUE)}),
          Therm_pref = case_when (
            mean_TB > 0 ~ "Warm",
            between (mean_TB, -3, 0) ~ "Cool",
            mean_TB < 0 ~ "Cold")
  )


# plot centroid of distribution, color by TB
TB_pal <- colorFactor(c("blue", "deepskyblue", "red2"), domain = c("Cold", "Cool", "Warm"))

mfri_by_spp %>%
leaflet () %>%
  addTiles() %>%
  #addCircleMarkers()
  addCircleMarkers (~lon_centroid, ~lat_centroid, 
                    # sci name, line break then TB, Steno, and depth
              popup = ~as.character (paste (Scientific_name, 
                                            paste ("Thermal bias: ", round (mean_TB, 2)),
                                            paste ("Stenothermal index: ", round (mean_Steno, 2)),
                                            paste("Mean depth: ", round (mean_depth, 2)), sep = "<br>")
                                     ),
              color = ~TB_pal(Therm_pref)
              )
