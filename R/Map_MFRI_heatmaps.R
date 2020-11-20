# Function to map MFRI spatial distribution for any species
# 11-17-2020
# JGM

library (tidyverse)

library (maps)
library (mapdata)
library (sf)
library (rnaturalearth)
library (rnaturalearthdata)
library (rgeos)
library (viridis)

# only care about non-zero data
mfri_abun <- read_csv ("Data/MFRI_comb_survey.csv",
                       col_types = cols(
                         sample_id = col_factor(),
                         species = col_factor(),
                         stat_sq = col_factor()
                       )
) %>%
  filter (!(year == 2011 & season == "autumn"),
          !(year < 2000 & season == "autumn")) %>% # remove autumn 2011 [labor strike] and pre-2000 autumn [sites weren't standard yet]
  group_by (sample_id, species) %>%
  summarize (n_tot = sum(n_per_nautmile),
             kg_tot = sum(kg_per_nautmile)) %>%
  mutate (n_log = log(n_tot),
          kg_log = log(kg_tot))

# predictor variables
# I compiled these in Build_MFRI_predictor_df.R. 
mfri_pred <-  read_csv ("Data/MFRI_predictor_df.csv",
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
  filter (!(year == 2011 & season == "autumn"),
          !(year < 2000 & season == "autumn")
  )  # remove autumn 2011, 131 samples. Also cutting out autumn pre-2000 (as of 7/23/2020. so did this with MASE fitting, but not with original run through of full gams)

# species list
# This relates species IDs to scientific names
spp_list <- read_csv ("Data/species_eng.csv",
                      col_types = cols(
                        Spp_ID = col_factor()
                      ))

# worldmap
world <- ne_countries(scale = "medium", returnclass = "sf")

plot_spp_map <- function (sci_name) {
  
  # match species sci name to ID code in spp_list
  spp_id <- as.numeric(as.character(spp_list$Spp_ID[which (spp_list$sci_name_underscore == sci_name)])) # this fixes weird factor problem
  
  # filter points for species
  spp_pts <- mfri_abun %>%
    filter (species == spp_id) %>%
    left_join (mfri_pred, by = "sample_id")
  
  # also show centroid of distribution?
  centroid <- spp_pts %>%
    group_by (season) %>%
    summarise (lon = weighted.mean (spp_pts$lon, spp_pts$kg_log),
              lat = weighted.mean (spp_pts$lat, spp_pts$kg_log))
  
  # plot heatmap of log biomass
  ggplot(data = world) +
    geom_sf() +
    coord_sf (xlim = c(-30, -8), ylim = c (62,70), expand = FALSE ) +
    stat_summary_2d(data =  spp_pts, aes (x = lon, y = lat, z = kg_log), fun = mean, binwidth = c (.25, .25)) +
    scale_fill_viridis_c(name = "Mean log biomass") +
    geom_point (aes (x = lon, y = lat), data = centroid, size = 2) +
    theme_bw() +
    facet_wrap (~season) +
    ggtitle (sci_name)
}

plot_spp_map ("Artediellus_atlanticus")
plot_spp_map ("Anarhichas_lupus")

# plot all together, color as warm/cool/cold, size as overall biomass?
# and then could animate or show centroid shift
