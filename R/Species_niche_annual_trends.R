# Species thermal/depth trends
# 10/20/20
# JGM

# Following 10/20 call with H. Valtysson, it would be useful to look at how the mean temperature, depth, and latitude of catch for each species has changed through time. This will tell us about the adaptation/resilience of each species--are they moving along with their niche, or are they staying put?


library (tidyverse)

# load data ----

# abundance data
# this is biomass data, where each row is the observed biomass of a species in a sample. absences are not included. Species are indicated by numeric species IDs, not scientific name. I'm loading this in the same way I've loaded it previously for other scripts. 
mfri_abun <- read_csv ("Data/MFRI_comb_survey.csv",
                       col_types = cols(
                         sample_id = col_factor(),
                         species = col_factor(),
                         stat_sq = col_factor()
                       )
)%>%
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


# Calculate biomass weighted means ----
# I'm going to use summarise across to take the mean and se of the columns of interest. 
# https://dplyr.tidyverse.org/dev/articles/colwise.html#multiple-functions
# https://datascience.stackexchange.com/questions/45148/weighted-mean-with-summarise-at-dplyr


# will use this at the end to reorder variable factors according to importance
var_levels = c("lat","bottom.temp", "tow.depth.begin","lon", "surface.temp")

spp_annual_trends <- mfri_abun %>%
  # remove species with only one observation
  group_by (species) %>%
  filter (n() > 1) %>%
  ungroup() %>%
  left_join (mfri_pred, by = "sample_id") %>%
  select (species, kg_tot, year, season, surface_temp, bottom_temp, tow_depth_begin, lat, lon) %>%
  # rename columns to replace underscore with period. Summarise will append new column names with an underscore and I can't figure out how to change it there
  rename_all(~ gsub("_", ".", .)) %>%
  group_by (species, year, season) %>%
  # for my variables of interest, take a weighted mean based on total biomass, and standard error
  summarise (
    across (surface.temp:lon,
      list (
        wtmean = ~ weighted.mean (., w = kg.tot, na.rm = TRUE),
        se = ~ sd (.x, na.rm = TRUE)/sqrt(length(.x))
        ) # end list
      ))  %>% # end summarise
  # to make things easier with ggplot, convert to a long format with a column for variable, mean, and se. I can only figure out how to do this in two steps with both longer and wider
  pivot_longer (! c(species, year, season),
                    names_to = c("var", "stat"),
                    names_sep = "_",
                    values_to = "val"
                ) %>%
  pivot_wider (
    names_from = stat,
    values_from = val
    ) %>%
  # finally, reorder the variable factors
  mutate (var = factor (var, levels = var_levels))

# write to csv
write.csv (spp_annual_trends, file = "Data/spp_niche_annual_trends.csv", row.names = FALSE)

# test plot with one species
spp_annual_trends
  filter (species == 24) %>%
  ggplot (aes (x = year, y = wtmean, col = season)) +
  geom_point() +
  geom_line () +
  geom_errorbar (aes (ymax = wtmean + se, ymin = wtmean - se)) +
  facet_wrap (~ var, scales = "free")

  
# plot all species  ----
dir.create ("Figures/Spp_niche_annual_trends")

for (spp in unique (spp_annual_trends$species)) {
    # match species ID to scientific name
    sci_name <- spp_list$sci_name_underscore[which (spp_list$Spp_ID == spp)]
    
    # create file name
    save_path <- file.path ("Figures", "Spp_niche_annual_trends", paste0 (sci_name, "_niche_trends.png"))
    
    # plot and save
    png (file = save_path, width = 16, height = 9, units = "in", res = 300)
print(    
    spp_annual_trends %>%
    filter (species == spp) %>%
      ggplot (aes (x = year, y = wtmean, col = season)) +
      geom_point() +
      geom_line () +
      geom_errorbar (aes (ymax = wtmean + se, ymin = wtmean - se)) +
      facet_wrap (~ var, scales = "free") +
      ggtitle (sci_name) +
      theme_bw () +
      theme (
        axis.text = element_text (size = 16),
        axis.title = element_text (size = 18),
        plot.title = element_text (size = 24),
        strip.text = element_text (size = 18),
        legend.title = element_text (size = 18),
        legend.text = element_text (size = 16)
      )
)
    dev.off()
  }
