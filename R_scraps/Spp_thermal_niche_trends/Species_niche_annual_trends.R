# Species thermal/depth trends
# 10/20/20
# JGM

# Following 10/20 call with H. Valtysson, it would be useful to look at how the mean temperature, depth, and latitude of catch for each species has changed through time. This will tell us about the adaptation/resilience of each species--are they moving along with their niche, or are they staying put?


library (tidyverse)
library (ggpmisc) # for annotating graphs with lm equation

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



# remove sst and add abundance and biomass----
# from Plot_spp_exploration
abundance_trends <- mfri_abun %>%
  group_by (species, year, season) %>%
  summarise (wtmean_abundance = mean (n_tot), 
             se_abundance = sd (n_tot, na.rm = TRUE)/sqrt(length(n_tot)),
             wtmean_biomass = mean (kg_tot),
             se_biomass = sd (kg_tot, na.rm = TRUE)/sqrt(length(kg_tot))
  ) %>%
  pivot_longer (! c(species, year, season),
                names_to = c("stat", "var"),
                names_sep = "_",
                values_to = "val"
  ) %>%
  pivot_wider (
    names_from = stat,
    values_from = val
  ) %>%
  # need to turn into a factor to join
  mutate (var = as.factor(var))

# add to spp_annual_trends with new factor order
var_levels = c("abundance","lat", "bottom.temp","biomass",  "lon","tow.depth.begin",  "surface.temp")

spp_annual_trends <- read_csv ("Data/spp_niche_annual_trends.csv",
                               col_types = cols (species = col_factor(),
                                                 var = col_factor())) %>%
  rbind (abundance_trends) %>%
  #  reorder the variable factors
  mutate (var = factor (var, levels = var_levels))

# test plot with one species
spp_annual_trends %>%
  filter (species == 24, var != "surface.temp") %>%
  ggplot (aes (x = year, y = wtmean, col = season)) +
  geom_point() +
  geom_line () +
  geom_errorbar (aes (ymax = wtmean + se, ymin = wtmean - se)) +
  facet_wrap (~ var, scales = "free")


# plot all species  ----
#dir.create ("Figures/Spp_niche_annual_trends")


# annotate with p values and adj R2
# https://mran.microsoft.com/snapshot/2016-03-16/web/packages/ggpmisc/vignettes/examples.html


plot_spp_niche <- function (spp) {
# for (spp in unique (spp_annual_trends$species)) {
    # match species ID to scientific name
    sci_name <- spp_list$sci_name_underscore[which (as.character(spp_list$Spp_ID) == spp)]
    
    title_name <- str_replace (sci_name, "_", " ")
    
    my.formula <- y ~ x
    
    
    # create file name
    # save_path <- file.path ("Figures", "Spp_niche_annual_trends", paste0 (sci_name, "_niche_trends.png"))
    
    # plot and save
    # png (file = save_path, width = 16, height = 9, units = "in", res = 300)
#print(    
    spp_annual_trends %>%
    filter (species == spp, var != "surface.temp") %>%
      ggplot (aes (x = year, y = wtmean, col = season)) +
      geom_point() +
      geom_line () +
      geom_errorbar (aes (ymax = wtmean + se, ymin = wtmean - se)) +
      facet_wrap (~ var, scales = "free") +
      ggtitle (title_name) +
      geom_smooth (method = "lm", se = FALSE, aes (color = season), alpha = 0.1) +
      stat_poly_eq(formula = my.formula,
                   aes (label = paste (..adj.rr.label.., ..p.value.label.., sep = "~~~"), color = season), parse = TRUE) +
      theme_bw() +
      labs (x = "") +
      theme (
        axis.text = element_text (size = 16),
        axis.title = element_text (size = 18),
        plot.title = element_text (size = 24, face = "italic"),
        strip.text = element_text (size = 18),
        legend.title = element_text (size = 18),
        legend.text = element_text (size = 16),
        legend.position = "bottom" #c(0.9, 0.2)
      )
#)
    # dev.off()
}

niche_spp <- as.character(sort (unique (spp_annual_trends$species)))

common_spp <- spp_list %>%
  filter (n_spring > 35 | n_autumn > 24)

pdf ("Figures/Spp_niche_annual_trends_lm_common_biomass.pdf", width = 11, height = 8.5)
  lapply (as.character(common_spp$Spp_ID), plot_spp_niche)
dev.off()

  
### are trends statistically significant? ----
# use purr to run an lm for each species
# http://www.rebeccabarter.com/blog/2019-08-19_purrr/#nesting-the-gapminder-data

# don't want to have an annual average, just want year as a predictor variable

spp_nested <- mfri_abun %>%
  select (species, kg_tot, year, season, surface_temp, bottom_temp, tow_depth_begin, lat, lon) %>%
  # rename columns to replace underscore with period. Summarise will append new column names with an underscore and I can't figure out how to change it there
  rename_all(~ gsub("_", ".", .)) %>%
    # remove species with only one observation
    group_by (species) %>%
    filter (n() > 1) %>%
    # for my variables of interest, take a weighted mean based on total biomass, and standard error
    mutate (
      # this isn't right...
      across (surface.temp:lon, list(wtd = ~ . * kg.tot) )
      )  %>% # end mutate
  nest ()

spp_nested_simple_lm <- spp_nested %>%
  mutate (depth_lm_sim = map (data, ~ lm (tow.depth.begin ~ year, data = .x)),
          bt_lm_sim = map (data, ~lm (bottom.temp ~ year, data = .x)),
          sst_lm_simp = map (data, ~lm (surface.temp ~ year, data = .x)),
          lat_lm_simp = map (data, ~lm (lat ~ year, data = .x)),
          lon_lm_simp = map (data, ~lm (lon ~ year, data = .x)))

spp_nested_simple_lm %>% pluck ("depth_lm_sim", 1)

# do one variable at a time. filter for significant. 
# code from advanced exercise in Rebecca Barter's gapminder purr tutorial


spp_depth_lm <- spp_nested %>%
  mutate (depth_lm_sim = map (data, ~ lm (tow.depth.begin ~ year, data = .x))) %>%
  mutate (lm_tidy = map (depth_lm_sim, broom::tidy)) %>%
  ungroup () %>%
  transmute (species, lm_tidy) %>%
  unnest (cols = c(lm_tidy))

spp_depth_sig <- spp_depth_lm %>%
  filter (term == "year", p.value < 0.05)

# really does make more sense to use biomass-weighted as response var, and group at some level. prob doesn't matter too much. try monthly
spp_month_wt_avg <- mfri_abun %>%
  # remove species with only one observation
  group_by (species, season) %>%
  filter (n() > 1) %>%
  ungroup() %>%
  # make a variable for month and year
  #mutate (my = format (date, "%Y-%m")) %>%
  select (species, kg_tot, month, year, season, surface_temp, bottom_temp, tow_depth_begin, lat, lon) %>%
  # rename columns to replace underscore with period. Summarise will append new column names with an underscore and I can't figure out how to change it there
  rename_all(~ gsub("_", ".", .)) %>%
  group_by (species, month, year, season) %>% 
  # for my variables of interest, take a weighted mean based on total biomass, and standard error
  summarise (
    across (surface.temp:lon,
            list (
              wtmean = ~ weighted.mean (., w = kg.tot, na.rm = TRUE)
              # se = ~ sd (.x, na.rm = TRUE)/sqrt(length(.x))
            ) # end list
    ))  %>% 
  ungroup () 


spp_lm_month_season_wtd <-  spp_month_wt_avg %>%
  group_by (species, season) %>%
  nest () %>%
  mutate (depth_lm = map (data, ~ lm (tow.depth.begin_wtmean ~ year, data = .x)),
          bt_lm = map (data, ~lm (bottom.temp_wtmean ~ year, data = .x)),
          sst_lm = map (data, ~lm (surface.temp_wtmean ~ year, data = .x)),
          lat_lm = map (data, ~lm (lat_wtmean ~ year, data = .x)),
          lon_lm = map (data, ~lm (lon_wtmean ~ year, data = .x)))

# https://www.monicathieu.com/posts/2020-04-08-tidy-multilevel/

spp_lm_month_season_wtd %>%
  #filter (species == 1, season == "spring") %>%
  pull ("depth_lm") %>%
  pluck (1) %>%
  broom::tidy()

spp_lm_month_season_wtd %>%
  #filter (species == 1, season == "spring") %>%
  pull ("depth_lm") %>%
  pluck (100) %>%
  summary()

spp_depth_lm <- spp_lm_month_season_wtd %>%
  group_by (species, season) %>%
  nest () %>%
  mutate (depth_lm = map (data, ~ lm (tow.depth.begin_wtmean ~ year, data = .x))) %>%
  mutate (lm_tidy = map (depth_lm, broom::tidy)) %>%
  ungroup () %>%
  transmute (species, season, lm_tidy) %>%
  unnest (cols = c(lm_tidy))

spp_depth_sig <- spp_depth_lm %>%
  filter (term == "year", p.value < 0.05) %>%
  arrange (estimate)

# ugly and should be able to do this with map more easily. I'm going to do this 5 times and then append together. 

spp_bt_sig <- spp_lm_month_season_wtd %>%
  group_by (species, season) %>%
  nest () %>%
  mutate (lm = map (data, ~ lm (bottom.temp_wtmean ~ year, data = .x))) %>%
  mutate (lm_tidy = map (lm, broom::tidy)) %>%
  ungroup () %>%
  transmute (species, season, lm_tidy) %>%
  unnest (cols = c(lm_tidy)) %>%
  filter (term == "year", p.value < 0.05) %>%
  arrange (estimate) %>%
  mutate (var = "bottom.temp")

spp_sst_sig <- spp_lm_month_season_wtd %>%
  group_by (species, season) %>%
  nest () %>%
  mutate (lm = map (data, ~ lm (surface.temp_wtmean ~ year, data = .x))) %>%
  mutate (lm_tidy = map (lm, broom::tidy)) %>%
  ungroup () %>%
  transmute (species, season, lm_tidy) %>%
  unnest (cols = c(lm_tidy)) %>%
  filter (term == "year", p.value < 0.05) %>%
  arrange (estimate) %>%
  mutate (var = "surface.temp")

spp_depth_sig <- spp_lm_month_season_wtd %>%
  group_by (species, season) %>%
  nest () %>%
  mutate (lm = map (data, ~ lm (tow.depth.begin_wtmean ~ year, data = .x))) %>%
  mutate (lm_tidy = map (lm, broom::tidy)) %>%
  ungroup () %>%
  transmute (species, season, lm_tidy) %>%
  unnest (cols = c(lm_tidy)) %>%
  filter (term == "year", p.value < 0.05) %>%
  arrange (estimate) %>%
  mutate (var = "depth")

spp_lat_sig <- spp_lm_month_season_wtd %>%
  group_by (species, season) %>%
  nest () %>%
  mutate (lm = map (data, ~ lm (lat_wtmean ~ year, data = .x))) %>%
  mutate (lm_tidy = map (lm, broom::tidy)) %>%
  ungroup () %>%
  transmute (species, season, lm_tidy) %>%
  unnest (cols = c(lm_tidy)) %>%
  filter (term == "year", p.value < 0.05) %>%
  arrange (estimate) %>%
  mutate (var = "lat")

spp_lon_sig <- spp_lm_month_season_wtd %>%
  group_by (species, season) %>%
  nest () %>%
  mutate (lm = map (data, ~ lm (lon_wtmean ~ year, data = .x))) %>%
  mutate (lm_tidy = map (lm, broom::tidy)) %>%
  ungroup () %>%
  transmute (species, season, lm_tidy) %>%
  unnest (cols = c(lm_tidy)) %>%
  filter (term == "year", p.value < 0.05) %>%
  arrange (estimate) %>%
  mutate (var = "lon")

spp_lm_sig <- rbind (spp_lat_sig, spp_lon_sig, spp_depth_sig, spp_bt_sig, spp_sst_sig)

saveRDS(spp_lm_sig, file = "Data/spp_niche_lm_sig.Rds")

spp_lm_sig <- readRDS ("Data/spp_niche_lm_sig.Rds")

## same thing, but only data since 2001

spp_nested_2001 <-  spp_month_wt_avg %>%
  filter (year >= 2001) %>%
  group_by (species, season) %>% 
  filter (n () > 1) %>%
  nest () 

spp_bt_sig <- spp_nested_2001 %>% 
  mutate (lm = map (data, ~ lm (bottom.temp_wtmean ~ year, data = .x))) %>%
  mutate (lm_tidy = map (lm, broom::tidy)) %>%
  ungroup () %>%
  transmute (species, season, lm_tidy) %>%
  unnest (cols = c(lm_tidy)) %>%
  filter (term == "year", p.value < 0.05) %>%
  arrange (estimate) %>%
  mutate (var = "bottom.temp")

spp_sst_sig <- spp_nested_2001 %>%
  mutate (lm = map (data, ~ lm (surface.temp_wtmean ~ year, data = .x))) %>%
  mutate (lm_tidy = map (lm, broom::tidy)) %>%
  ungroup () %>%
  transmute (species, season, lm_tidy) %>%
  unnest (cols = c(lm_tidy)) %>%
  filter (term == "year", p.value < 0.05) %>%
  arrange (estimate) %>%
  mutate (var = "surface.temp")

spp_depth_sig <- spp_nested_2001 %>%
  mutate (lm = map (data, ~ lm (tow.depth.begin_wtmean ~ year, data = .x))) %>%
  mutate (lm_tidy = map (lm, broom::tidy)) %>%
  ungroup () %>%
  transmute (species, season, lm_tidy) %>%
  unnest (cols = c(lm_tidy)) %>%
  filter (term == "year", p.value < 0.05) %>%
  arrange (estimate) %>%
  mutate (var = "depth")

spp_lat_sig <- spp_nested_2001 %>%
  mutate (lm = map (data, ~ lm (lat_wtmean ~ year, data = .x))) %>%
  mutate (lm_tidy = map (lm, broom::tidy)) %>%
  ungroup () %>%
  transmute (species, season, lm_tidy) %>%
  unnest (cols = c(lm_tidy)) %>%
  filter (term == "year", p.value < 0.05) %>%
  arrange (estimate) %>%
  mutate (var = "lat")

spp_lon_sig <- spp_nested_2001 %>%
  mutate (lm = map (data, ~ lm (lon_wtmean ~ year, data = .x))) %>%
  mutate (lm_tidy = map (lm, broom::tidy)) %>%
  ungroup () %>%
  transmute (species, season, lm_tidy) %>%
  unnest (cols = c(lm_tidy)) %>%
  filter (term == "year", p.value < 0.05) %>%
  arrange (estimate) %>%
  mutate (var = "lon")

spp_lm_sig <- rbind (spp_lat_sig, spp_lon_sig, spp_depth_sig, spp_bt_sig, spp_sst_sig)

saveRDS(spp_lm_sig, file = "Data/spp_niche_lm_sig_2001.Rds")

spp_lm_sig_01 <- readRDS ("Data/spp_niche_lm_sig_2001.Rds")

# probably should keep intercep, not sure how to include. would be ideal to have intercept, slope, r2, and p-value from summary()
