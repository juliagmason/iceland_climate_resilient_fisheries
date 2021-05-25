# Multiple visualizations--spider plots etc
# 5/21/21
# JGM

common_spp <- spp_list %>%
  filter (n_spring > 35 | n_autumn > 24)

# group species by abundance trends ----
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

# let's do 1995-2010 for now
# https://r4ds.had.co.nz/many-models.html
year_lm <- function (df) {
  lm (wtmean ~ year, data = df)
}

biomass_categories <- abundance_trends %>%
  filter (var == "biomass", species %in% common_spp$Spp_ID, between (year, 1995, 2010)) %>%
  group_by (species, season) %>%
  nest() %>%
  mutate (model = map(data, year_lm)) %>%
  #mutate (glance = map (model, broom::glance)) %>%
  mutate (tidy = map(model, broom::tidy)) %>%
  unnest (tidy) %>%
  filter (term == "year") %>%
  mutate (biomass_cat = case_when
          (estimate > 0 & p.value < 0.05 ~ "Positive",
            estimate < 0 & p.value < 0.05 ~ "Negative",
            TRUE ~ "Insig"))

spp_annual_trends <- read_csv ("Data/spp_niche_annual_trends.csv")

slopes <- spp_annual_trends %>%
  group_by (species, season, var) %>%
  filter (species %in% common_spp$Spp_ID) %>%
  nest() %>%
  mutate (model = map(data, year_lm)) %>%
  #mutate (glance = map (model, broom::glance)) %>%
  mutate (tidy = map(model, broom::tidy)) %>%
  unnest (tidy) %>%
  filter (term == "year") %>%
  select (species, season, var, estimate, p.value) %>%
  pivot_wider (names_from = var,
               values_from = c(estimate, p.value)) #%>%
  # pivot_longer (! c(species, season),
  #               names_to = c("stat", "var"),
  #               names_sep = "_",
  #               values_to = "val")


# plot temp and lat slopes, color with biomass_cat?  
biomass_categories %>%
  mutate (species = as.numeric(species)) %>%
  select (species, season, biomass_cat) %>%
  left_join (slopes, by = c ("species", "season")) %>%
  ggplot () +
  geom_point (aes (x = estimate_bottom.temp, y = estimate_lat, col = biomass_cat))


biomass_categories %>%
  mutate (species = as.numeric(species)) %>%
  select (species, season, biomass_cat) %>%
  left_join (slopes, by = c ("species", "season")) %>%
  ggplot () +
  geom_point (aes (x = estimate_bottom.temp, y = estimate_tow.depth.begin, col = biomass_cat))

biomass_categories %>%
  mutate (species = as.numeric(species)) %>%
  select (species, season, biomass_cat) %>%
  left_join (slopes, by = c ("species", "season")) %>%
  ggplot () +
  geom_point (aes (x = estimate_lat, y = estimate_tow.depth.begin, col = biomass_cat))


# just look at slopes
slopes %>% filter (p.value_lat < 0.05) %>% arrange (season, desc(estimate_lat)) %>% View()

# add to spp_annual_trends with new facto

# Caveats with spider plots
# https://www.data-to-viz.com/caveat/spider.html

# First try spider plot with bottom temp, depth, latitude, where the segment length is equivalent to the lollipop length

# spp_2period from plot_lollipop

# can't figure out how to do this with negatives.




# Start with PCA?
# http://environmentalcomputing.net/principal-components-analysis/#:~:text=Principal%20Components%20Analysis%20(PCA)%20is,%2D%20or%203%2Ddimensional%20space.

pca_df <- spp_2per_gg %>%
  #filter (var %in% c ("bottom.temp", "tow.depth.begin", "lat")) %>%
  mutate (diff = wtmean_Period2 - wtmean_Period1) %>%
  select (Spp_ID, var, diff) %>% # has to be Spp_ID unless I ungroup somewhere
  pivot_wider (names_from = var,
               values_from = diff)

scale_df <- scale (pca_df[, -1])

spp_PCA <- princomp (scale_df, cor = FALSE)
plot (spp_PCA$scores)
summary (spp_PCA)
loadings (spp_PCA)
biplot (spp_PCA)

# add tb?
tb <- pca_df %>%
  left_join (spp_list, by = "Spp_ID") %>%
  mutate (Therm_pref = case_when (
    mean_TB > 0 ~ "Warm",
    between (mean_TB, -3, 0) ~ "Cool",
    mean_TB < 0 ~ "Cold"
  ))
  
therm_pref <- tb$Therm_pref



# parallel coordinates plot ----
# https://www.datanovia.com/en/blog/beautiful-radar-chart-in-r-using-fmsb-and-ggplot-packages/#ggplot-radar-chart-using-the-ggradar-r-package
library (GGally)

# include biomass or abundance
spp_2period <- mfri_abun %>%
  # # remove species with only one observation
  # group_by (species) %>%
  # filter (n() > 1) %>%
  # ungroup() %>%
  filter (species %in% common_spp$Spp_ID) %>%
  select (species,  year, season, kg_tot, bottom_temp, tow_depth_begin, lat, lon) %>% # removed surface_temp
  # rename columns to replace underscore with period. Summarise will append new column names with an underscore and I can't figure out how to change it there
  rename_all(~ gsub("_", ".", .)) %>%
  # mutate (period = case_when (year %in% c(1985:1993) ~ "Period1",
  #                    year %in% c(2005:2013) ~ "Period2")
  #         ) %>%
  mutate (period = case_when (year %in% c(1995:1999) ~ "Period1",
                              year %in% c(2010:2014) ~ "Period2")
  ) %>%
  group_by (species, period, season) %>%
  summarise (
    across (bottom.temp:lon,
            list (
              wtmean = ~ weighted.mean (., w = kg.tot, na.rm = TRUE),
              se = ~ sd (.x, na.rm = TRUE)/sqrt(length(.x))
            ) # end list
    )) 

ggparcoord (
  tb,
  columns = 2:5,
  groupColumn = 16,
  showPoints = TRUE,
  alphaLines = 0.5
) + 
  theme_bw() +
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  theme(legend.position = "top") +
  ggtitle ("Parallel coordinates, major spp, 2010-2014 vs 1995-1999")


# do with abundance categories instead
biomass_cat_spring <- biomass_categories %>%
  filter (season == "spring") %>%
  rename (Spp_ID = species)

tb %>%
  left_join (biomass_cat_spring, by = "Spp_ID") %>% 
  ggparcoord (
    columns = 2:5,
    groupColumn = 25,
    showPoints = TRUE,
    alphaLines = 0.5
  ) + 
  theme_bw() +
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  theme(legend.position = "top") +
  ggtitle ("Parallel coordinates, major spp, 2010-2014 vs 1995-1999")

