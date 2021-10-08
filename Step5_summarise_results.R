## Summarize model outputs and stats for paper
# 6/13/2021

library (tidyverse)

model_stats <- read_csv("Models/GAM_performance_Rug_tw_LL.csv") # made in Step1_Fit_GAMs_function.R


# Which are unsuitable?
unsuit <- model_stats %>% filter (MASE >=1)
unsuit2 <- model_stats %>%
  filter (MASE >= 1 | DM_GAM_p > 0.05)



# mean deviance explained
model_stats %>%
  filter (!species %in% unsuit$species) %>%
  summarise (mean = mean(dev_ex),
             range = range(dev_ex),
             sd = sd(dev_ex))

# tweedie
# mean range    sd
# <dbl> <dbl> <dbl>
#   1 0.587  0.21 0.197
# 2 0.587  0.96 0.197



# difference between full and naive dev exp

model_stats %>%
  filter (!species %in% c(unsuit$species)) %>%
  mutate (diff_exp = dev_ex - dev_ex_naive) %>%
  summarise (mean = mean(diff_exp),
             range = range(diff_exp),
             sd = sd(diff_exp))
# A tibble: 2 x 3
# mean  range    sd
# <dbl>  <dbl> <dbl>
#   1 0.217 0.0500 0.115
# 2 0.217 0.55   0.115

# Which species lose the most dev explained?
model_diff <- model_stats %>%
  filter (!species %in% c(unsuit$species)) %>%
  mutate (diff_exp = dev_ex - dev_ex_naive) 

summary (model_diff$diff_exp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0100  0.0800  0.1200  0.1465  0.2000  0.5100 
sd (model_diff$diff_exp)

# species with the greatest contribution of temp to dev expl
model_diff %>%
  arrange (desc (diff_exp)) %>%
  filter (diff_exp > .3) %>%
  select (species, dev_ex, diff_exp )

# species with the least contribution
model_stats %>%
  filter (!species %in% c(unsuit$species)) %>%
  mutate (diff_exp = dev_ex - dev_ex_naive) %>%
  arrange (diff_exp) %>%
  select (species, dev_ex, diff_exp )



## individual variable importance ----
var_imp <- read_csv("Models/var_imp_sp_Rug_tw_LL.csv") # made in Step1_Fit_GAMs_function.R

var_imp %>%
  filter (!species %in% c(unsuit$species)) %>%
  group_by (Var) %>%
  summarise (mn = mean (Diff_f),
             range = range (Diff_f),
             sd = sd (Diff_f)) %>%
  arrange (desc (mn))

## habitat change ----
 
load ("Data/hab_change_rug_tw_df.RData") # made on CISER remote server, Step3_Calculate_habitat_change.R


hab_change <- hab_change_rug %>%
  filter (!species %in% c(unsuit$species)) 

# species that increased suitable habitat
hab_inc <- hab_change %>%
  group_by (species, scenario) %>% summarise (med = median (log10_foldchange)) %>% filter (med > 0)

# species with decreased suitable habitat
hab_dec <- hab_change %>%
  group_by (species, scenario) %>% summarise (med = median (log10_foldchange)) %>% filter (med < 0)

# now i want to know if it increased or decreased in all climate models. 
hab_change %>%
  filter (species %in% hab_inc$species) %>%
  group_by (species, scenario) %>%
  summarise (min = min (log10_foldchange)) %>%
  filter (min > 0) %>%
  group_by (scenario) %>% 
  summarise (count = n())
# 10 245, 17 585
  

hab_change %>%
  filter (species %in% hab_dec$species) %>%
  group_by (species, scenario) %>%
  summarise (max = max (log10_foldchange)) %>%
  filter (max < 0) %>%
  group_by (scenario) %>% 
  summarise (count = n())
# 14 245, 13 585

# which species switched direction among scenarios?
hab_change %>%
  group_by (species, scenario) %>% 
  summarise (med = median (log10_foldchange)) %>%
  group_by (species) %>%
  summarise (medprod = prod(med)) %>% 
filter (medprod < 0)

hab_change %>%
  group_by (species, scenario) %>% 
  summarise (med = median (log10_foldchange)) %>% 
  filter (species == "Triglops_murrayi")
 

# can I compare the spread in predicted change among scenarios?
diff_spread <- hab_change %>%
  group_by (species, scenario) %>%
  summarise (max_diff = max(pred_med) - hist_med,
          min_diff = min(pred_med) - hist_med,
          diff_spread = max_diff - min_diff) %>%
  distinct()

anova(lm (diff_spread ~ scenario, data = diff_spread))
summary(lm (diff_spread ~ scenario, data = diff_spread))

# stats on habitat indices and habitat change ----
# is there a rltp between hab change and TB/steno?
hab_ind_lm_fun <- function (scen, var, hab_index) {
  # scen is scenario, 245 or 585
  # var is the response variable, amount of habitat change
  # hab_index is habitat index, mean_TB, mean_Steno, or mean_depth
  
  lm_form <- as.formula (paste0 (var, "~", hab_index)) 
  lm_ob <- lm (lm_form, data = filter(hab_change, scenario == scen))
  
  stats_df <- data.frame(
    hab_in = hab_index,
    resp_var = var,
    scenario = scen,
    r2 = round(summary(lm_ob)$adj.r.squared, 3),
    p_val = round(anova (lm_ob)$"Pr(>F)"[1], 3),
    int = round(lm_ob$coefficients[1],3),
    coef = round(lm_ob$coefficients[2], 3)
  )
  
}


habchange_ls <- expand.grid (scen = c(245, 585),
                             var = "log10_foldchange",
                             hab_index = c("mean_TB", "mean_Steno", "mean_depth")) %>%
  as.list()

habchange_stats_df <-  pmap_dfr (habchange_ls, hab_ind_lm_fun)
row.names (habchange_stats_df) <- NULL

habchange_stats_df 

# Centroids ----

load ("Data/centroids_Rug_allscenarios_tw.RData") # made in Step4_Calculate_centroid_change.R

spp_list <- read_csv("Data/species_eng.csv",
                     col_types = cols(Spp_ID = col_factor())
) %>%
  rename (species = sci_name_underscore)

cent_stats <- centroid_change_dc %>%
  filter (!species %in% c("Squalus_acanthias", "Icelus_bicornis")) %>%
  left_join (spp_list,by = "species" )
  

# ensemble mean and sd
centroid_change_dc %>%
  filter (!species %in% c("Squalus_acanthias", "Icelus_bicornis")) %>%
  left_join (spp_list, by = "species") %>%
  # characterize thermal bias as 3 broad niches from Campana et al. 2020
  mutate (
    Therm_pref = case_when (
      mean_TB > 0 ~ "Warm",
      between (mean_TB, -3, 0) ~ "Cool",
      mean_TB < 0 ~ "Cold"
    )) %>%
  
  group_by (Therm_pref, scenario) %>%
  summarize (mean = mean(dist)/1000,
             sd = sd(dist)/1000) %>% View()

# summarise distance results by model and scenario 
centroid_change_dc %>%
  filter (!species %in% c("Squalus_acanthias", "Icelus_bicornis")) %>%
  left_join (spp_list, by = "species") %>%
  # characterize thermal bias as 3 broad niches from Campana et al. 2020
  mutate (
    Therm_pref = case_when (
      mean_TB > 0 ~ "Warm",
      between (mean_TB, -3, 0) ~ "Cool",
      mean_TB < 0 ~ "Cold"
    )) %>%
  
  group_by (Therm_pref, scenario, model) %>%
  summarize (mean = mean(dist)/1000) %>% View()

# habitat indices effect on centroid change distance and bearing
hab_ind_lm_fun <- function (scen, var, hab_index) {
  
  lm_form <- as.formula (paste0 (var, "~", hab_index)) 
  lm_ob <- lm (lm_form, data = filter (cent_stats, scenario == scen))
  
  stats_df <- data.frame(
    hab_in = hab_index,
    resp_var = var,
    scenario = scen,
    r2 = round(summary(lm_ob)$adj.r.squared, 3),
    p_val = round(anova (lm_ob)$"Pr(>F)"[1], 3),
    int = round(lm_ob$coefficients[1],3),
    coef = round(lm_ob$coefficients[2], 3)
  )
  
}


centroids_ls <- expand.grid (scen = c(245, 585),
                             var = c("dist", "bearing"),
                             hab_index = c("mean_TB", "mean_Steno", "mean_depth")) %>%
  as.list()

centroids_stats_df <-  pmap_dfr (centroids_ls, hab_ind_lm_fun)
row.names (centroids_stats_df) <- NULL

centroids_stats_df 

## habitat indices effect on warm and cool edge shifts
# calculate distance between warm and cold edges too
edge_change <- centroid_change_dc %>%
  filter (!species %in% c("Squalus_acanthias", "Icelus_bicornis")) %>%
  mutate (warm_change = pred_warm - hist_warm,
          cold_change = pred_cold - hist_cold) %>%
  left_join (spp_list, by = "species")
  
hab_ind_lm_fun <- function (scen, var, hab_index) {
    
    lm_form <- as.formula (paste0 (var, "~", hab_index)) 

    
    lm_ob <- lm (lm_form, data = filter (edge_change, scenario == scen))
    
    stats_df <- data.frame(
      hab_in = hab_index,
      resp_var = var,
      scenario = scen,
      r2 = round(summary(lm_ob)$adj.r.squared, 3),
      p_val = round(anova (lm_ob)$"Pr(>F)"[1], 3),
      int = round(lm_ob$coefficients[1],3),
      coef = round(lm_ob$coefficients[2], 3)
    )
    
}

edge_ls <- expand.grid (scen = c(245, 585),
                             var = c("warm_change", "cold_change"),
                             hab_index = c("mean_TB", "mean_Steno", "mean_depth")) %>%
  as.list()

edge_stats_df <-  pmap_dfr (edge_ls, hab_ind_lm_fun)
row.names (edge_stats_df) <- NULL

edge_stats_df %>%
  filter (p_val < 0.05)
