## Summarize negbin model stats for paper
# 6/13/2021

library (tidyverse)

model_stats <- read_csv("Models/GAM_performance_Rug_nb.csv") %>%
  filter (!species %in% c("Squid", "Myctophidae"))


# Which are unsuitable?
unsuit <- model_stats %>%
  filter (MASE >= 1 | DM_GAM_p > 0.05)

# squalus acanthius and squid. M. aeglefinus p value is 0.08. 

# mean deviance explained
model_stats %>%
  filter (!species %in% unsuit$species) %>%
  summarise (mean = mean(dev_ex),
             range = range(dev_ex),
             sd = sd(dev_ex))
#   mean range    sd
# <dbl> <dbl> <dbl>
# 1 0.395  0.03 0.216
# 2 0.395  0.88 0.216

# including haddock
model_stats %>%
  filter (!species %in% c("Squalus_acanthius")) %>%
  summarise (mean = mean(dev_ex),
             range = range(dev_ex),
             sd = sd(dev_ex))
# mean range    sd
# <dbl> <dbl> <dbl>
# 1 0.407  0.03 0.224
# 2 0.407  0.91 0.224

# difference between full and naive dev exp
model_stats %>%
  filter (!species %in% c("Squalus_acanthius")) %>%
  mutate (diff_exp = dev_ex - dev_ex_naive) %>%
  summarise (mean = mean(diff_exp),
             range = range(diff_exp),
             sd = sd(diff_exp))
# mean range     sd
# <dbl> <dbl>  <dbl>
#   1 0.144  0.01 0.0942
# 2 0.144  0.51 0.0942

model_stats %>%
  filter (!species %in% c(unsuit$species)) %>%
  mutate (diff_exp = dev_ex - dev_ex_naive) %>%
  summarise (mean = mean(diff_exp),
             range = range(diff_exp),
             sd = sd(diff_exp))
#   mean range     sd
# <dbl> <dbl>  <dbl>
# 1 0.146  0.01 0.0949
# 2 0.146  0.51 0.0949

# Which species lose the most dev explained?
model_diff <- model_stats %>%
  filter (!species %in% c("Squalus_acanthias")) %>%
  mutate (diff_exp = dev_ex - dev_ex_naive) 

summary (model_diff$diff_exp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0100  0.0800  0.1200  0.1465  0.2000  0.5100 
sd (model_diff$diff_exp)

model_diff %>%
  arrange (desc (diff_exp)) %>%
  filter (diff_exp > .2)
# T. esmark, S vivi, M. merlan, L whiffi, S marinus, M molva, Loph pisc, C. monstrosa, E. gurnard, P virens, L. gracilis, L. seminudus
model_stats %>%
  filter (!species %in% c(unsuit$species)) %>%
  mutate (diff_exp = dev_ex - dev_ex_naive) %>%
  arrange (diff_exp) 
# lepto mac, C. microps, T. murrati, I bicornis, P platessa, A. dentic <= 5. 


## individual variable importance ----
var_imp <- read_csv("Models/var_imp_Rug_nb.csv")
var_imp %>%
  filter (!species %in% c("Squid", "Myctophidae", "Squalus_acanthias")) %>%
  group_by (Var) %>%
  summarise (mn = mean (Diff_f),
             range = range (Diff_f),
             sd = sd (Diff_f)) %>%
  arrange (desc (mn))

## habitat change ----
load ("Data/hab_change_rug_dc.RData") # made on CISER, has depth cropped for Molva and Hip plat

hab_change <- hab_change_rug_dc %>%
  filter (!species %in% c("Squid", "Myctophidae", "Squalus_acanthias")) 

hab_inc <- hab_change %>%
  group_by (species, scenario) %>% summarise (med = median (log10_foldchange)) %>% filter (med > 0)

hab_dec <- hab_change %>%
  group_by (species, scenario) %>% summarise (med = median (log10_foldchange)) %>% filter (med < 0)


# now i want to know if it decreased in all models. 
hab_change %>%
  filter (species %in% hab_inc$species) %>%
  group_by (species, scenario) %>%
  summarise (min = min (log10_foldchange)) %>%
  filter (min > 0) %>%
  group_by (scenario) %>% 
  summarise (count = n())
# 12 245, 20 585

hab_change %>%
  filter (species %in% hab_dec$species) %>%
  group_by (species, scenario) %>%
  summarise (max = max (log10_foldchange)) %>%
  filter (max < 0) %>%
  group_by (scenario) %>%
  summarise (count = n())
# 14 245, 16 585

# can I compare the spread?
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
tb2 <-  lm (log10_foldchange ~ mean_TB, data = filter (hab_change, scenario == 245))
summary(tb2)
# y = 0.06x + 0.05, adj r2 = 0.19, p < 0.001
cov2cor(vcov(tb2))[1, 2] # 0.286209

tb5 <-  lm (log10_foldchange ~ mean_TB, data = filter (hab_change, scenario == 585))
summary(tb5)
# y = 0.06x + 0.05, adj r2 = 0.27, p < 0.001
cov2cor(vcov(tb5))[1, 2] # also 0.286209???

# provide correlation
#https://stackoverflow.com/questions/45515595/summary-extract-correlation-coefficient

st2 <- lm (log10_foldchange ~ mean_Steno, data = filter (hab_change, scenario == 245))
summary (st2)
# adj r2 -0.005, p value 0.989
cov2cor(vcov(st2))[1, 2] # -0.9480727

st5 <- lm (log10_foldchange ~ mean_Steno, data = filter (hab_change, scenario == 585))
summary (st5)
# adj r2 -0.002, p value 0.6
cov2cor(vcov(st5))[1, 2]

d2 <- lm (log10_foldchange ~ mean_depth, data = filter (hab_change, scenario == 245))
summary (d2)
# adj r2 0.001, p value 0.26
cov2cor(vcov(d2))[1, 2] # -0.8357387

d5 <- lm (log10_foldchange ~ mean_depth, data = filter (hab_change, scenario == 585))
summary (d5)
# adj r2 0.006, p value 0.1
cov2cor(vcov(st5))[1, 2]



# Centroids ----
load ("Data/centroids_Rug_allscenarios_DepthCrop.RData")
spp_list <- read_csv("Data/species_eng.csv",
                     col_types = cols(Spp_ID = col_factor())
) %>%
  rename (species = sci_name_underscore)

cent_stats <- centroid_change_dc %>%
  filter (!species %in% c("Squid", "Myctophidae", "Squalus_acanthias")) %>%
  left_join (spp_list,by = "species" )
  
# summarise distance results by model and scenario 
centroid_change_dc %>%
  filter (!species %in% c("Squid", "Myctophidae", "Squalus_acanthias")) %>%
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

# ensemble mean
centroid_change_dc %>%
  filter (!species %in% c("Squid", "Myctophidae", "Squalus_acanthias")) %>%
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

# direction/bearing 
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

## warm vs cool edge
# calculate distance between warm and cold edges too
edge_change <- centroid_change_dc %>%
  filter (!species %in% c("Squid", "Myctophidae", "Squalus_acanthias")) %>%
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
