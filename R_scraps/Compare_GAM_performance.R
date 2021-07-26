## Compare GAM performance
# 5 10 21
# JGM

library (tidyverse)

# look at compare_performance but would need to do for each species
#https://easystats.github.io/performance/reference/compare_performance.html
library (performance)

load("Models/Borm_14_alltemp/Gadus_morhua_PA.Rdata")
Borm_orig_PA <- gam_PA

load ("Models/rugos3m/Gadus_morhua_PA.Rdata")
rug_PA <- gam_PA

compare_performance (Borm_orig_PA, rug_PA)

# Compiled separate csvs of GAM performance. Put them all in one place, with values relative to the main model, Borm_14_alltemp

# Models to compare: Borm_14_alltemp, Borm_14_alltemp_btmin, Depth_Temp, Depth_Temp_btmin, Borm_14_alltemp_intn_test, Smooth_latlon_drop_year

Borm_14_alltemp_perf <- read_csv ("Models/GAM_performance_Borm_14_alltemp_NOFAKEZEROS.csv") %>%
  mutate (Suitable = ifelse (MASE_GAM < 1 & DM_GAM_p < 0.05, 1, 0),
          model = "Borm_14_alltemp") %>%
  dplyr::select (model, species, Suitable, PA_dev, PA_AIC, LB_dev, LB_AIC, MAE_gam)


Borm_14_alltemp_btmin_perf <- read_csv ("Models/GAM_performance_Borm_14_alltemp_btmin_NOFAKEZEROS.csv")%>%
  mutate (Suitable = ifelse (MASE_GAM < 1 & DM_GAM_p < 0.05, 1, 0),
          model = "Borm_14_alltemp_btmin") %>%
  dplyr::select (model,species, Suitable, PA_dev, PA_AIC, LB_dev, LB_AIC, MAE_gam)

Depth_temp_perf <- read_csv("Models/GAM_performance_Depth_Temp.csv") %>%
  mutate (Suitable = ifelse (MASE_GAM < 1 & DM_GAM_p < 0.05, 1, 0),
          model = "Depth_temp") %>%
  dplyr::select (model,species, Suitable, PA_dev, PA_AIC, LB_dev, LB_AIC, MAE_gam)

Depth_temp_btmin_perf <- read_csv("Models/GAM_performance_Depth_Temp_btmin.csv") %>%
  mutate (Suitable = ifelse (MASE_GAM < 1 & DM_GAM_p < 0.05, 1, 0),
          model = "Depth_temp_btmin") %>%
  dplyr::select (model,species, Suitable, PA_dev, PA_AIC, LB_dev, LB_AIC, MAE_gam)

Borm_14_intn_perf <- read_csv("Models/GAM_performance_Borm_14_alltemp_intn_NOFAKEZEROS.csv") %>%
  mutate (Suitable = ifelse (MASE_GAM < 1 & DM_GAM_p < 0.05, 1, 0),
          model = "Borm_14_intn") %>%
  dplyr::select (model,species, Suitable, PA_dev, PA_AIC, LB_dev, LB_AIC, MAE_gam)

Borm_14_noDepth_perf <- read_csv ("Models/GAM_performance_Borm_14_nodepth_NOFAKEZEROS.csv")%>%
  mutate (Suitable = ifelse (MASE_GAM < 1 & DM_GAM_p < 0.05, 1, 0),
          model = "Borm_14_noDepth") %>%
  dplyr::select (model,species, Suitable, PA_dev, PA_AIC, LB_dev, LB_AIC, MAE_gam)

Smooth_latlon_perf <- read_csv("Models/GAM_performance_Smooth_latlon_drop_year.csv") %>%
  mutate (Suitable = ifelse (MASE_GAM < 1 & DM_GAM_p < 0.05, 1, 0),
          model = "Smooth_latlon") %>%
  dplyr::select (model,species, Suitable, PA_dev, PA_AIC, LB_dev, LB_AIC, MAE_gam)


Rug_perf <- read_csv ("Models/GAM_performance_rugos3m.csv") %>%
  mutate (Suitable = ifelse (MASE_GAM < 1 & DM_GAM_p < 0.05, 1, 0),
          model = "Rugosity") %>%
  dplyr::select (model,species, Suitable, PA_dev, PA_AIC, LB_dev, LB_AIC, MAE_gam)

# df_rbind <- rbind (Borm_14_alltemp_perf, Borm_14_alltemp_btmin_perf, Depth_temp_perf, Depth_temp_btmin_perf, Borm_14_intn_perf, Smooth_latlon_perf) %>%
#   pivot_wider (-c(species),
#                names_from )

# what if I do similar to hab change, where i have one base df, and then another for the "scenarios"
comp_rbind <-rbind (Borm_14_alltemp_btmin_perf, Depth_temp_perf, Depth_temp_btmin_perf, Borm_14_intn_perf, Borm_14_noDepth_perf, Smooth_latlon_perf) %>%
  #rename_at(vars(-c(model, species)),function(x) paste0(x,".2")) %>%
  pivot_longer (!c(species, model), 
                names_to = "Metric",
                values_to = "Value_2")

comp_join <- Borm_14_alltemp_perf %>%
  pivot_longer (!c(species, model), 
                names_to = "Metric",
                values_to = "Value_1") %>%
  left_join (comp_rbind, by = c("species", "Metric")) %>%
  mutate (Diff = Value_2 - Value_1) %>%
  #select (-c(model.x, Value_1, Value_2)) %>%
  pivot_wider (-c(model.x, Value_2),
               names_from = model.y, 
               values_from = Diff) %>%
  rename (Borm14_model_base_value = Value_1) %>%
  arrange (Metric, species)


write.csv (comp_join, file = "Models/GAM_performance_comparison_NOFAKEZEROS.csv", row.names = FALSE)

# also just do averages? even p-value differences??

# Compare rugosity and borm_14
borm_long <- Borm_14_alltemp_perf %>%
  filter (species %in% Rug_perf$species) %>%
  dplyr::select (species, PA_dev, PA_AIC, LB_dev, LB_AIC, MAE_gam, Suitable) %>%
  pivot_longer (PA_dev:LB_AIC,
                names_to = c("Part", "metric"),
                names_sep = "_", ) %>%
  pivot_wider (names_from = "metric",
               values_from = "value") %>%
  mutate (Model = "Bormicon region")

rug_long <- Rug_perf %>%
  dplyr::select (species, PA_dev, PA_AIC, LB_dev, LB_AIC, MAE_gam, Suitable) %>%
  pivot_longer (PA_dev:LB_AIC,
                names_to = c("Part", "metric"),
                names_sep = "_", ) %>%
  pivot_wider (names_from = "metric",
               values_from = "value")%>%
  mutate (Model = "Rugosity")

dt_long  <-  Depth_temp_perf %>%
  filter (species %in% Rug_perf$species) %>%
  dplyr::select (species, PA_dev, PA_AIC, LB_dev, LB_AIC, MAE_gam, Suitable) %>%
  pivot_longer (PA_dev:LB_AIC,
                names_to = c("Part", "metric"),
                names_sep = "_", ) %>%
  pivot_wider (names_from = "metric",
               values_from = "value") %>%
  mutate (Model = "Depth temp only")

dxt_long <- Borm_14_intn_perf %>%
  filter (species %in% Rug_perf$species) %>%
  dplyr::select (species, PA_dev, PA_AIC, LB_dev, LB_AIC, MAE_gam, Suitable) %>%
  pivot_longer (PA_dev:LB_AIC,
                names_to = c("Part", "metric"),
                names_sep = "_", ) %>%
  pivot_wider (names_from = "metric",
               values_from = "value") %>%
  mutate (Model = "Depth temp interaction")

rug_compare <- rbind (borm_long, rug_long, dt_long, dxt_long) %>%
  relocate (c(MAE_gam, Suitable), .after = AIC) %>%
  arrange (species, Part, AIC)

save (rug_compare, file = "Models/Borm_Rug_TD_compare10.RData")





# this didn't work, but other option maybe
# https://stackoverflow.com/questions/50660577/subtract-rows-of-data-frames-column-wise-preserving-multiple-factor-column

#######################################################################################################################


# is there something weird with fake zeros!!
borm <- mfri_pred %>% dplyr::select(sample_id, bormicon_region)
mfri_abun %>%
  left_join (borm, by = "sample_id") %>%
  group_by (species) %>%
  summarise (n_borm = length (unique (bormicon_region))) %>% filter (species == 71)
  filter (n_borm < 12) %>% 
  rename (Spp_ID = species) %>%
  left_join (spp_list, by = "Spp_ID") %>%
  select (Spp_ID, sci_name_underscore, n_borm) %>% 
  filter (sci_name_underscore %in% spp_Smooth_latlon$sci_name_underscore) %>% 
  arrange (sci_name_underscore) %>% View()
