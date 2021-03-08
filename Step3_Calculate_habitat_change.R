## Calculate change in thermal suitable habitat projections
# JGM
# 9/9/2020

library (raster)
library (tidyverse)
library (beepr) # for letting me know when long runs finish


# list of species
spp_list  <- read_csv ("Data/species_eng.csv",
                       col_types = cols(
                         Spp_ID = col_factor()
                       )) %>%
  rename (species = sci_name_underscore) %>% #rename to match species column
  # only grab first option for species name (more legible graphs)
  # https://stackoverflow.com/questions/42565539/using-strsplit-and-subset-in-dplyr-and-mutate
  mutate(Common_name = sapply(str_split(Common_name, ","), function(x) x[1]))


# ==================
# Calculate habitat change ----
# ==================

# spp I ran models for
load ("Models/spp_Smooth_latlon.RData") 
borm_spp <- spp_Smooth_latlon %>%
  filter (!sci_name_underscore == "Myoxocephalus_scorpius")



# Calculate amount of habitat in historical and predicted bricks from my rasters (created in Step2_Predict_ensemble_rasters.R)

# doing both historical and future at the same time takes >2x longer, because repeating the historical one 4-5 times. Instead, make separate df for historical and future, and combine with left_join. 

# historical function ----
sum_habitat_hist <- function (sci_name) {
  
  # load raster brick for historical period
  hist_br <- brick (file.path ("Models/Prediction_bricks", 
                               paste(sci_name, "Borm_14_alltemp", "2000_2018.grd", sep = "_"))
  )
  
  # take sum of all the cells for each time step, for Morely et al. 2018 method
  sum_hist <- cellStats(hist_br, stat = 'sum') # this should have 228 values, for each month 2000-2018
  
  df <- data.frame (species = sci_name,
                    hist_mean = mean (sum_hist),
                    hist_med = median (sum_hist),
                    hist_sd = sd (sum_hist))
  
}



# apply function, base r method:

landed_hist_hab <- do.call(rbind, lapply(landed_spp, sum_habitat_hist, GAM = "Borm_14_alltemp"))

# purr method:
system.time (hist_hab <- map_df (borm_spp$sci_name_underscore, sum_habitat_hist)) # 20s for landed_spp, 95s for all spp

# future function ----
sum_habitat_future <- function (sci_name, CM, scenario) {
  # CM is 4-letter lowercase climate model abbreviation
  # scenario is 245 or 585
  pred_br <- brick (file.path ("Models/Prediction_bricks", 
                               paste(sci_name, "Borm_14_alltemp", CM, scenario, "2061_2080.grd", sep = "_")
                               )
  )
  sum_pred <- cellStats(pred_br, stat = 'sum')
  
  df <- data.frame (species = sci_name,
                    model = CM,
                    scenario = scenario, 
                    pred_mean = mean (sum_pred),
                    pred_med = median (sum_pred),
                    pred_sd = sd (sum_pred)
                    )

  
} # end pred function 


# list of all the possible combinations of species, CM, and scenario. I couldn't figure out how to use cross and filter to get rid of the CM2.6 and 245 combination, but can do with expand_grid 

CM_list <- c("gfdl", "cnrm", "ipsl", "mohc", "CM26")

cm_expand <- expand_grid (sci_name = borm_spp$sci_name_underscore,
                          CM = CM_list,
                          scenario = c(245, 585)) %>%
  filter (!(CM == "CM26" & scenario == 245)) %>% 
  as.list() # convert to list to feed to pmap


system.time(pred_hab <- pmap_dfr (cm_expand, sum_habitat_future)); beep() 
# 233 seconds for landed spp, 530 seconds for all spp

# combine and save
pred_hist_hab <- pred_hab %>%
  left_join (hist_hab, by = "species")
save (pred_hist_hab, file = "Data/pred_hist_hab_df.RData")


# calculate habitat change ----


# Looking at various metrics. Percent change is the most common, but hard to interpret when negative and positive changes are very large. Makes positive changes look way more important than negative changes. 

hab_change <- pred_hist_hab %>%
  left_join (spp_list, by = "species") %>% 
  mutate (log10_foldchange = log10 (pred_mean / hist_mean),
          log2_foldchange = log2 (pred_mean / hist_mean),
          perc_change = (pred_mean - hist_mean) / hist_mean * 100,
          simple_ratio = ifelse (pred_mean > hist_mean,
                                 pred_mean/hist_mean,
                                 -(hist_mean/pred_mean)
                                 )
          )

# save
save (hab_change, file = "Data/hab_change_df.RData")


# count how many overall increased vs. decreased ----
# only look at 47 suitable species, defined in Step1_Fit_GAMs_function.R
load ("Models/spp_Borm_suit.RData")

hab_inc <- hab_change %>% 
  filter (species %in% borm_suit) %>%
  group_by (species, scenario) %>% summarise (med = median (log10_foldchange)) %>% filter (med > 0)

hab_dec <- hab_change %>% 
  filter (species %in% borm_suit) %>%
  group_by (species, scenario) %>% summarise (med = median (log10_foldchange)) %>% filter (med < 0)

# squid and p. blennoides change based on scenario. 

# now i want to know if it decreased in all models. 
hab_change %>%
  filter (species %in% hab_inc$species) %>%
  group_by (species, scenario) %>%
  summarise (min = min (log10_foldchange)) %>%
  filter (min > 0) %>%
  group_by (scenario) %>%
  summarise (count = n())
# 14 spp in 245, 18 in 585
# among borm_suit, 13, in 245, 15 in 585

hab_change %>%
  filter (species %in% hab_dec$species) %>%
  group_by (species, scenario) %>%
  summarise (max = max (log10_foldchange)) %>%
  filter (max < 0) %>%
  group_by (scenario) %>%
  summarise (count = n())
# 16 species
# borm_suit, 15, in 245, 16 585

# ==================
# calculate prediction means for looking at overall change ----
# ==================


# manipulate habitat predictions so I have a long/tidy df with 3 columns: species name, period (hist, 245, 585), and mean habitat. Mean for historical is the mean for 2000-2018; mean for future is a mean of means, 2060-2080 for all climate models
hab_means <- pred_hist_hab  %>%
  filter (!species %in% c("Phycis_blennoides"), species %in% borm_suit) %>% # weird high values due to sinusoid depth rltp. also remove pelagic/non-suitable.
  # could I use map here instead?
  group_by (species, scenario) %>%
  summarise (hab_future = mean(pred_mean),
             hab_hist = first (hist_mean)) %>% 
  pivot_wider (names_from = scenario,
               values_from = hab_future, 
               names_prefix = "hab_") %>%
  pivot_longer (!species, 
                names_to = "period", 
                names_prefix = "hab_",
                values_to = "habitat") 
# 48 total bc also took out p. blennoides


# For a more legible graph, only show the top few species
top_hab_spp <- hab_means %>%
  group_by (species) %>%
  summarize (max_hab = max (habitat)) %>%
  # join to spp_list to use common_names
  left_join (spp_list, by = "species") %>% 
  top_n (7, max_hab) # qual color palettes have 8-12 options


# plot total change in biomass  ----
png ("Figures/Hab_change_overall_biomass_commnames_suitable.png", width = 16, height = 9, res = 300, units = "in")

# stacked barplot with columns for historical, 245, and 585. color is species habitat, total height of bar is total amount of suitable habitat. 

hab_means %>%
  
  # add common names
  left_join (spp_list, by = "species") %>% 
  
  mutate (Common_name = ifelse (species %in% top_hab_spp$species, 
                                Common_name,
                                "Other"),
          # set factor order 
          period = factor (period, levels = c("hist", 245, 585)),
          Common_name = factor (Common_name, levels = c (top_hab_spp$Common_name, "Other"))
  ) %>%
  ggplot (aes (x = period, y = habitat, fill = Common_name)) +
  geom_bar (stat = "identity") +
  scale_fill_brewer (palette = "Dark2", name = "Species") +
  labs (y = "Suitable thermal habitat", x = "Period") +
  theme_bw() +
  theme (
    axis.text.x = element_text (size = 18),
    axis.text.y = element_text (size = 16),
    axis.title = element_text (size = 18),
    plot.title = element_text (size = 24),
    legend.text = element_text (size = 16),
    legend.title = element_text (size = 18)
  ) +
  ggtitle ("Total suitable thermal habitat in Iceland's EEZ, all species")
dev.off()


# OR facet wrap 
hab_means %>% 
  
  ggplot (aes (x = period, y = log(habitat), fill = species)) +
  geom_bar (stat = "identity") +
  facet_wrap (~scenario)


# ================== 
# Statistical relationships btw hab_change and thermal preference ----
# ==================

# scatterplot habitat change vs. TB, steno ----

# median log10 change for each species
med_hab_change <- hab_change %>%
  group_by (species, scenario) %>%
  # use mutate to keep other columns
  mutate (md_change = median (log10_foldchange)) %>%
  distinct_at (vars (species, Common_name,  mean_TB, mean_Steno, mean_depth, scenario, md_change)) 


# is there a rltp between hab change and TB/steno?
summary (lm (md_change ~ mean_TB, data = filter (med_hab_change, scenario == 245)))
# y = 0.03x -0.05, adj r2 = 0.03, p = 0.13

summary (lm (md_change ~ mean_TB, data = filter (med_hab_change, scenario == 585)))
# y = 0.06x -0.04, adj r2 = 0.12, p = 0.009765

# would expect larger Steno to be smaller habitat change, but not significant
summary (lm (md_change ~ mean_Steno, data = filter (med_hab_change, scenario == 245)))
# y = -0.03x +0.06, adj r2 = 0.01, p = 0.18
# depth: adj r2 -0.007, p = 0.47

summary (lm (md_change ~ mean_depth, data = filter (med_hab_change, scenario == 585)))
# y = -0.04x +0.12, adj r2 = 0.02, p = 0.14
# depth adj r2 -0.02, p = 0.9

# not sig with depth. 
# not sig with interaction or both variables.
summary (lm (md_change ~ mean_TB * mean_Steno, data = filter (med_hab_change, scenario == 245)))
# tb is positive, not sig. steno is negative, not sig. interaction is negative, not sig. overall r2 0.17, p = 0.009. negative interaction means that warmer species saw a habitat increase, whereas more general species saw more habitat decrease. as you get more general, the effect of tb on habitat change decreases.  

png ("Figures/scatter_TB_log10change.png")
  med_hab_change %>%
  ggplot (aes (x = mean_TB, y = md_change)) +
  geom_point () +
  geom_text (aes(label = Common_name)) +
  geom_vline (xintercept = 0, lty = 2) +
  geom_hline (yintercept = 0, lty = 2) +
  geom_smooth (method = lm) +
  facet_wrap (~scenario) +
  theme_bw()

dev.off()

