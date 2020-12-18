## Calculate and plot percent change in thermal suitable habitat
# JGM
# 9/9/2020

library (raster)
library (tidyverse)
library (beepr) # for letting me know when long runs finish
#library (rgeos)


# list of species
spp_list  <- read_csv ("Data/species_eng.csv",
                       col_types = cols(
                         Spp_ID = col_factor()
                       )) %>%
  rename (species = sci_name_underscore) %>% #rename to match species column
  # only grab first option for species name (more legible graphs)
  # https://stackoverflow.com/questions/42565539/using-strsplit-and-subset-in-dplyr-and-mutate
  mutate(Common_name = sapply(str_split(Common_name, ","), function(x) x[1]))

          
   # fill in any missing common names, for more legible graphs

# spp I ran models for
load ("Models/spp_Smooth_latlon.RData") 
borm_spp <- spp_Smooth_latlon %>%
  filter (!sci_name_underscore == "Myoxocephalus_scorpius")


# ==================
# Calculate habitat change ----
# ==================

# Calculate amount of habitat in historical and predicted bricks from my rasters (created in Predict_ensemble_rasters.R)

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



# ==================
# load and plot habitat change ----
# ==================
load ("Data/pred_hist_hab_df.RData")

# list of suitable/used species. 49
load ("Models/spp_Smooth_latlon.RData")

borm_spp <- spp_Smooth_latlon %>% 
  filter (! sci_name_underscore %in% c("Myoxocephalus_scorpius", "Amblyraja_hyperborea", "Rajella_fyllae,", "Lumpenus_lampretaeformis", "Clupea_harengus", "Scomber_scombrus", "Mallotus_villosus"))

MASE <- read_csv ("Models/GAM_performance_Borm_14_alltemp.csv")
spp_suit <- MASE %>%
  filter (MASE_GAM < 1, DM_GAM_p < 0.05)

borm_suit <- borm_spp %>% filter (sci_name_underscore %in% spp_suit$species) %>% pull (sci_name_underscore)


# calculate prediction means for looking at overall change

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



# compile various metrics of change for looking at species-by-species change ----


# bring in icelandic names
isl_spp <- read_csv ("Data/Raw_data/species.csv",
                     col_types = cols(
                       tegund = col_factor()
                       )) %>%
  rename (Spp_ID = tegund) %>%
  left_join (spp_list, by = "Spp_ID") %>%
  dplyr::select (species, heiti)


hab_change <- pred_hist_hab %>%
  left_join (spp_list, by = "species") %>% 
  #left_join (isl_spp, by = "species") %>%
  left_join (MASE, by = "species") %>%
  

  mutate (Model_suitable = ifelse ( MASE_GAM < 1 & DM_GAM_p < 0.05, 
                              "1", 
                              "0"),
          Quota = factor(Quota),
    
    # create column to manipulate fill based on quota and model
    # https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette, show_col(hue_pal()(2))
    fill_quota = case_when (
      Model_suitable == 0 ~ "Model unsuitable",
      Model_suitable == 1 & Quota == 1 ~ "Quota",
      Model_suitable == 1 & Quota == 0 ~ "Non-Quota"
    ), 
    
    # column for fill based on thermal preference
    Therm_pref = case_when (
      mean_TB > 0 ~ "Warm",
      between (mean_TB, -3, 0) ~ "Cool",
      mean_TB < 0 ~ "Cold"
    ),
    
    fill_therm = case_when(
      Model_suitable == 0 ~ "*Model unsuitable",
      Model_suitable == 1 & mean_TB > 0 ~ "Warm",
      Model_suitable == 1 & between (mean_TB, -3, 0) ~ "Cool",
      Model_suitable == 1 &  mean_TB < 0 ~ "Cold"
    ),
    
    # calculate change -- experimenting with best way of presenting this. 
    log10_foldchange = log10 (pred_mean / hist_mean),
    log2_foldchange = log2 (pred_mean / hist_mean),
    perc_change = (pred_mean - hist_mean) / hist_mean * 100,
    simple_ratio = ifelse (pred_mean > hist_mean,
                           pred_mean/hist_mean,
                           -(hist_mean/pred_mean)
                           ),
    
    # reorder species values by amt of change
    #https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html
    # would depend on change variable of interest...?
    species = fct_reorder (species, perc_change),
    # also reorder common_name for changing labels
    Common_name = fct_reorder (Common_name, perc_change)
    # icelandic name is heiti
   # heiti = fct_reorder (heiti, perc_change)
  ) 
    

# make a ggplot unit that I can manipulate for different uses ----
# can manipulate boxplot aesthetics, col and fill values, legend position, title. Labs command gets overridden, so put in subsequent

# legend position: c(0.83, 0.15) works well for percent change,c(0.6, 0.9) for log10 fold change 

hab_change_gg <- hab_change %>%
  # filter landed species, using MFRI data loaded in plot_region_sector_exposure. remember to include lumpfish
  #filter (Spp_ID %in% ldgs$species | Spp_ID == 48) %>% 
  # get rid of species that I didn't have enough data for model diagnostics
  filter (!is.na (Model_suitable)) %>%
  #filter (Model_suitable == 1, species != "Leptoclinus_maculatus") %>%
  ggplot () +
  guides (color = FALSE) +
  geom_vline (xintercept = 0, lty = 2, col = "dark gray") +
  facet_wrap (~scenario) +
  theme_bw() +
  theme (
    axis.text.x = element_text (size = 14),
    axis.text.y = element_text (size = 12),
    axis.title = element_text (size = 14),
    plot.title = element_text (size = 16),
    strip.text = element_text (size = 14),
    legend.text = element_text (size = 14)
  ) 

# plot log10 change, by quota ----
png (file = "Figures/Log10_hab_change_quota_boxplot_Borm14_alltemp_commnames.png", width = 16, height = 9, units = "in", res = 300)

hab_change_gg +
  geom_boxplot (aes (y = Common_name, 
                     x = log10_foldchange,
                     color = Quota,
                     fill = fill_quota,
                     width = 0.85)) +
  labs (fill = "", y = "",
        x = "Habitat change") +
  scale_fill_manual (values = alpha (c("lightgray",  "#F8766D", "#00BFC4"), 0.5)) +
  ggtitle ("Log x-fold Habitat change, 2000-2018 vs. 2061-2080") + 
  theme (legend.position = c(0.6, 0.9))

dev.off()

# plot percent change, by TB ----
 #png (file = "Figures/Log10_hab_change_TB_boxplot_Borm14_commnames.png", width = 16, height = 9, units = "in", res = 300)

#png (file = "Figures/Log10_hab_change_TB_boxplot_Borm14_alltemp_islnames.png", width = 10, height = 8, units = "in", res = 150)
png (file = "Figures/Log10_hab_change_TB_boxplot_Borm14_commnames_simplified.png", width = 16, height = 9, units = "in", res = 300)

hab_change_gg +
  geom_boxplot (aes (y = Common_name,
                     x = log10_foldchange,
                     color = Therm_pref, 
                     #color = mean_Steno,
                     #fill = mean_Steno,
                     #fill = fill_therm,
                     width = 0.85
                     )) +
  labs (fill = "",
        y = "",
        x = "Log Habitat change") +
  #scale_color_binned (breaks = c (2, 4, 6), low = "orange", high = "blue" ) +
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  #scale_fill_manual (values = alpha (c("lightgray", "blue", "deepskyblue", "red2"), 0.5)) +
  #scale_fill_binned (breaks = c (2, 4, 6), low = "orange", high = "blue" ) +
  #theme (legend.position = c(0.6, 0.9)) +
  #theme (legend.position = c(0.65))
  #theme (legend.title = element_text (size = 18)) +
  ggtitle ("Log x-fold change, 2000-2018 vs. 2061-2080") 

dev.off()



# plot portrait with TB for Cat

png (file = paste0("Figures/Percent_hab_change_TB_boxplot_portrait_", GAM, ".png"), width = 8.5, height = 11, units = "in", res = 300)

# how many overall increased vs. decreased?
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

hab_change %>%
  filter (species %in% hab_dec$species) %>%
  group_by (species, scenario) %>%
  summarise (max = max (log10_foldchange)) %>%
  filter (max < 0) %>%
  group_by (scenario) %>%
  summarise (count = n())
# 16 species
  
# scatterplot habitat change vs. TB, steno ----

# median log10 change for each species
med_hab_change <- hab_change %>%
  group_by (species, scenario) %>%
  # use mutate to keep other columns
  mutate (md_change = median (log10_foldchange)) %>%
  distinct_at (vars (species, Common_name, heiti, mean_TB, mean_Steno, mean_depth, scenario, md_change)) 


# is there a rltp between hab change and TB/steno?
summary (lm (md_change ~ mean_TB, data = filter (med_hab_change, scenario == 245)))
# y = 0.04x -0.05, adj r2 = 0.14, p = 0.00123

summary (lm (md_change ~ mean_TB, data = filter (med_hab_change, scenario == 585)))
# y = 0.07x -0.04, adj r2 = 0.25, p = 1.626e-05

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


# bring in centroid change
load ("Data/centroids_Borm14_alltemp_allscenarios.RData")

# calculate distance between warm and cold edges too
edge_change_med <- centroid_change %>%
  mutate (warm_change = pred_warm - hist_warm,
          cold_change = pred_cold - hist_cold) %>%
  group_by (species, scenario) %>%
  summarize (med_warm = median (warm_change, na.rm = TRUE),
             med_cold = median (cold_change, na.rm = TRUE)) %>%
  left_join (spp_list, by = "species")

cent_change_mn <- centroid_change %>%
  
  group_by (species, scenario) %>%
  summarize (mean_dist = mean(dist),
             mean_bearing = mean(bearing)) %>%
  mutate (bearing_360 = ifelse (mean_bearing < 0,
                                mean_bearing + 360,
                                mean_bearing)) %>%
  left_join (spp_list, by = "species")


# is there a rltp between edge change and TB/steno?
summary (lm (med_cold ~ mean_TB, data = filter (edge_change_med, scenario == 245)))
# p = 0.9 for warm, but y = 0.09x + 0.03, adj r2 0.13, p = 0.006 for med_cold

summary (lm (med_cold ~ mean_TB, data = filter (edge_change_med, scenario == 585)))
# p = 0.09 for warm, y = 0.09x + 0.11, adj r2 = 0.19, p = 0.001

# this means that warmer species see their cold edge push upward. 

png ("Figures/scatter_TB_coldedge.png")
edge_change_med %>%
  ggplot (aes (x = mean_TB, y = med_cold)) +
  geom_point () +
  geom_text (aes(label = Common_name), data = filter (edge_change_med, abs (med_cold) > 0.5)) +
  geom_vline (xintercept = 0, lty = 2) +
  geom_hline (yintercept = 0, lty = 2) +
  geom_smooth (method = lm) +
  facet_wrap (~scenario) +
  theme_bw()

dev.off()

# would expect larger Steno to be smaller habitat change
summary (lm (med_cold ~ mean_Steno, data = filter (edge_change_med , scenario == 245)))
# warm, p = 0.6, cold p = 0.2 

summary (lm (med_cold ~ mean_Steno, data = filter (edge_change_med , scenario == 585)))
# warm p = 0.9, cold p = 0.3. not better with interaction or both terms. 

summary (lm (med_warm ~ mean_depth, data = filter (edge_change_med , scenario == 245)))
# ad 0.03, p 0.13 cold, ad -0.02, p 0.71 warm

summary (lm (med_warm ~ mean_depth, data = filter (edge_change_med , scenario == 585)))
# ad 0.03, p = 0.13 cold, ad -0.02, p = 0.60 warm

# plot amount of cold and warm change
centroid_change %>%
  filter (species %in% borm_suit) %>%
  mutate (warm_change = pred_warm - hist_warm,
          cold_change = pred_cold - hist_cold) %>%
  ggplot (aes (x = warm_change, y = fct_reorder(species, warm_change))) +
  geom_boxplot () +
  facet_wrap (~scenario) +
  theme_bw ()

cent_hab <- centroid_change %>%
  filter (species %in% borm_suit) %>%
  mutate (warm_change = pred_warm - hist_warm,
          cold_change = pred_cold - hist_cold) %>%
  left_join (hab_change, by = c ("species", "model", "scenario"))
  
cent_hab %>%
  ggplot (aes(x = log10_foldchange, y = warm_change, col = mean_TB)) +
  geom_point () +
  geom_smooth() +
  theme_bw()

cent_hab %>%
  ggplot (aes(x = log10_foldchange, y = cold_change, col = mean_TB)) +
  geom_point () +
  geom_smooth() +
  theme_bw()

# species that lost habitat were likely to see their warm edge increase. no rltp with species that incresed habitat. that means a northward movment, cropping bottom. 

# species that gained habitat see their cold edge increase, pushing upward. not a lot of change in species that lost habitat, except a cluster of 5. G argentatus and l. gracilis cnrm 245

summary (lm (warm_change ~ log10_foldchange, data = cent_hab))
# adj r2 0.14, y = 0.07x - 0.35 log change, p <<< 0

summary (lm (warm_change ~ log10_foldchange, data = filter(cent_hab, warm_change < 0)))
# sig, but positive rltp
summary (lm (warm_change ~ log10_foldchange, data = filter(cent_hab, warm_change > 0)))
# sig, but negative rltp


# centroid change
# neither distance nor bearing significant for steno, TB, depth
summary (lm (mean_bearing ~ mean_TB * mean_depth, data = filter (cent_change_mn, scenario == 245)))

# TB * depth is significant for bearing, not depth. adj r2 0.14. TB very negative, depth pos, interaction positive. means that the effect of TB on bearing change increases with unit increase in depth. deeper species see a more pronounced thermal relationship with bearing. 

# https://biologyforfun.wordpress.com/2014/04/08/interpreting-interaction-coefficient-in-r-part1-lm/

#This means that warmer species move more west (negative bearing, this is with -180-180 bearings), which isn't actually what I'm seeing in the compass plots. 
summary (lm (bearing_360 ~ mean_TB, data = filter (cent_change_mn, scenario == 245)))
  
# also has a negative rltp with 360 bearing??

# also, there's a significant negative rltp btween mean_TB and mean_depth

 # # SDMTools patchstat test ----
library (SDMTools)

# https://rdrr.io/cran/SDMTools/man/PatchStat.html
# https://gis.stackexchange.com/questions/210149/patch-stat-r-for-loop-question

# take mean first? can it do a raster brick?
br_hist <- brick (file.path ("Models/Prediction_bricks", hist_files[i]))
br_245 <- brick (file.path ("Models/Prediction_bricks", pred_files_245[i]))
br_585 <- brick (file.path ("Models/Prediction_bricks", pred_files_585[i]))

hist_ps <- PatchStat (subset(br_hist, 1), latlon = TRUE) # works, but only gives me patchID

hist_ccl_test <- ConnCompLabel()



## Kristin rgeos method ----

  
  
  # # Kleisner 2017 selects all points with values greater than one sd above the mean, then computes area of those kernels. but what mean?? For now, will take overall mean of historical period--just one value. calc is significantly faster than overlay for this (1.4 vs 9s for test brick). 
  # 
  # # rgeos gArea vs. geosphere areaPolygon??
  # # https://gis.stackexchange.com/questions/264517/why-does-projection-not-affect-answer-calculated-by-rgeosgarea
  # 
  # hist_mn <- cellStats(calc (br, fun = mean), stat = "mean")
  # hist_sd <- cellStats(calc (br, fun = mean), stat = "sd")
  # 
  # # suitable habitat value is mean + one sd
  # suit_val <- hist_mn + hist_sd
  # 
  # # https://stackoverflow.com/questions/45428129/how-to-subset-a-raster-based-on-grid-cell-values
  # 
  # # convert brick to points df, subset values > suit_val
  # hist_suit <- rasterToPolygons(br[[100]], fun = function (x){ x > suit_val})
  # projection (hist_suit) <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
  # 
  # # http://taromieno.netlify.com/post/raster_to_polygons_2018/
  # hist_suit_sp <- as (br, 'SpatialPolygonsDataFrame') # this works for brick, but can't do subset?
  # 
  # # https://www.rdocumentation.org/packages/taRifx/versions/1.0.3/topics/subsetSPDF
  # x <- subsetSPDF (hist_suit_sp, x > suit_val)
  # 
  # # maybe this would be helpful?
  # # https://cengel.github.io/R-spatial/spatialops.html
  # 
  # hist_area <- gArea (hist_suit) # this gives 10.7, no idea what the units are
  # 
  # library (geosphere)
  # area_geosphere <- areaPolygon(hist_suit)/1e6 # this gives 174 values (for 174 cells > suit_val)
  # 
  # # just record number of cells above suit val for now?
  # dim(hist_suit)[1]
  # 
  # 
  # 
  # # get values of each layer of brick
  # hist_vals <- getValues (br[[1]]) # 33600
  # length (which (hist_vals > suit_val))
