## Calculate and plot percent change in thermal suitable habitat
# JGM
# 9/9/2020

library (raster)
library (tidyverse)
library (beepr) # for letting me know when long runs finish
#library (rgeos)


# list of species
spp_list <- read_csv ("Data/species_eng.csv") %>%
  rename (species = sci_name_underscore) %>% # rename to match species column
  mutate (Common_name = ifelse (is.na (Common_name), 
                                       Scientific_name,
                                Common_name)
  ) # fill in any missing common names, for more legible graphs

# spp I ran models for
load ("Models/spp_Smooth_latlon.RData") 
borm_spp <- spp_Smooth_latlon %>%
  filter (!sci_name_underscore == "Myoxocephalus_scorpius")

# species that have landings data and that I ran models for 
landed_spp <- spp_list %>%
  filter (!is.na (Landed_name) & species %in% spp_Smooth_latlon$sci_name_underscore) %>%
  dplyr::pull (species)


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
# load and plot ----
# ==================
load ("Data/pred_hist_hab_df.RData")



# calculate prediction means for looking at overall change
hab_means <- pred_hist_hab  %>%
  filter (!species %in% c("Phycis_blennoides")) %>% # weird high values
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
  

# don't need to see all the individual species names--show top 10?
top_hab_spp <- hab_means %>%
  group_by (species) %>%
  summarize (max_hab = max (habitat)) %>%
  top_n (7, max_hab) # qual color palettes have 8-12 options

# plot total change in biomass 
png ("Figures/Hab_change_overall_biomass.png", width = 16, height = 9, res = 300, units = "in")
hab_means %>%
  mutate (species = ifelse (species %in% top_hab_spp$species, 
                            species,
                            "Other"),
          # set factor order?
          period = factor (period, levels = c("hist", 245, 585)),
          species = factor (species, levels = c (top_hab_spp$species, "Other"))
          ) %>%
  ggplot (aes (x = period, y = habitat, fill = species)) +
  geom_bar (stat = "identity") +
  scale_fill_brewer (palette = "Dark2") +
  labs (y = "Suitable thermal habitat") +
  theme_bw() +
  theme (
    axis.text.x = element_text (size = 14),
    axis.text.y = element_text (size = 12),
    axis.title = element_text (size = 14),
    plot.title = element_text (size = 16),
    legend.text = element_text (size = 14),
    legend.title = element_text (size = 14)
  ) +
  ggtitle ("Total suitable thermal habitat in Iceland's EEZ, all species")
dev.off()
  

# OR facet wrap 
hab_means %>% 

  ggplot (aes (x = period, y = log(habitat), fill = species)) +
  geom_bar (stat = "identity") +
  facet_wrap (~scenario)



# compile various metrics of change for looking at species-by-species change

# bring in GAM diagnostics for model suitability
MASE <- read_csv ("Models/GAM_performance_Borm_14_alltemp.csv")

hab_change <- pred_hist_hab %>%
  left_join (spp_list, by = "species") %>% 
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
    
    # calculate change
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
    # also reorder common_name for changing labels?
    Common_name = fct_reorder (Common_name, perc_change)
  ) 
    

# make a ggplot unit that I can manipulate for different uses
# can manipulate boxplot aesthetics, col and fill values, legend position, title. Labs command gets overridden, so put in subsequent

# legend position: c(0.83, 0.15) works well for percent change,c(0.6, 0.9) for log10 fold change 

hab_change_gg <- hab_change %>%
  # get rid of species that I didn't have enough data for model diagnostics
  filter (!is.na (Model_suitable)) %>%
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

# plot log10 change, by quota
png (file = "Figures/Log10_hab_change_quota_boxplot_Borm14_alltemp.png", width = 16, height = 9, units = "in", res = 300)
hab_change_gg +
  geom_boxplot (aes (y = species, 
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

# plot percent change, by TB
# png (file = "Figures/Percent_hab_change_TB_boxplot_Borm14_alltemp_commnames.png", width = 16, height = 9, units = "in", res = 300)

png (file = "Figures/Log10_hab_change_TB_boxplot_Borm14_alltemp_commnames.png", width = 16, height = 9, units = "in", res = 300)

hab_change_gg +
  geom_boxplot (aes (y = Common_name,
                     x = log10_foldchange,
                     color = Therm_pref, 
                     fill = fill_therm,
                     width = 0.85
                     )) +
  labs (fill = "",
        y = "",
        x = "Habitat change") +
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  scale_fill_manual (values = alpha (c("lightgray", "blue", "deepskyblue", "red2"), 0.5)) +
  theme (legend.position = c(0.6, 0.9)) +
  ggtitle ("Log x-fold change, 2000-2018 vs. 2061-2080") 

dev.off()



# plot portrait with TB for Cat

png (file = paste0("Figures/Percent_hab_change_TB_boxplot_portrait_", GAM, ".png"), width = 8.5, height = 11, units = "in", res = 300)






  
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
