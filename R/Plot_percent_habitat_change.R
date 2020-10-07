## Calculate and plot percent change in thermal suitable habitat
# JGM
# 9/9/2020

library (raster)
library (tidyverse)
#library (rgeos)


hist_files <- list.files (path = "Models/Prediction_bricks", pattern = ".*Smooth_latlon.*2000_2018.grd")
pred_files_245 <- list.files (path = "Models/Prediction_bricks", pattern = ".*Smooth_latlon_245_2061_2080.grd")
pred_files_585 <- list.files (path = "Models/Prediction_bricks", pattern = ".*Smooth_latlon_585_2061_2080.grd")


hist_files <- list.files (path = "Models/Prediction_bricks", pattern = ".*Depth_Temp.*2000_2018.grd")
pred_files_245 <- list.files (path = "Models/Prediction_bricks", pattern = ".*Depth_Temp_245_2061_2080.grd")
pred_files_585 <- list.files (path = "Models/Prediction_bricks", pattern = ".*Depth_Temp_585_2061_2080.grd")



# list of species names in Models folder
#load ("Models/spp_Smooth_latlon.RData")
load ("Models/spp_Depth_Temp.RData") # td_spp_names. same, just squid and myctophids

# make data table: species, scenario, period, mean, sd

# just morely method for now
hab_change <- data.frame ()

for (i in 1:length(hist_files)) {
  
  # load files as raster brick. reasonably sure they're in the same order
  br_hist <- brick (file.path ("Models/Prediction_bricks", hist_files[i]))
  br_pred_245 <- brick (file.path ("Models/Prediction_bricks", pred_files_245[i]))
  br_pred_585 <- brick (file.path ("Models/Prediction_bricks", pred_files_585[i]))
  
  #projection (br) <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
  
  # Morely et al. 2018 just use sum of values for each year, then takes mean
  sum_hist <- cellStats(br_hist, stat = 'sum')
  mean_hist <- mean (sum_hist)
  #var_hist <- var (sum_hist)
  
  sum_pred_245 <- cellStats(br_pred_245, stat = 'sum')
  mean_pred_245 <- mean (sum_pred_245)
  #var_pred <- var (sum_pred)
  
  sum_pred_585 <- cellStats(br_pred_585, stat = 'sum')
  mean_pred_585 <- mean (sum_pred_585)
  
  # break up elements of filename to make it easier to parse
  file_split <- strsplit (hist_files[i], "_")[[1]]
  
  # fill out temporary df with two rows for both scenarios
  
  hab_df <- data.frame (
    # species name will be first two elements of file_split. except squid and myctophidae
    species = ifelse (file_split[1] %in% c ("Squid", "Myctophidae"), 
                      rep (file_split[1], 2),
                      rep (paste (file_split[1], file_split[2], sep = "_"), 2)
                      ),
    
    # scenario will be third from last [models might have variable descriptors]
    scenario = c(245, 585),
    
    # perc change will be (pred - hist)/hist * 100
    perc_change_Morely =  c(100 * (mean_pred_245 - mean_hist)/mean_hist,
                            100 * (mean_pred_585 - mean_hist)/mean_hist)
    
    )
  
  hab_change <- rbind (hab_change, hab_df)

} # end for loop

# save for now
save (hab_change, file = "Data/perc_hab_change_Morely_smoothlatlon.RData")
save (hab_change, file = "Data/perc_hab_change_Morely_depthtemp.RData")

# load and plot ----
load ("Data/perc_hab_change_Morely_smoothlatlon.RData")

head (hab_change)

# fix underscore issue for smoothlatlon
# not working with mutate for some reason
hab_change$species[which (hab_change$species == "Squid_Smooth")] <- "Squid"
hab_change$species[which (hab_change$species == "Myctophidae_Smooth")] <- "Myctophidae"

summary (hab_change$perc_change_Morely) # some extreme high values

hab_change %>%
  arrange (desc (perc_change_Morely)) %>%
  head (10)

hab_change %>%
  ggplot (aes (x = perc_change_Morely, fill = scenario)) +
  geom_density()

# order based on 245 perc change
# https://stackoverflow.com/questions/28190435/changing-factor-levels-with-dplyr-mutate
# for just overall order based on perc_change, use aes (x = reorder(species, perc_change_Morely))
spp_order <- hab_change %>%
  filter (perc_change_Morely < 20000, scenario == 245) %>%
  arrange (perc_change_Morely) %>%
  # add all the names...
  rename (sci_name_underscore = species) %>%
  left_join (spp_list, by = "sci_name_underscore") %>%
  rename (species = sci_name_underscore) %>%
  mutate (name = ifelse (!is.na (Landed_name),
                                 Landed_name, 
                                 species))
 

# fill based on quota
quota_status <- read_csv ("Data/species_eng.csv") %>%
  dplyr::select (sci_name_underscore, Quota, Landed_name) %>%
  rename (species = sci_name_underscore)

# fill based on certainty
smoothLL_MASE <- read_csv ("Models/GAM_diagnostics_all_spp.csv") %>%
  rename (species = Species)


png (file = "Figures/Percent_hab_change_by_quota_SmoothLatlon_6000.png", width = 16, height = 9, units = "in", res = 300)
hab_change %>% 
  left_join (quota_status, by = "species") %>% 
  left_join (smoothLL_MASE, by = "species") %>% 
  mutate (Model_suitable = ifelse (
    MASE_GAM < 1 & DM_GAM_p < 0.05, 
    "1", 
    "0"
  )) %>%
  filter (perc_change_Morely < 20000, !is.na (Model_suitable)) %>%
  #filter (perc_change_Morely < 750) %>%

  mutate (species = factor(species, levels = spp_order$species),
          Quota = factor(Quota),
          fill_col = case_when(
            MASE_GAM >= 1 | DM_GAM_p >= 0.05 ~ "Model unsuitable",
            MASE_GAM < 1 & DM_GAM_p < 0.05 & Quota == 1 ~ "Quota",
            MASE_GAM < 1 & DM_GAM_p < 0.05 & Quota == 0 ~ "Non-Quota"
            )
          ) %>% 
 
  ggplot (aes (x = species, 
               y = perc_change_Morely,
               color = Quota, 
               fill = fill_col,
               width = 0.85)) +
  scale_fill_manual (values = c("lightgray",  "#F8766D", "#00BFC4")) +
  labs (fill = "",
        x = "",
        y = "Percent habitat change") +
  guides (color = FALSE) +
  geom_bar (stat = "identity") + 
  coord_flip() + 
  theme_bw() +
  facet_wrap (~scenario) +
  ggtitle ("Percent habitat change, 2000-2018 vs 2061-2080") +
  theme (
    axis.text = element_text (size = 16),
    axis.title = element_text (size = 18),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 18),
    legend.text = element_text (size = 18)
  )
dev.off()

# plot starting with herring, cut off at 750
spp_order <- hab_change %>%
  filter (perc_change_Morely < 750, scenario == 245) %>%
  arrange (perc_change_Morely)

png (file = "Figures/Percent_hab_change_by_quota_SmoothLatlon.png", width = 18, height = 12, units = "in", res = 300)
hab_change %>% 
  left_join (quota_status, by = "species") %>% 
  left_join (smoothLL_MASE, by = "species") %>% 
  mutate (Model_suitable = ifelse (
    MASE_GAM < 1 & DM_GAM_p < 0.05, 
    "1", 
    "0"
  )) %>%
  #filter (perc_change_Morely < 20000, !is.na (Model_suitable)) %>%
  filter (perc_change_Morely < 750) %>%
  
  mutate (species = factor(species, levels = spp_order$species),
          Quota = factor(Quota),
          fill_col = case_when(
            MASE_GAM >= 1 | DM_GAM_p >= 0.05 ~ "Model unsuitable",
            MASE_GAM < 1 & DM_GAM_p < 0.05 & Quota == 1 ~ "Quota",
            MASE_GAM < 1 & DM_GAM_p < 0.05 & Quota == 0 ~ "Non-Quota"
          )
  ) %>% 
  
  ggplot (aes (x = species, 
               y = perc_change_Morely,
               color = Quota, 
               fill = fill_col,
               width = 0.85)) +
  scale_fill_manual (values = c("lightgray",  "#F8766D", "#00BFC4")) +
  labs (fill = "",
        x = "",
        y = "Percent habitat change") +
  guides (color = FALSE) +
  geom_bar (stat = "identity") + 
  coord_flip() + 
  theme_bw() +
  facet_wrap (~scenario) +
  ggtitle ("Percent habitat change, 2000-2018 vs 2061-2080") +
  theme (
    axis.text.x = element_text (size = 20),
    axis.text.y = element_text (size = 18),
    axis.title = element_text (size = 20),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 20),
    legend.text = element_text (size = 20),
    legend.position = c(0.87, 0.15)
  ) 
dev.off()

# plot with common name for figures
png (file = "Figures/Percent_hab_change_by_quota_SmoothLatlon_comm_names.png", width = 16, height = 9, units = "in", res = 300)
hab_change %>% 
  left_join (quota_status, by = "species") %>% 
  left_join (smoothLL_MASE, by = "species") %>% 
  mutate (Model_suitable = ifelse (
    MASE_GAM < 1 & DM_GAM_p < 0.05, 
    "1", 
    "0"
  )) %>%
  filter (perc_change_Morely < 20000, !is.na (Model_suitable)) %>%
  #filter (perc_change_Morely < 750) %>%
  
  mutate (species = factor(species, levels = spp_order$species),
          Quota = factor(Quota),
          fill_col = case_when(
            MASE_GAM >= 1 | DM_GAM_p >= 0.05 ~ "Model unsuitable",
            MASE_GAM < 1 & DM_GAM_p < 0.05 & Quota == 1 ~ "Quota",
            MASE_GAM < 1 & DM_GAM_p < 0.05 & Quota == 0 ~ "Non-Quota"
          )
  ) %>% 
  
  ggplot (aes (x = species, 
               y = perc_change_Morely,
               color = Quota, 
               fill = fill_col,
               width = 0.85)) +
  scale_fill_manual (values = c("lightgray",  "#F8766D", "#00BFC4")) +
  scale_x_discrete (labels = spp_order$name) +
  labs (fill = "",
        y = "Percent habitat change") +
  guides (color = FALSE) +
  geom_bar (stat = "identity") + 
  coord_flip() + 
  theme_bw() +
  facet_wrap (~scenario) +
  ggtitle ("Smoothed lat/lon Percent habitat change, 2000-2018 vs 2061-2080")
dev.off()


# depth/temp ----
load ("Data/perc_hab_change_Morely_depthtemp.RData")

spp_order <- hab_change %>%
  filter (perc_change_Morely < 750, scenario == 245) %>%
  arrange (perc_change_Morely)

# quota and model diagnostics
quota_status <- read_csv ("Data/species_eng.csv") %>%
  select (sci_name_underscore, Quota) %>%
  rename (species = sci_name_underscore)

depth_temp_MASE <- read_csv ("Models/Temp_Depth_GAM_diagnostics.csv")

png (file = "Figures/Percent_hab_change_by_quota_DepthTemp.png", width = 16, height = 9, units = "in", res = 300)
hab_change %>% 
  left_join (quota_status, by = "species") %>% 
  left_join (depth_temp_MASE, by = "species") %>% 
mutate (Model_suitable = ifelse (
  MASE_GAM < 1 & DM_GAM_p < 0.05, 
  "1", 
  "0"
)) %>%
  filter (perc_change_Morely < 750, !is.na (Model_suitable)) %>%
  # create column to manipulate fill based on quota and model
  # https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette, show_col(hue_pal()(2))
  mutate (species = factor(species, levels = spp_order$species),
          Quota = factor(Quota),
          fill_col = case_when(
            Model_suitable == 0 ~ "Model unsuitable",
            Model_suitable == 1 & Quota == 1 ~ "Quota",
            Model_suitable == 1 & Quota == 0 ~ "Non-Quota"
            ) 
          ) %>% 
  ggplot (aes (x = species, 
               y = perc_change_Morely,
               color = Quota, 
               fill = fill_col,
               width = 0.85)) +
  scale_fill_manual (values = c("lightgray",  "#F8766D", "#00BFC4")) +
  labs (fill = "",
        y = "Percent habitat change") +
  guides (color = FALSE) +
  geom_bar (stat = "identity") + 
  coord_flip() + 
  theme_bw() +
  facet_wrap (~scenario) +
  ggtitle ("Depth/Temp model Percent habitat change, 2000-2018 vs. 2061-2080")

dev.off()

  
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
  
  
 
  

