## Calculate and plot percent change in thermal suitable habitat
# JGM
# 9/9/2020

library (raster)
library (tidyverse)
#library (rgeos)


# I'm using the Morely et al. 2018 method for now, sum of predictions values for each cell. I'm going to calculate the sum standardized by area for each year in the historical period, then take the mean. Then I'm going to do the same for each of the climate models. I will then calculate the percent difference (projection mean minus hist mean over hist mean) for each model. Then I'll take the mean and standard deviation of those percent difference values. 


# ==================
# function to run this for each GAM I've fit ----
# ==================

calculate_perc_hab_change <- function (GAM) {
  # GAM is the name of the model/folder where prediction bricks are saved
  # Could also change year range if I look at different ranges
  
  # make an empty data frame to store mean and sd perc habitat change for each species and scenario
  hab_change <- data.frame ()
  
  # find the historical brick files
  hist_files <- list.files (path = "Models/Prediction_bricks", 
                            pattern = paste0(".*", GAM, ".*2000_2018.grd")
                            )

  for (i in 1:length(hist_files)) {
    
    ### break up elements of filename to make it easier to parse
    file_split <- strsplit (hist_files[i], "_")[[1]]
    
    spp_name = ifelse (file_split[1] %in% c ("Squid", "Myctophidae"), 
                      file_split[1],
                      paste (file_split[1], file_split[2], sep = "_")
                      )
    

    # ==================
    # calculate historical habitat, standardized by area
    # ==================
    
    # load files as raster brick
    br_hist <- brick (file.path ("Models/Prediction_bricks", hist_files[i]))
    
    # for Bormicon region models, would need to standardize by area. It looks like the area is slightly different between historical and different scenarios, which is weird. 
    # https://www.researchgate.net/post/is_there_an_easy_way_to_determine_the_area_of_a_raster_inside_a_polygon_shapefile
    # I don't fully understand what this is doing. But it seems to be scaling appropriately for predictions that cover more/less area. 
    area_hist <- cellStats(raster::area(br_hist[[1]], na.rm = TRUE), stat = sum)
    
    # this is the sum of all the cells for each time step, should have 228 values for 2000-2018
    sum_hist <- cellStats(br_hist, stat = 'sum') * area_hist
    mean_hist <- mean (sum_hist)
    
    # ==================
    # bring in predictions and calculate percent change, for both scenarios
    # ==================
    
    scen_df <- data.frame()
    
    for (scen in c(245, 585)) {
      
      # list appropriate files for species and scenario
      pred_files <- list.files (path = "Models/Prediction_bricks", 
                                    pattern = paste0(spp_name, "_", GAM, ".*", scen, "_2061_2080.grd"),
                                    full.names = TRUE
                                    )
      
      # I have a different set of models for both scenarios
      if (scen == 585) {
        CM_list = c("gfdl", "cnrm", "ipsl", "mohc", "CM26")
      } else {CM_list = c("gfdl", "cnrm", "ipsl", "mohc") }
      
      
      # ==================
      # calculate percent change for each model, using helper function
      # ==================

      calc_CM_perc_change <- function (hist_mean, pred_files, CM) {
        # hist_mean is a single value, calculated above
        #pred_files are the bricks for each species and scenario
        # CM is the name of the climate model
        
        br_pred <- brick (pred_files[grep(CM, pred_files)])
        area_pred <- cellStats(raster::area(br_pred[[1]], na.rm = TRUE), stat = sum)
        sum_pred <- cellStats(br_pred, stat = 'sum') * area_pred
        mean_pred <- mean (sum_pred)
        
        perc_change <- (mean_pred - mean_hist)/mean_hist * 100
        
      }
      
      
      
      perc <- sapply (CM_list, calc_CM_perc_change,
              hist_mean = mean_hist,
              pred_files = pred_files
              )
      
      # keep all values
      spp_df <- data.frame (
        species = spp_name,
        model = CM_list,
        scenario = scen,
        perc_change = perc
      )
      
      
      
      scen_df <- rbind (scen_df, spp_df)
      
    } # end scenario loop
    
    hab_change <- rbind (hab_change, scen_df)
    
  } # end hist_files i loop
  
  # save
  save (hab_change, file = paste0("Data/perc_hab_change_Morely_", GAM, ".RData"))
  
  print (Sys.time())
  
} # end function

   
calculate_perc_hab_change ("Borm_14_alltemp")  
    
    


# ==================
# load and plot ----
# ==================

# species list with categories

spp_list <- read_csv ("Data/species_eng.csv",
                      col_types = cols(
                        Spp_ID = col_factor()
                      )) %>%
  rename (species = sci_name_underscore) # rename to match species column

# function to plot by quota status ----
plot_hab_change_quota_fun <- function (GAM, hab_change_df, spp_order) {
  
  MASE <- read_csv (paste0("Models/GAM_performance_", GAM, ".csv"))
  
  png (file = paste0("Figures/Percent_hab_change_by_quota_boxplot_portrait", GAM, ".png"), width = 8.5, height = 11, units = "in", res = 300)
  
  print (
    
    hab_change %>% 
      left_join (spp_list, by = "species") %>% 
      left_join (MASE, by = "species") %>% 
      mutate (Model_suitable = ifelse (
        MASE_GAM < 1 & DM_GAM_p < 0.05, 
        "1", 
        "0"
      )) %>% 
      filter (!is.na (Model_suitable)) %>% 
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
      ggplot (aes (y = species, 
                   x = perc_change,
                   color = Quota, 
                   fill = fill_col,
                   width = 0.85)) +

      guides (color = FALSE) +
      geom_boxplot() +
  
      geom_vline (xintercept = 0, lty = 2, col = "dark gray") +
      # geom_col () +
      # geom_errorbar (aes (ymin = mn_change - sd_change, ymax = mn_change + sd_change), col = "black") +
      #coord_flip() + 
      facet_wrap (~scenario, scales = "free_x") +
      theme_bw() +
      scale_fill_manual (values = alpha (c("lightgray",  "#F8766D", "#00BFC4"), 0.5)) +
      labs (fill = "",
            x = "Percent habitat change") +
      #ggtitle (paste(GAM, "Percent habitat change, 2000-2018 vs. 2061-2080")) +
      ggtitle ("Percent habitat change, 2000-2018 vs. 2061-2080") +
      # theme (
      #   axis.text.x = element_text (size = 20),
      #   axis.text.y = element_text (size = 14),
      #   axis.title = element_text (size = 20),
      #   plot.title = element_text (size = 24),
      #   strip.text = element_text (size = 20),
      #   legend.text = element_text (size = 20),
      #   legend.position = c(0.87, 0.15)
      # ) 
    theme (
      axis.text.x = element_text (size = 14),
      axis.text.y = element_text (size = 12),
      axis.title = element_text (size = 14),
      plot.title = element_text (size = 16),
      strip.text = element_text (size = 14),
      legend.text = element_text (size = 14),
      legend.position = c(0.83, 0.15)
    ) 
   
     
  )
  
  dev.off()
                    
  
} # end function

#https://stackoverflow.com/questions/57651144/conditional-formatting-of-axis-text-using-ggplot2
# color names by quota status and boxes by thermal pref
# function to plot by TB and Steno ----
plot_hab_change_TB_steno_fun <- function (GAM, hab_change_df, spp_order) {
  
  MASE <- read_csv (paste0("Models/GAM_performance_", GAM, ".csv"))
  
  png (file = paste0("Figures/Percent_hab_change_vs_TB_", GAM, ".png"), width = 16, height = 9, units = "in", res = 300)
  
  print (
      
      hab_change %>% 
      group_by (species, scenario) %>%
      summarise (mn_change = mean(perc_change),
                 sd_change = sd (perc_change)) %>%
      left_join (spp_list, by = "species") %>% 
      left_join (MASE, by = "species") %>% 
      mutate (Model_suitable = case_when (
        MASE_GAM < 1 & DM_GAM_p < 0.05 ~ "1", 
        is.na (MASE_GAM) ~ "0",
        TRUE ~ "0")
        ) %>% 
      ggplot (aes (x = mean_TB, 
                   y = mn_change,
 
                   col = Model_suitable)) +
      
        geom_smooth (method = "lm", alpha = 0.2) +
        geom_point() +
      facet_wrap (~scenario, scales = "free") +
      theme_bw() +

      scale_color_manual (values = c ("gray", "black")) +
      ggtitle (paste(GAM, "Thermal bias vs. Percent habitat change (2000-2018 vs. 2061-2080)"))
    
    
  )
  
  dev.off()
  
  png (file = paste0("Figures/Percent_hab_change_vs_Steno_", GAM, ".png"), width = 16, height = 9, units = "in", res = 300)
  
  hab_change %>% 
    group_by (species, scenario) %>%
    summarise (mn_change = mean(perc_change),
               sd_change = sd (perc_change)) %>%
    left_join (spp_list, by = "species") %>% 
    left_join (MASE, by = "species") %>% 
    mutate (Model_suitable = case_when (
      MASE_GAM < 1 & DM_GAM_p < 0.05 ~ "1", 
      is.na (MASE_GAM) ~ "0",
      TRUE ~ "0"),
      
      Therm_pref = case_when (
        mean_TB > 0 ~ "Warm",
        between (mean_TB, -3, 0) ~ "Cool",
        mean_TB < 0 ~ "Cold"
      )
    ) %>% 
    ggplot (aes (x = mean_Steno, 
                 y = mn_change,
                 color = Therm_pref,
                 fill = Model_suitable,
                 shape = Model_suitable)
    ) +
    #geom_smooth (method = "lm", alpha = 0.2) +
    geom_point() +
    facet_wrap (~scenario, scales = "free") +
    theme_bw() +
    geom_hline (yintercept = 0, lty = 2, col = "dark gray") +
    scale_shape_manual (values = c (21, 19)) +
    scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
    scale_fill_manual (values = c ("white", "black")) +
    ggtitle (paste(GAM, "Steno index vs. Percent habitat change (2000-2018 vs. 2061-2080)"))
  
  dev.off()
  
} # end function



# ==================
# Borm 14 all temp ----

load ("Data/perc_hab_change_Morely_Borm_14_alltemp.RData")

spp_order <- hab_change %>%
  filter (scenario == 585) %>%
  group_by (species) %>%
  summarise (mn_change = mean (perc_change)) %>%
  arrange (mn_change)

plot_hab_change_quota_fun (GAM = "Borm_14_alltemp",
                      hab_change_df = hab_change,
                      spp_order = spp_order)

plot_hab_change_TB_steno_fun (GAM = "Borm_14_alltemp",
                              hab_change_df = hab_change,
                              spp_order = spp_order)

# plot portrait with TB for Cat

png (file = paste0("Figures/Percent_hab_change_TB_boxplot_portrait_", GAM, ".png"), width = 8.5, height = 11, units = "in", res = 300)

png (file = paste0("Figures/Percent_hab_change_TB_boxplot_", GAM, ".png"), width = 16, height = 9, units = "in", res = 300)

print (
  
  hab_change %>% 
    left_join (spp_list, by = "species") %>% 
    left_join (MASE, by = "species") %>% 
    mutate (Model_suitable = ifelse (
      MASE_GAM < 1 & DM_GAM_p < 0.05, 
      "1", 
      "0"
    )) %>%
    filter (!is.na (Model_suitable)) %>%
    # create column to manipulate fill based on quota and model
    # https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette, show_col(hue_pal()(2))

      
    mutate (species = factor(species, levels = spp_order$species),
            Therm_pref = case_when (
              mean_TB > 0 ~ "Warm",
              between (mean_TB, -3, 0) ~ "Cool",
              mean_TB < 0 ~ "Cold"
            ),
            fill_col = case_when(
              Model_suitable == 0 ~ "*Model unsuitable",
              Model_suitable == 1 & mean_TB > 0 ~ "Warm",
              Model_suitable == 1 & between (mean_TB, -3, 0) ~ "Cool",
              Model_suitable == 1 &  mean_TB < 0 ~ "Cold"
            ),
            scen_long = case_when (
              scenario == 245 ~ "Optimistic scenario",
              scenario == 585 ~ "Worst case scenario"
            )
    ) %>% 
    ggplot (aes (y = species, 
                 x = perc_change,
                 color = Therm_pref, 
                 fill = fill_col,
                 width = 0.85)) +
    
    guides (color = FALSE) +
    geom_boxplot() +
    
    geom_vline (xintercept = 0, lty = 2, col = "dark gray") +
    # geom_col () +
    # geom_errorbar (aes (ymin = mn_change - sd_change, ymax = mn_change + sd_change), col = "black") +
    #coord_flip() + 
    facet_wrap (~scen_long, scales = "free_x") +
    theme_bw() +
    scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
    scale_fill_manual (values = alpha (c("lightgray", "blue", "deepskyblue", "red2"), 0.5)) +
    labs (fill = "",
          x = "Percent habitat change") +
    #ggtitle (paste(GAM, "Percent habitat change, 2000-2018 vs. 2061-2080")) +
    ggtitle ("Percent habitat change, 2000-2018 vs. 2061-2080") +

    theme (
      axis.text.x = element_text (size = 14),
      axis.text.y = element_text (size = 12),
      axis.title = element_text (size = 14),
      plot.title = element_text (size = 16),
      strip.text = element_text (size = 14),
      legend.text = element_text (size = 14),
      legend.position = c(0.80, 0.12)
    ) 
  
  
)

dev.off()

# ==================
# smooth lat/lon ----

load ("Data/perc_hab_change_Morely_smoothlatlon.RData")


head (hab_change)

# fix underscore issue for smoothlatlon
# not working with mutate for some reason
hab_change$species[which (hab_change$species == "Squid_Smooth")] <- "Squid"
hab_change$species[which (hab_change$species == "Myctophidae_Smooth")] <- "Myctophidae"

# filtered for perc change < 20000

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
 

# plot starting with herring, cut off at 750
spp_order <- hab_change %>%
  filter (perc_change_Morely < 750, scenario == 245) %>%
  arrange (perc_change_Morely)


# ==================
# depth/temp ----


load ("Data/perc_hab_change_Morely_depthtemp.RData")

spp_order <- hab_change %>%
  filter (perc_change_Morely < 750, scenario == 245) %>%
  arrange (perc_change_Morely)




  
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
