## Radial compass plots
# 8/21/2020
# JGM

library (geosphere) # for distHaversine and bearing
library (raster)
library (tidyverse)
library (SDMTools) # for calculating circular average. Not available for R version 4.0.2, downloaded from https://www.rforge.net/SDMTools/git.html
library (spatstat) # for weighted quantiles



# Code from Morely et al. 2018, calculating % habitat change as change in sum of prediction values



calculate_centroids_fun <- function (GAM, spp_names, year_range) {
  # empty df to store full dataset
  centroids_df <- data.frame ()

  start_t <- Sys.time()
    
    for (spp in spp_names) {
      
      # load prediction bricks
      hist_br <- brick (paste0("Models/Prediction_bricks/", spp, "_", GAM, "_2000_2018.grd"))
      
      # take mean so have one value per period
      hist_mn <- calc (hist_br, mean) # calc is faster
      
      # spatial points df so I have a value corresponding to each lat/lon combo
      df_pre <- data.frame(rasterToPoints(hist_mn))
      
      # calculate mean lon and lat, store as data.frame to match with climate models
      
      # eventually want something about circular movement around iceland??
      hist_df <- data.frame (
        species = spp,
        period = "hist",
        model = "hist",
        scenario = "hist",
        mean_Lat = weighted.mean(df_pre$y, df_pre$layer), # layer is the thermpred prediction value
        mean_Lon =  weighted.mean(df_pre$x, df_pre$layer),
        #warm and cool edges, Fredston-Herman 2020. should probably get this from obs, not backcast? 5th and 95th latitude quantile
        # might not be that helpful because i'm limiting to EEZ, and might run into borm problems
        warm_edge = weighted.quantile (df_pre$y, w = df_pre$layer, probs = 0.05, na.rm = TRUE),
        cold_edge = weighted.quantile (df_pre$y, w = df_pre$layer, probs = 0.95, na.rm = TRUE)
      )
      
      # Do the same for climate models, both scenarios
      scen_df <- data.frame()
      
      for (scen in c(245, 585)) {
        
        # list appropriate files for species and scenario
        pred_files <- list.files (path = "Models/Prediction_bricks", 
                                  pattern = paste0(spp, "_", GAM, ".*", scen, "_2061_2080.grd"),
                                  full.names = TRUE
        )
        
        # I have a different set of models for both scenarios
        if (scen == 585) {
          CM_list = c("gfdl", "cnrm", "ipsl", "mohc", "CM26")
        } else {CM_list = c("gfdl", "cnrm", "ipsl", "mohc") }
        
        
        # make empty df to store results for all CMs, for this scenario
        all_CM_df <- data.frame()
        
        for (CM in CM_list) {
          
          pred_br <- brick (pred_files[grep(CM, pred_files)])
          
          #take mean so have one value per period
          pred_mn <- calc (pred_br, mean) # calc is faster
          df_post <- data.frame(rasterToPoints(pred_mn))
          
          # compile in dataframe
          CM_df <- data.frame (
            species = spp,
            period = "future",
            model = CM, 
            scenario = scen,
            mean_Lat = weighted.mean(df_post$y, df_post$layer),
            mean_Lon = weighted.mean(df_post$x, df_post$layer),
            warm_edge = weighted.quantile (df_post$y, w = df_post$layer, probs = 0.05, na.rm = TRUE),
            cold_edge = weighted.quantile (df_post$y, w = df_post$layer, probs = 0.95, na.rm = TRUE)
          )
          
          all_CM_df <- rbind (all_CM_df, CM_df)
          
        } # end CM loop
        
        scen_df <- rbind (scen_df, all_CM_df)
        
      } # end scenario loop
      
      centroids_df <- rbind (centroids_df, hist_df, scen_df)
      
      cent_mn <- scen_df %>% group_by (period, scenario) %>%
        summarise (mn_Lat = mean (mean_Lat),
                   mn_Lon = mean (mean_Lon)) #%>%
        group_by (period) %>%
        summarise (diff_lat = diff(mn_Lat),
                   diff_lon = diff(mn_Lon))
       
      cent_mn$scenario[which (cent_mn$period == "hist")] <- NA
      
      cent_no_period <- cent_mn %>%
        ungroup() %>%
        dplyr::select (-period)
      
      cent_mn_wide_scen <- cent_no_period %>%
        pivot_wider (names_from = scenario,
                     values_from = c(mn_Lat, mn_Lon))
      
      cent_mn_double_hist <- rbind (cent_mn, cent_mn[3,])
      cent_mn_double_hist$scenario[3:4] <- c(245, 585)
      cent_mn_double_hist_wide <- cent_mn_double_hist %>% pivot_wider (names_from = period,
                                           values_from = c(mn_Lat, mn_Lon))
      
      scen_double_hist <- rbind (scen_df, scen_df[5,])
      scen_double_hist$scenario[c(5, 11)] <- c(245, 585)
      
      scen_double_hist %>% pivot_wider (names_from = period,
                                        values_from = c(mean_Lat, mean_Lon))
      
      
      cent_mn_wider <- cent_mn %>%

        pivot_wider (names_from = period,
                     values_from = c(mn_Lat, mn_Lon))
      
      just_hist <- scen_df[5, c(1, 5:8)]
      colnames (just_hist)[2:5] <- paste0("hist_", colnames(just_hist)[2:5])
      
      just_future <- scen_df [-5,]
      
      # https://stackoverflow.com/questions/40554592/geosphere-disthaversine-dplyr-error-wrong-length-for-vector-should-be-2
      tmp <- just_future %>%
        left_join (just_hist, by = "species") %>%
        mutate (dist = distHaversine (cbind(mean_Lon, mean_Lat),
                                      cbind(hist_mean_Lon, hist_mean_Lat)
                                      ),
                bearing = bearing (cbind(mean_Lon, mean_Lat),
                                   cbind(hist_mean_Lon, hist_mean_Lat)
                                   )
                )
                            
  
      tmp_scen <- scen_df %>%
        group_by (period) %>%
        summarise (dist = distHaversine (cbind(mean_Lon, mean_Lat),
                                         cbind(hist_mean_Lon, hist_mean_Lat)
        ))
        pivot_wider (
          names_from = period, 
          values_from = c(mean_Lat:cold_edge))
      
      # calculate distance and bearing
      mean_shifts <- scen_df %>%
        group_by (species, scenario) %>%
        summarise (across(mean_Lat_post:cold_edge_post, mean))
      
   
      spp_df <- mean_shifts %>%
        mutate ( dist = distHaversine (c (mean_Lon_post, mean_Lat_pre),
                                      c (mean_Lon_post, mean_Lat_pre))/1000,
                 bearing = bearing (c (mean_Lon_post, mean_Lat_pre),
                                    c (mean_Lon_post, mean_Lat_pre))/1000
        )
          data.frame (
        species = rep(spp, 2),
        scenario = c (245, 585),
        
        dist = c (distHaversine ( c(mean_Lon_pre, mean_Lat_pre), 
                                  c(mean_Lon_post_245, mean_Lat_post_245))/ 1000, 
                  distHaversine ( c(mean_Lon_pre, mean_Lat_pre), 
                                  c(mean_Lon_post_585, mean_Lat_post_585))/ 1000
        ),
        
        bearing = c (bearing ( c(mean_Lon_pre, mean_Lat_pre), 
                               c(mean_Lon_post_245, mean_Lat_post_245)
        ),
        bearing (c(mean_Lon_pre, mean_Lat_pre), 
                 c(mean_Lon_post_585, mean_Lat_post_585)
        )
        )
      ) # end df
      
      
      
      # combine
      centroids_df <- rbind (centroids_df, spp_df)
      
      
    }  # end spp for loop
    
  end_t <- Sys.time() 
  print (end_t - start_t)
  
  # assign so it stays in environment??
  centroids_df <<- centroids_df

} # end function

# copying again
calculate_centroids_fun <- function (model, spp_names, year_range) {
  # empty df to store
  centroids_df <- data.frame ()
  
  start_t <- Sys.time()
  
  for (spp in spp_names) {
    
    # load prediction bricks
    hist <- brick (paste0("Models/Prediction_bricks/", spp, "_", model, "_2000_2018.grd"))
    pred_245 <- brick (paste0("Models/Prediction_bricks/", spp, "_", model, "_ensemble_mean_245_", year_range, ".grd"))
    pred_585 <- brick (paste0("Models/Prediction_bricks/", spp, "_", model, "_ensemble_mean_585_", year_range, ".grd"))
    
    # take mean so have one value per period
    hist_mn <- calc (hist, mean) # calc is faster
    pred_245_mn <- calc (pred_245, mean)
    pred_585_mn <- calc (pred_585, mean)
    
    
    # spatial points df so I have a value corresponding to each lat/lon combo
    df_pre <- data.frame(rasterToPoints(hist_mn))
    df_post_245 <- data.frame(rasterToPoints(pred_245_mn))
    df_post_585 <- data.frame(rasterToPoints(pred_585_mn))
    
    # calculate mean lon and lat
    mean_Lat_pre <- weighted.mean(df_pre$y, df_pre$layer) # layer is the thermpred prediction value
    mean_Lon_pre <- weighted.mean(df_pre$x, df_pre$layer)
    
    mean_Lat_post_245 <- weighted.mean(df_post_245$y, df_post_245$layer)
    mean_Lat_post_585 <- weighted.mean(df_post_585$y, df_post_585$layer)
    
    mean_Lon_post_245 <- weighted.mean(df_post_245$x, df_post_245$layer)
    mean_Lon_post_585 <- weighted.mean(df_post_585$x, df_post_585$layer)
    
    
    # make data frame with two rows for both scenarios
    spp_df <- data.frame (
      species = rep(spp, 2),
      scenario = c (245, 585),
      
      dist = c (distHaversine ( c(mean_Lon_pre, mean_Lat_pre), 
                                c(mean_Lon_post_245, mean_Lat_post_245))/ 1000, 
                distHaversine ( c(mean_Lon_pre, mean_Lat_pre), 
                                c(mean_Lon_post_585, mean_Lat_post_585))/ 1000
      ),
      
      bearing = c (bearing ( c(mean_Lon_pre, mean_Lat_pre), 
                             c(mean_Lon_post_245, mean_Lat_post_245)
      ),
      bearing (c(mean_Lon_pre, mean_Lat_pre), 
               c(mean_Lon_post_585, mean_Lat_post_585)
      )
      )
    ) # end df
    
    
    
    # combine
    centroids_df <- rbind (centroids_df, spp_df)
    
    
  }  # end for loop
  
  end_t <- Sys.time() 
  print (end_t - start_t)
  
  # assign so it stays in environment??
  centroids_df <<- centroids_df
  
  
} # end function

summary (centroids_df)
# bearing seems to be -180 to 180, not 0-360

# borm_14, do by tb and steno ----
load ("Models/spp_Smooth_latlon.RData")
borm_spp <- filter (spp_Smooth_latlon, 
                    ! sci_name_underscore %in% c("Myoxocephalus_scorpius", "Amblyraja_hyperborea", "Rajella_fyllae,", "Lumpenus_lampretaeformis"))

calculate_centroids_fun(model = "Borm_14_alltemp", spp_names = borm_spp$sci_name_underscore, year_range = "2061_2080")

# save for now!
save (centroids_df, file = "Data/centroids_Borm_14_alltemp.RData")

borm_MASE <- read_csv ("Models/GAM_performance_Borm_14_alltemp.csv")
spp_list  <- read_csv ("Data/species_eng.csv",
                                  col_types = cols(
                                    Spp_ID = col_factor()
                                  )) %>%
  rename (species = sci_name_underscore) # rename to match species column

png ("Figures/Compass_Borm14alltemp_TB_jitter.png", width = 16, height = 9, units = "in", res = 300)
set.seed(15)
centroids_df %>%
  left_join (borm_MASE, by = "species") %>%
  left_join (spp_list, by = "species") %>%
  # just cut out not suitable
  filter (MASE_GAM < 1 & DM_GAM_p < 0.05) %>%
  mutate (
    Therm_pref = case_when (
      mean_TB > 0 ~ "Warm",
      between (mean_TB, -3, 0) ~ "Cool",
      mean_TB < 0 ~ "Cold"
    ),
    scen_long = case_when (
      scenario == 245 ~ "Optimistic scenario",
      scenario == 585 ~ "Worst case scenario"
    ),
    # convert bearing to 0-360
    bearing = ifelse (bearing < 0, bearing + 360, bearing)) %>%
  #filter (dist < 100) %>%
  ggplot(aes (x = bearing,
              y = dist, 
              colour = Therm_pref,
              label = species)) +
  coord_polar(start = 0) +
  geom_segment(aes (y = 0,
                    xend = bearing, 
                    yend = dist
  ), 
  size = 1.2
  ) +  
  geom_text(aes(label = species, 
                x = bearing,
                y = 200,  

                # not sure what angle is doing here but it seems right
                angle = ifelse (bearing < 180,
                                -bearing + 90,
                                -bearing + 270
                )
  ),
  col = "black",
  position = position_jitter(width = 0.5, height = 50),
  size = 3.5, 
  vjust = 0.1) +
  facet_wrap (~ scen_long) +
  labs(y= "Distance (km)") + #, 
  #title=paste("Spring, North, Circular Average = ", 
  #round(CA[1], 2))) +
  scale_x_continuous(breaks= c(22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5), limits = c(0,360)) + 
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  labs (color = "") + 
  theme_bw() +
  theme (
    axis.text.x = element_text (size = 18),
    axis.text.y = element_text (size = 18),
    axis.title = element_text (size = 20),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 20),
    legend.text = element_text (size = 20),
  ) +
  
  # theme(plot.title = element_text(size = rel(1.5)), 
  #       axis.title = element_text(size = rel(1.25)), 
  #       legend.text = element_text(size = rel(1.25) ), 
  #       axis.text = element_text(size = rel(1)), 
  #       legend.title= element_text(size = rel(1.25) )) +
  ggtitle ("Centroid shift bearing and distance, 2000-2018 vs. 2060-2081")
dev.off()

# https://eliocamp.github.io/codigo-r/2018/09/multiple-color-and-fill-scales-with-ggplot2/
new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}
library(viridis)

png ("Figures/Compass_Borm14alltemp_Steno.png", width = 16, height = 9, units = "in", res = 300)

centroids_df %>%
  left_join (borm_MASE, by = "species") %>%
  left_join (spp_list, by = "species") %>%
  # just cut out not suitable
  filter (MASE_GAM < 1 & DM_GAM_p < 0.05) %>%
  mutate (
    Therm_pref = case_when (
      mean_TB > 0 ~ "Warm",
      between (mean_TB, -3, 0) ~ "Cool",
      mean_TB < 0 ~ "Cold"
    ),
    # convert bearing to 0-360
    bearing = ifelse (bearing < 0, bearing + 360, bearing)) %>%
  #filter (dist < 100) %>%
  ggplot(aes (x = bearing,
              y = dist, 

              label = species)) +
  coord_polar(start = 0) +
  geom_segment(aes (y = 0,
                    #colour = mean_Steno,
                    xend = bearing, 
                    yend = dist
  ), 
  size = 1.2) + 
  scale_color_viridis_c ("Stenothermal index") +
  new_scale("color") +
  geom_text(aes(label = species, 
                x = bearing,
                y = 200,  
                col = Therm_pref,
                # not sure what angle is doing here but it seems right
                angle = ifelse (bearing < 180,
                                -bearing + 90,
                                -bearing + 270
                )
  ),
  #col = "black",
  size = 3.5, 
  vjust = 0.1) +
  facet_wrap (~ scenario) +
  labs(y= "Maximum distance/decade (km)") + #, 
  #title=paste("Spring, North, Circular Average = ", 
  #round(CA[1], 2))) +
  scale_x_continuous(breaks= c(22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5), limits = c(0,360)) + 
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  labs (color = "") + 
  theme_bw() +
  
  theme(plot.title = element_text(size = rel(1.5)), 
        axis.title = element_text(size = rel(1.25)), 
        legend.text = element_text(size = rel(1.25) ), 
        axis.text = element_text(size = rel(1)), 
        legend.title= element_text(size = rel(1.25) )) +
  ggtitle ("Centroid shift bearing and distance, 2000-2018 vs. 2060-2081")
dev.off()



# Smooth_latlon 2000-2018 and 2061-2080 ----

# load smooth_latlon spp
load ("Models/spp_Smooth_latlon.RData")

# diagnostics to designate based on certainty ----
smoothLL_MASE <- read_csv ("Models/GAM_diagnostics_all_spp.csv") %>%
  rename (species = Species)

# quota status ----
quota_status <- read_csv ("Data/species_eng.csv") %>%
  select (sci_name_underscore, Quota) %>%
  rename (species = sci_name_underscore)

# circular average ----
# this is from Kristin's code

# overall CA
centroids_df %>%
  group_by (scenario) %>%
  summarize (CA = circular.averaging (direction = bearing)) # seems to figure out 0-360 automatically?

# CA based on category
centroids_df %>%
  left_join (smoothLL_MASE, by = "species") %>%
  left_join (quota_status, by = "species") %>%
  mutate (
    # categorize based on suitability
    fill_col = case_when(
      MASE_GAM >= 1 | DM_GAM_p >= 0.05 ~ "Model unsuitable",
      MASE_GAM < 1 & DM_GAM_p < 0.05 & Quota == 1 ~ "Quota",
      MASE_GAM < 1 & DM_GAM_p < 0.05 & Quota == 0 ~ "Non-Quota"
    )) %>%
      group_by (scenario, fill_col) %>%
      summarize (CA = circular.averaging (direction = bearing))


# plot smooth lat/lon ----  
png ("Figures/Compass_SmoothLL_all.png", width = 16, height = 9, units = "in", res = 300)
centroids_df %>%
  left_join (smoothLL_MASE, by = "species") %>%
  left_join (quota_status, by = "species") %>%
  mutate (
    # categorize based on suitability
    fill_col = case_when(
      MASE_GAM >= 1 | DM_GAM_p >= 0.05 ~ "Model unsuitable",
      MASE_GAM < 1 & DM_GAM_p < 0.05 & Quota == 1 ~ "Quota",
      MASE_GAM < 1 & DM_GAM_p < 0.05 & Quota == 0 ~ "Non-Quota"
    ),
    # convert bearing to 0-360
    bearing = ifelse (bearing < 0, bearing + 360, bearing)) %>%
  filter (dist < 100) %>%
ggplot(aes (x = bearing,
            y = dist, 
            colour = fill_col,
            label = species)) +
  coord_polar(start = 0) +
  geom_segment(aes (y = 0,
                    xend = bearing, 
                    yend = dist
                    ), 
               size = 1.2
               ) +  
  geom_text(aes(label = species, 
                x = bearing,
                y = 200,  
                # not sure what angle is doing here but it seems right
                angle = ifelse (bearing < 180,
                                -bearing + 90,
                                -bearing + 270
                )
  ),
  size = 3.5, 
  vjust = 0.1) +
  facet_wrap (~ scenario) +
  labs(y= "Maximum distance/decade (km)") + #, 
  #title=paste("Spring, North, Circular Average = ", 
  #round(CA[1], 2))) +
  scale_x_continuous(breaks= c(22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5), limits = c(0,360)) + 
  scale_color_manual (values = c("gray",  "#F8766D", "#00BFC4")) +
  labs (color = "") + 
  theme_bw() +
  
  theme(plot.title = element_text(size = rel(1.5)), 
        axis.title = element_text(size = rel(1.25)), 
        legend.text = element_text(size = rel(1.25) ), 
        axis.text = element_text(size = rel(1)), 
        legend.title= element_text(size = rel(1.25) )) +
  ggtitle ("Smoothed Lat/Lon Bearing and Distance, 2000-2018 vs. 2060-2081")

dev.off()

## Depth/Temp model ----
# load smooth_latlon spp
load ("Models/spp_Depth_Temp.RData")

calculate_centroids_fun(model = "Depth_Temp", spp_names = td_spp_names, year_range = "2061_2080")

# diagnostics to designate based on certainty ----
TD_MASE <- read_csv ("Models/Temp_Depth_GAM_diagnostics.csv") 

# quota status ----
quota_status <- read_csv ("Data/species_eng.csv") %>%
  select (sci_name_underscore, Quota) %>%
  rename (species = sci_name_underscore)

# calculate circular average ----
centroids_df %>%
  group_by (scenario) %>%
  summarize (CA = circular.averaging (direction = bearing)) # seems to figure out 0-360 automatically?

# CA based on category
centroids_df %>%
  left_join (TD_MASE, by = "species") %>%
  left_join (quota_status, by = "species") %>%
  mutate (
    # categorize based on suitability
    fill_col = case_when(
      MASE_GAM >= 1 | DM_GAM_p >= 0.05 ~ "Model unsuitable",
      MASE_GAM < 1 & DM_GAM_p < 0.05 & Quota == 1 ~ "Quota",
      MASE_GAM < 1 & DM_GAM_p < 0.05 & Quota == 0 ~ "Non-Quota"
    )) %>%
  group_by (scenario, fill_col) %>%
  summarize (CA = circular.averaging (direction = bearing))


# plot depth temp ----  
png ("Figures/Compass_DepthTemp_all.png", width = 16, height = 9, units = "in", res = 300)
centroids_df %>%
  left_join (TD_MASE, by = "species") %>%
  left_join (quota_status, by = "species") %>% 
  # more species in depth/temp than I was able to calculate MASE GAM bc of testing/training. may need to rethink this
  filter (!is.na (MASE_GAM)) %>%
  mutate (
    # categorize based on suitability
    fill_col = case_when(
      MASE_GAM >= 1 | DM_GAM_p >= 0.05 ~ "Model unsuitable",
      MASE_GAM < 1 & DM_GAM_p < 0.05 & Quota == 1 ~ "Quota",
      MASE_GAM < 1 & DM_GAM_p < 0.05 & Quota == 0 ~ "Non-Quota"
    ),
    # convert bearing to 0-360
    bearing = ifelse (bearing < 0, bearing + 360, bearing)) %>%
  filter (dist < 100) %>%
  ggplot(aes (x = bearing,
              y = dist, 
              colour = fill_col,
              label = species)) +
  coord_polar(start = 0) +
  geom_segment(aes (y = 0,
                    xend = bearing, 
                    yend = dist
  ), 
  size = 1.2
  ) +  
  geom_text(aes(label = species, 
                x = bearing,
                y = 200,  
                # not sure what angle is doing here but it seems right
                angle = ifelse (bearing < 180,
                                -bearing + 90,
                                -bearing + 270
                )
  ),
  size = 3.5, 
  vjust = 0.1) +
  facet_wrap (~ scenario) +
  labs(y= "Maximum distance/decade (km)") + #, 
  #title=paste("Spring, North, Circular Average = ", 
  #round(CA[1], 2))) +
  scale_x_continuous(breaks= c(22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5), limits = c(0,360)) + 
  scale_color_manual (values = c("gray",  "#F8766D", "#00BFC4")) +
  labs (color = "") + 
  theme_bw() +
  
  theme(plot.title = element_text(size = rel(1.5)), 
        axis.title = element_text(size = rel(1.25)), 
        legend.text = element_text(size = rel(1.25) ), 
        axis.text = element_text(size = rel(1)), 
        legend.title= element_text(size = rel(1.25) )) +
  ggtitle ("Depth/Temp Bearing and Distance, 2000-2018 vs. 2060-2081")

dev.off()

## Code test with simulated data----
# generate random simulated data for 10 species
test_df <- data.frame (
  dist = runif (10, 25, 100),
  bearing = runif (10, 0, 360),
  spp = spp_list$Common_name[1:10],
  group = as.factor(sample (1:3, 10, replace = T))
)


# code from Kristin's PlosOne paper to do this with ggplot
# Kristin code


CA <- circular.averaging(direction=test_df$bearing) # will need to find some other way of calculating circular average

library (tidyverse)
# use geom_segment, not geom_line
# https://stackoverflow.com/questions/10515703/ggplot2-polar-plot-arrows

ggplot(test_df,
       aes (x = bearing,
            y = dist, 
            #group = spp, 
            colour = group), 
       label = spp
) +
  coord_polar(start=0) +
  geom_segment(aes (y = 0,
                    xend = bearing, 
                    yend = dist
  ), 
  size=1.2) +  
  geom_text(aes(label = spp, 
                x = bearing,
                y = 100,  
                # not sure what angle is doing here but it seems right
                angle = ifelse (bearing < 180,
                                -bearing + 90,
                                -bearing + 270
                )
  ),
  size=5, 
  vjust=0.1) +
  labs(y= "Maximum distance/decade (km)") + #, 
  #title=paste("Spring, North, Circular Average = ", 
  #round(CA[1], 2))) +
  scale_x_continuous(breaks= c(22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5), limits = c(0,360)) + 
  theme_bw() +
  
  theme(plot.title = element_text(size = rel(1.5)), 
        axis.title = element_text(size = rel(1.25)), 
        legend.text = element_text(size = rel(1.25) ), 
        axis.text = element_text(size = rel(1.5)), 
        legend.title= element_text(size = rel(1.25) ))#+ 
#scale_color_manual(values = colorit_cluster)

# Morely et al. 2018 code ----
# https://github.com/pinskylab/project_velocity/blob/master/MS_figures.R

library (raster)

# I'm going to assume I don't need to do the correction for biomass weighting? something about converging longitude? all of my stuff should be on the same grid. 

# projection grid
load ("Data/prediction_raster_template.RData")

pred_r_template[] <- area(pred_r_template)[] # don't know what this does. adds a layer columm, 140 unique

# # make df of lat/lon points. In Morely code this has depth and two other values (rugosity, sediment size?). not sure if I need them. 
# 
 grid_area <- data.frame(rasterToPoints(pred_r_template), stringsAsFactors = FALSE) 
# 
# grid_area <- unique(data.frame(lat = grid_area$y, area = grid_area$layer)) # 140 rows, just lat
# 
# # from morely: # now get the latitude values to match up as sig-figs altered in raster
# lats <- unique(pred_r_template$)
# grid_area$latitude<- NA

# columns for init and final lat and lon, mean + SD in latitudinal change, mean + SD in absolute shift distance
centroids245 <- data.frame(
  latPre=numeric(), lonPre=numeric(), 
  latPost=numeric(), lonPost=numeric(), 
  meanLat=numeric(), sdLat=numeric(), 
  meanDist=numeric(), sdDist=numeric(), 
  radius=numeric()
  )

centroids585 <- data.frame(
  latPre=numeric(), lonPre=numeric(), 
  latPost=numeric(), lonPost=numeric(), 
  meanLat=numeric(), sdLat=numeric(),
  meanDist=numeric(), sdDist=numeric(), 
  radius=numeric()
  )
options(warn=0) 


# load my one prediction brick
tmp_brick <- brick ("Models/Prediction_bricks/Gadus_morhua_245_slatlon_temp.grd")

# iceland EEZ shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=5680
library (sf)
eez <- st_read("Data/eez.shp")

# clip to eez
pred_clip <- mask (tmp_brick, eez)


brick_pre <- pred_clip[[1:10]] # 2015-2024
brick_post <- pred_clip[[1023:1032]] # 2091-2100

pred_pre <- calc (brick_pre, mean)
pred_post <- calc (brick_post, mean)

# spatial points df so I have a value corresponding to each lat/lon combo
df_pre <- data.frame(rasterToPoints(pred_pre))
df_post <- data.frame(rasterToPoints(pred_post))

mean_Lat_pre <- weighted.mean(df_pre$y, df_pre$layer) # layer is the thermpred prediction value
mean_Lat_post <- weighted.mean(df_post$y, df_post$layer)

mean_Lon_pre <- weighted.mean(df_pre$x, df_pre$layer)
mean_Lon_post <- weighted.mean(df_post$x, df_post$layer)


# line 366
library (geosphere) # distHaverstine
dist <- distHaversine(c(mean_Lon_pre, mean_Lat_pre), c(mean_Lon_post, mean_Lat_post))/ 1000 # km
bearing <- bearing (c(mean_Lon_pre, mean_Lat_pre), c(mean_Lon_post, mean_Lat_post))

library (plotrix) # polar.plot
# line 651 of morely
polar.plot(lengths = dist, polar.pos = bearing,
           lwd = 2.5, line.col = "red",
           lables = c(), 
           boxed.radial = F, start = 90, clockwise= T) #, radial.lim = c(0, 1500))


# nothing is showing up. try with test_df simulated data

polar.plot(lengths = test_df$dist, polar.pos = test_df$bearing,
           lwd = 2.5, line.col = "red",
           labels = c(), 
           boxed.radial = F, start = 90, clockwise= T) #, radial.lim = c(0, 1500))

yearRange <- c('2015-2024', '2091-2100')
cent_preds26lat <- matrix(data=NA, nrow=2, ncol=16)
cent_preds26lon <- matrix(data=NA, nrow=2, ncol=16)

for(j in 1:2){
  years = yearRange[j]
  preds <- pred.agg26[pred.agg26$year_range == years,]
  for(k in 4:19){
    weights_adj <- preds[,k] * (preds$area) #*100) # the '*100' is in case I want to do hectares_it shouldn't matter
    valueLat = wtd.mean(preds$latitude, weights = weights_adj, na.rm=FALSE) 
    valueLon = wtd.mean(preds$longitude, weights = weights_adj, na.rm=FALSE) 
    if(is.na(valueLat)){
      print('Problem')
    }
    cent_preds26lat[j,k-3] = valueLat
    cent_preds26lon[j,k-3] = valueLon
  }
}  

