# Plot habitat suitability change by bormicon region 

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

# suitable spp
load ("Models/spp_Borm_suit.RData")


# Bormicon region ----
# using code from Plot_bt_by_bormicon.R
load ("Data/borm_coords.Rdata")

borm_ls <- borm_coords %>% 
  dplyr::select (subdivision, lon, lat) %>%
  split (borm_coords$subdivision)

# only want lon-lats in the list, not the names
borm_ls <- lapply(borm_ls, function(x) { x["subdivision"] <- NULL; x })

# convert to polygons, and add subdivision names back in
bp <- lapply (borm_ls, Polygon)
bpi <- lapply (seq_along (bp), function (i) Polygons(list(bp[[i]]), 
                                                     ID = names(borm_ls)[i]  ))
# convert to spatial polygons
borm_sp <- SpatialPolygons(bpi, proj4string = CRS("+proj=longlat +datum=WGS84"))

# convert to spatial polygons DF
borm_spdf <- SpatialPolygonsDataFrame(borm_sp,
                                      data.frame(id = unique(borm_coords$subdivision),
                                                 row.names = unique(borm_coords$subdivision)))


# ==================
# Calculate habitat change ----
# ==================

# spp I ran models for
load ("Models/spp_Smooth_latlon.RData") 
borm_spp <- spp_Smooth_latlon %>%
  filter (!sci_name_underscore == "Myoxocephalus_scorpius")


# historical function ----
sum_habitat_hist_borm <- function (sci_name) {
  
  # load raster brick for historical period
  hist_br <- brick (file.path ("Models/Prediction_bricks", 
                               paste(sci_name, "Borm_14_alltemp", "2000_2018.grd", sep = "_"))
  )
  
  
  # divide by bormicon regions
  h_borm <- raster::extract (hist_br, borm_spdf) # list of 38 subdivisions, with a x by 228 "matrix" "array"
  
  # I want to take a spatial sum for each time step. then group by division, take a sum. then take a temporal mean. 
  
  # Need to merge to larger divisions. Don't know how to do within a list, so convert to data frame
  h_borm_mn <- h_borm %>%
    # I want the sum of the values for each time step. so would have 38 subdivisions, with 228 values
    map (colSums, na.rm = TRUE) %>%
    # convert this to a data frame with 38 rows (subdivisions) and 228 columns (time step values)
    unlist () %>%
    matrix (nrow = 228) %>%
    t() %>%
    as.data.frame () %>%
    # append subdivision and division names and merge the divisions I merged for modeling
    mutate (subdivision = unique (borm_coords$subdivision),
            division = substr (subdivision, 1, 3),
            division = case_when (division == 115 ~ 113,
                                  division == 112 ~ 111,
                                  division == 110 ~ 109,
                                  TRUE ~ as.numeric(division))) %>%
    # convert to long so I can group_by division
    pivot_longer (cols = starts_with ("V"), 
                  names_to = "month", 
                  values_to = "value") %>%
    # now group by division and month and take a sum for each month
    group_by (division, month) %>%
    summarize (div_sum = sum (value)) %>%
    # now group by division and take a mean across all months
    group_by (division) %>%
    summarize (hist_mn = mean(div_sum), 
               hist_sd = sd(div_sum)) %>%
    mutate (species = sci_name)

  
}

hist_hab_borm_tmp <- hist_hab_borm

system.time (hist_hab_borm <- map_df (borm_suit, sum_habitat_hist_borm))

hist_hab_borm <- hist_hab_borm %>%
  mutate (species = rep (borm_suit, each = 12))

sum_habitat_future_borm <- function (sci_name, CM, scenario) {
  # CM is 4-letter lowercase climate model abbreviation
  # scenario is 245 or 585
  pred_br <- brick (file.path ("Models/Prediction_bricks", 
                               paste(sci_name, "Borm_14_alltemp", CM, scenario, "2061_2080.grd", sep = "_")
                               )
                    )
  
  # divide by bormicon regions
  p_borm <- raster::extract (pred_br, borm_spdf) # list of 38 subdivisions, with a x by 228 "matrix" "array"
  
  # I want to take a spatial sum for each time step. then group by division, take a sum. then take a temporal mean. 
  
  # Need to merge to larger divisions. Don't know how to do within a list, so convert to data frame
  p_borm_mn <- p_borm %>%
    # I want the sum of the values for each time step. so would have 38 subdivisions, with 228 values
    map (colSums, na.rm = TRUE) %>%
    # convert this to a data frame with 38 rows (subdivisions) and 228 columns (time step values)
    unlist () %>%
    matrix (nrow = 240) %>%
    t() %>%
    as.data.frame () %>%
    # append subdivision and division names and merge the divisions I merged for modeling
    mutate (subdivision = unique (borm_coords$subdivision),
            division = substr (subdivision, 1, 3),
            division = case_when (division == 115 ~ 113,
                                  division == 112 ~ 111,
                                  division == 110 ~ 109,
                                  TRUE ~ as.numeric(division))) %>%
    # convert to long so I can group_by division
    pivot_longer (cols = starts_with ("V"), 
                  names_to = "month", 
                  values_to = "value") %>%
    # now group by division and month and take a sum for each month
    group_by (division, month) %>%
    summarize (div_sum = sum (value)) %>%
    # now group by division and take a mean across all months
    group_by (division) %>%
    summarize (pred_mn = mean(div_sum), 
               pred_sd = sd(div_sum)) %>%
    mutate (species = sci_name,
            model = CM,
            scenario = scenario
    )
  
} # end pred function 

CM_list <- c("gfdl", "cnrm", "ipsl", "mohc", "CM26")

cm_expand <- expand_grid (sci_name = borm_suit,
                          CM = CM_list,
                          scenario = c(245, 585)) %>%
  filter (!(CM == "CM26" & scenario == 245)) %>% 
  as.list() # convert to list to feed to pmap


system.time(pred_hab_borm <- pmap_dfr (cm_expand, sum_habitat_future_borm)); beep() 

# combine and save
pred_hist_hab_borm <- pred_hab_borm %>%
  left_join (hist_hab_borm, by = c("species", "division"))
save (pred_hist_hab_borm, file = "Data/pred_hist_hab_borm_df.RData")


# calculate and plot habitat change ----
pred_hist_hab_borm %>%
  mutate (log10_foldchange = log10 (pred_mn / hist_mn)) %>%
  filter (scenario == 585) %>%
  ggplot (aes (x = as.factor(division), y = log10_foldchange)) +
  geom_jitter (aes(color = model), alpha = 0.3) +
  geom_boxplot(alpha = 0.2) +
  theme_bw()

# biggest high spread in 111, biggest lows in 197, 103, 101, 104, 108, 111


# order the borm regions by temperature, depth, etc. 
# use code from Plot_bt_by_bormicon. Maybe only take the mean from the 2000-2018 period, and then look at the future period too. 
min(which (year(glorys_dates) == 2000))

v_br <- raster::extract (bt_clip[[85:312]], borm_spdf) # list of 38 subdivisions, with a 1:x, 1:228 "matrix" "array"


# Need to merge to larger divisions. Don't know how to do within a list, so convert to data frame. 

# I could do something very hacky and embarrassing with rbind?
div1 <- rbind (v_br[[1]], v_br[[2]], v_br[[3]], v_br[[4]], v_br[[5]])
div2 <- rbind (v_br[[6]], v_br[[7]], v_br[[8]])
div3 <- rbind (v_br[[9]], v_br[[10]])
div4 <- rbind (v_br[[11]], v_br[[12]])
div5 <- rbind (v_br[[13]], v_br[[14]], v_br[[15]], v_br[[16]])
div6 <- v_br[[17]]
div7 <- v_br[[18]]
div8 <- rbind (v_br[[19]], v_br[[20]])
div9 <- rbind (v_br[[21]], v_br[[22]], v_br[[23]], v_br[[24]], v_br[[37]])
div11 <- rbind (v_br[[25]], v_br[[26]], v_br[[27]])
div13 <- rbind (v_br[[28]], v_br[[29]], v_br[[30]], v_br[[36]])
div14 <- rbind (v_br[[31]], v_br[[32]], v_br[[33]], v_br[[34]], v_br[[35]], v_br[[38]])

borm_bt_hist <- as.data.frame(rbind (div1, div2, div3, div4, div5, div6, div7, div8, div9, div11, div13, div14)) %>%
  mutate(
  division = c(
    rep (101, nrow (div1)),
    rep (102, nrow (div2)),
    rep (103, nrow (div3)),
    rep (104, nrow (div4)),
    rep (105, nrow (div5)),
    rep (106, nrow (div6)),
    rep (107, nrow (div7)),
    rep (108, nrow (div8)),
    rep (109, nrow (div9)),
    rep (111, nrow (div11)),
    rep (113, nrow (div13)),
    rep (114, nrow (div14))
  )
  ) %>%
  
  # convert to long so I can group_by division
  pivot_longer (cols = starts_with ("layer"), 
                names_to = "month", 
                values_to = "value") %>%
  group_by (division) %>%
  summarize (mean_bt = mean (value, na.rm = TRUE),
             sd_bt = sd (value, na.rm = TRUE)) %>%
  arrange (mean_bt)

# plot in order of temperature (past) ---
pred_hist_hab_borm %>%
  mutate (log10_foldchange = log10 (pred_mn / hist_mn)) %>%
  filter (scenario == 585) %>%
  left_join (borm_bt_hist, by = "division") %>%
  mutate (division = fct_reorder(as.factor(division), mean_bt)) %>%
  ggplot (aes (x = as.factor(division), y = log10_foldchange)) +
  geom_jitter (aes(color = model), alpha = 0.3) +
  geom_boxplot(alpha = 0.2) +
  theme_bw()


# try to add species therm pref?
png (file = "Figures/Hab_change_bormicon_by_temp.png", width = 16, height = 9, units = "in", res = 150)

pred_hist_hab_borm %>%
  mutate (log10_foldchange = log10 (pred_mn / hist_mn)) %>%
  filter (scenario == 585) %>%
  left_join (borm_bt_hist, by = "division") %>%
  mutate (division = fct_reorder(as.factor(division), mean_bt)) %>%
  rename (sci_name_underscore = "species") %>% 
  left_join (spp_list, by = "sci_name_underscore") %>%
  mutate (Therm_pref = case_when (
    mean_TB > 0 ~ "Warm",
    between (mean_TB, -3, 0) ~ "Cool",
    mean_TB < 0 ~ "Cold")
    ) %>%
  ggplot (aes (x = as.factor(division), y = log10_foldchange)) +
  geom_jitter (aes(color = Therm_pref, shape = model), alpha = 0.3) +
  geom_boxplot(alpha = 0.2) +
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  theme_bw() +
  labs (x = "Bormicon region, coldest to warmest",
        y = "log10-fold habitat change") +
  ggtitle ("Projected habitat change, scenario SSP-5.85, \nBormicon regions ordered coolest (113) to warmest (107) in 2000-2018 period") +
  theme (axis.text.y = element_text (size = 12),
         axis.text.x = element_text (size = 12),
         axis.title = element_text (size = 14),
         plot.title = element_text (size = 14),
         legend.title = element_text (size = 13),
         legend.text = element_text (size = 11))

dev.off()

# same with future?