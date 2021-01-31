# Bring in MFRI landings data

# Pamela Woods sent landings data on 11/24/2020: Data are kg per haul, but I can also aggregate by month if it's just too big. I think most of the columns are self-explanatory, but all the spatial information just refers to the harbor where the landings were reported. The operations column may not actually be 100% correct - there is some strange data and no clear guidelines on how these values are reported or entered, so I don't know how informative they are but at least you can have a look. 

#The sampling_type = 'landings' is probably all you need - these are by Icelandic vessels, but sampling_type = 'foreign_landings' will need to be included if you are calculating total landings from the stock. I only passed data from 1993 on because record-keeping changed a lot around that time.

# the vessel designation shows vessel number followed by "-#". Every vessel number refers to the same actual vessel, but the "-#" refers to a different time in which there may be modifications made to the vessel. So we treat the whole thing as a 'vessel' even though truly the actual vessel is designated by the vessel number (without "-#).

# I took a look at the Periodic Permit more closely and it looks to me that most of the records mostly reflect the transition from effort based 'small boat' system to the current quota-based 'small boat' system that gradually happened around 2000 - 2004. 'Hook Permit' represents the small-boat system (which is quite a range in size - more like small industrial vessels rather than small-scale fisheries. Some papers by Maartje Oostdijk have a description of these). 

library (tidyverse)
library (beepr)
# very large file, >1 GB
# ldgs <- read.csv ("Data/Raw_data/MFRI_landings.csv"); beep() # crashes with read_csv?
# 
# Just look at domestic landings in the last decade for now
# ldgs %>%
#   filter (year > 2010, sampling_type == "landings") %>%
#   write_csv ("Data/MFRI_landings_last_decade.csv")


# bring in species table to match common names
spp_list <- read_csv ("Data/species_eng.csv") %>%
  rename (species = sci_name_underscore) %>% # rename to match species column
  mutate (Common_name = ifelse (is.na (Common_name), 
                                Scientific_name,
                                Common_name)
  ) # fill in any missing common names, for more legible graphs


# only look at last 10 years for now. did 2010-2020 just since 2020 isn't totally complete. 
ldgs <- read_csv ("Data/MFRI_landings_last_decade.csv"); beep()


sort (unique (ldgs$license_system))
length (unique (ldgs$harbor))

length (unique (ldgs$species)) # 94
sort (unique (ldgs$species)) 
# has lumpfish as 3 different categories: Lumfish_roe 949, "Lumpfish_(f)" 951, "Lumpfish_(m) 950" but not 48 which is what I used for my models. 

# ~8000 negative species weights measurements? most have NA harbor. assuming this is some kind of quota balancing thing??


# how to separate quota types? have GRT, operations, and license_system
ldgs_smallGRT <- ldgs %>% 
  filter (GRT <= 15)

table (ldgs_smallGRT$license_system)
table (ldgs_smallGRT$operations)

# for small, coastal fishing is the same, non commercial license has the same as Def fund. seaweed harvesting the same. hook permit = hook and line boat

# coastal fishing--4462 are >= 15, 320690 are <= 15

# hook permit is max 30 (as of 2013, but ), quota is big boats. 
hooks_lic <- ldgs %>% filter (license_system == "Hook permit")
hooks_op <- ldgs %>% filter (operations == "Hook and line boat")

# hook license_system includes operations: hook and line boat, hook permit (12), recreational fishery

hook_diff <- hooks_lic %>% 
  filter (vessel %in% hooks_op$vessel) %>%
  distinct_at (vars (vessel, registered_length, GRT, gear, operations, license_system))

str (hook_diff)

ldgs %>%
  group_by (license_system) %>%
  summarize (min = min (GRT, na.rm = TRUE),
             mean = mean (GRT, na.rm = TRUE),
             max = max (GRT, na.rm = TRUE))

#license vs ops contingency table?
lic_ops_cont <- table ( "ops" = ldgs$operations, "license" = ldgs$license_system)

# plan (see GAM method notes): split into coastal fishery, small quota (operations: small boat quota + hook and line boat + hook permit), large boat (trawler + quota vessel). this doesn't completely cleanly match 30 GRT. 

# this takes out recreational, seaweed, divers, lumpfishery (although there are still lumpfish landings in small boats), uncategorized, non-commercial

ldgs_3sector <- ldgs %>%
  mutate (Sector = case_when (
    operations == "Costal fishing" ~ "Coastal fishing",
    operations %in% c("Small quota boat", "Hook and line boat", "Hook permit") ~ "Small quota boats", 
    operations %in% c("Trawler", "Quota vessel") ~ "Large quota boats"
  )) %>%
  filter (!is.na (Sector))

# look at 15 vs larger, per cat's request
ldgs_15 <- ldgs %>%
  mutate (Vessel_size = ifelse (GRT > 15, "GRT > 15", "GRT <= 15"))
# doesn't really matter, none is exactly 30. but 412 are exactly 15

# map harbor points ----

harb_pts <- ldgs %>%
  distinct_at (vars (latitude, longitude, harbor)) %>%
  mutate (longitude = -(longitude))


# 80 harbors, but 59 unique lat and lon. several harbors have NA lat/lon, it's not that they're repeated. 65 harbors 2010-2020

# NOTE: longitude values are positive

library (leaflet)

harb_pts %>%
  leaflet () %>%
  addTiles() %>%
  #addCircleMarkers()
  addMarkers (~longitude, ~latitude,
              popup = ~as.character(harbor))



# missing ports

# https://www.freight-comparator.com/ports/88/iceland.html

missing_harb <- as.data.frame (rbind (
  c("Drangsnes", 65.6937, -21.4580),
  c("Arnarstapi", 64.7691, -23.6259),
  c("Ã\u0081rskÃ³gastrÃ¶nd / Hauganes", 65.9250, -18.3070),
  # Strandir is larger westfjords region. Using Kuvikur, https://guidetoiceland.is/connect-with-locals/regina/the-remote-strandir-in-the-westfjords-of-iceland
  c("Strandir",65.9312, -21.3354),
  c("BolungarvÃ­k", 66.1518, -23.2617),
  c("HaganesvÃ­k", 66.083, -19.167),
  c("BÃºÃ°ardalur", 65.1082, -21.7679),
  c("BÃ­ldudalur", 65.6864, -23.6006),
  c("SuÃ°ureyri",	 66.1311, -23.5272),
  c("MiÃ°sandur,HvalfirÃ°i", 64.4032, -21.4655),
	c("StÃ¶Ã°varfjÃ¶rÃ°ur", 64.8335, -13.8732),
	c("ReykhÃ³lar", 65.4450, -22.2066),
  c("BrjÃ¡nslÃ¦kur", 65.5301, -23.1904),
	c("MjÃ³ifjÃ¶rÃ°ur", 65.2038, -13.7902)
)
)

colnames (missing_harb) <- c("harbor", "latitude", "longitude")
missing_harb$latitude <- as.numeric (missing_harb$latitude)
missing_harb$longitude <- as.numeric (missing_harb$longitude)
	

missing_harb %>%
  leaflet () %>%
  addTiles() %>%
  #addCircleMarkers()
  addMarkers (~longitude, ~latitude,
              popup = ~as.character(harbor))

# add this to harb_pts. have to switch around because I set it up badly
missing_harb <- missing_harb %>%
  dplyr:: select (latitude, longitude, harbor)

harb_pts <- harb_pts %>%
  filter (!is.na (latitude)) %>%
  rbind (missing_harb)

  
# match harbors to bormicon polygons ----
# borm_polys from Rasterize_bormicon_regions.R

# https://mattherman.info/blog/point-in-poly/

library (sf)

load ("Data/borm_polys.RData")

# convert to sf
harb_sf <- harb_pts %>%
  filter (!is.na (latitude)) %>%
  st_as_sf (
    coords = c("longitude", "latitude")
  )


harb_borm <- st_join (
  harb_sf, 
  borm_polys, 
  join = st_within) %>%
  mutate (division = substr(Group.1, 1, 3)) %>%
  as_tibble()

# Calculate catch proportions for sectors and regions ----

# Rogers et al. used relative revenue from each species, which would be ideal. but I can do volume. might be able to get value from SI landings data, just translate from tonnes to ISK and do averages. They said risk based on catch proportion was 0.94 correlated with revenue risk. 


#start with proportion of species per sector caught in each region. Later might combine 104 and 105

# combine lumpfish so I can somewhat use it



borm_op_props <- ldgs_3sector %>%
  # fix lumpfish, just adults m/f
  mutate (species = ifelse (species %in% c(950, 951), 48, species)) %>%
  filter (weight_total > 0, !is.na (latitude)) %>%
  left_join (harb_borm, by = "harbor") %>%
  dplyr::select (division, species, Sector, weight_total) %>%
  group_by (division, Sector) %>%
  mutate (tot_wt = sum (weight_total)) %>%
  group_by (division, Sector, species) %>%
  mutate (spp_wt = sum (weight_total), 
          spp_prop_wt = spp_wt / tot_wt, 
          .keep = "unused") %>%
  ungroup() %>%
  distinct ()  %>%
  # convert spp_ID to scientific name
  rename (Spp_ID = species) %>%
  left_join (dplyr::select (spp_list, Spp_ID, species), by = "Spp_ID")


# quick vis--major species by sector and region
borm_op_props %>%
  filter (spp_prop_wt > 0.1) %>%
  ggplot () +
  geom_bar (aes(x = division, y = spp_prop_wt, fill = as.factor(species)), stat = "identity") +
  facet_wrap (~Sector)


# overall props by operation
op_props <- ldgs_3sector %>%
  # fix lumpfish, just adults m/f
  mutate (species = ifelse (species %in% c(950, 951), 48, species)) %>%
  filter (weight_total > 0, !is.na (latitude)) %>%
  dplyr::select (species, Sector, weight_total) %>%
  group_by (Sector) %>%
  mutate (tot_wt = sum (weight_total)) %>%
  group_by (Sector, species) %>%
  mutate (spp_wt = sum (weight_total), 
          spp_prop_wt = spp_wt / tot_wt, 
          .keep = "unused") %>%
  ungroup() %>%
  distinct ()  %>%
  # convert spp_ID to scientific name
  rename (Spp_ID = species) %>%
  left_join (dplyr::select (spp_list, Spp_ID, species), by = "Spp_ID")

# overall props by division
div_props <- ldgs_3sector %>%
  # fix lumpfish, just adults m/f
  mutate (species = ifelse (species %in% c(950, 951), 48, species)) %>%
  filter (weight_total > 0, !is.na (latitude)) %>%
  left_join (harb_borm, by = "harbor") %>%
  dplyr::select (species, division, weight_total) %>%
  group_by (division) %>%
  mutate (tot_wt = sum (weight_total),
          division = as.factor(division)) %>%
  group_by (division, species) %>%
  mutate (spp_wt = sum (weight_total), 
          spp_prop_wt = spp_wt / tot_wt, 
          .keep = "unused") %>%
  ungroup() %>%
  distinct ()  %>%
  # convert spp_ID to scientific name
  rename (Spp_ID = species) %>%
  left_join (dplyr::select (spp_list, Spp_ID, species), by = "Spp_ID")


# now, calculate projected habitat change by region ----

#  categorize my prediction rasters based on bormicon regions. 


# extract wants a patial polygons dataframe. Have to do with subdivisions since they're properly looped to be polygons.

# https://gis.stackexchange.com/questions/171124/data-frame-to-spatialpolygonsdataframe-with-multiple-polygons

# using borm_coords from Rasterize_bormicon_regions.R.

library (sp)
library (raster)

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


# function to extract raster values in bormicon regions, do similarly to plot_percent_habitat_change.r where I do historical, then future, then merge
borm_hist_hab_fun <- function (sci_name) {
  
  # load raster brick for historical period
  hist_br <- brick (file.path ("Models/Prediction_bricks", 
                               paste(sci_name, "Borm_14_alltemp", "2000_2018.grd", sep = "_"))
  )
  
  # extract values in each bormicon subdivision polygon
  v_br <- raster::extract (hist_br, borm_spdf) # list of 38 subdivisions, with a 1:x, 1:228 "matrix" "array"
  
  # Need to merge to larger divisions. Don't know how to do within a list, so convert to data frame
  v_br_sum <- v_br %>%
    # this takes the sum for each time step within each subdivision. returns a list of 38, each with a vector of 1:228
    map (colSums, na.rm = TRUE) %>%
    # convert this to a data frame with 38 rows (subdivisions) and 228 columns (time step values)
    unlist () %>%
    matrix (nrow = 228) %>% 
    t() %>%
    as.data.frame () %>%
    # append subdivision and division names, and merge the divisions I merged for modeling
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
    # take the overall sum of all values in the division for each time step
    group_by (division, month) %>%
    summarise (sum = sum(value, na.rm = TRUE)) %>%
    # take the temporal mean for each division
    group_by (division) %>%
    summarise (hist_mean = mean (sum, na.rm = TRUE),
               hist_sd = sd (sum, na.rm = TRUE)) %>%
    # add species name so I can build a df for all species
    mutate (species = sci_name)
  
} # end function



load ("Models/spp_Smooth_latlon.RData") 
borm_spp <- spp_Smooth_latlon %>%
  filter (!sci_name_underscore == "Myoxocephalus_scorpius")

# borm suit
load ("Models/spp_Borm_suit.RData")

# list of species that are landed and that I modeled
landed_spp <- spp_list %>%
  filter (species %in% borm_op_props$species, 
          species %in% borm_spp$sci_name_underscore) %>%
  pull (species)

system.time (hist_hab_borm <- map_df (borm_suit, borm_hist_hab_fun)); beep() # 638s


# future predictions

borm_pred_hab_fun <- function (sci_name, CM, scenario) {
  
  # load raster brick for historical period
  pred_br <- brick (file.path ("Models/Prediction_bricks", 
                               paste(sci_name, "Borm_14_alltemp", CM, scenario, "2061_2080.grd", sep = "_")
  )
  )
  
  # extract values in each bormicon subdivision polygon
  v_br <- raster::extract (pred_br, borm_spdf) # list of 38 subdivisions, with a 1:x, 1:228 "matrix" "array"
  
  # Need to merge to larger divisions. Don't know how to do within a list, so convert to data frame
  v_br_sum <- v_br %>%
    # this takes the sum for each time step within each subdivision. returns a list of 38, each with a vector of 1:228
    map (colSums, na.rm = TRUE) %>%
    # convert this to a data frame with 38 rows (subdivisions) and 240 columns (time step values)
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
    # take the overall sum of all values in the division for each time step
    group_by (division, month) %>%
    summarise (sum = sum(value, na.rm = TRUE)) %>%
    # take the temporal mean for each division
    group_by (division) %>%
    summarise (pred_mean = mean (sum, na.rm = TRUE),
               pred_sd = sd (sum, na.rm = TRUE)) %>%
    # add species name so I can build a df for all species
    mutate (species = sci_name,
            model = CM,
            scenario = scenario)
  
}

CM_list <- c("gfdl", "cnrm", "ipsl", "mohc", "CM26")

cm_expand <- expand_grid (sci_name = borm_suit,
                          CM = CM_list,
                          scenario = c(245, 585)) %>%
  filter (!(CM == "CM26" & scenario == 245)) %>% 
  as.list() # convert to list to feed to pmap


system.time(pred_hab <- pmap_dfr (cm_expand, borm_pred_hab_fun)); beep() # 6091.32 for landed spp, 6660.18 for borm_suit

#combine and save
pred_hist_hab_borm <- pred_hab %>%
  left_join (hist_hab_borm, by = c("species", "division"))
save (pred_hist_hab_borm, file = "Data/pred_hist_hab_borm_df_suitable.RData")


# calculate risk exposure/fishing opportunity: catch proportion * habitat change ----

# plot total biomass change for each region ----
load ("Data/pred_hist_hab_borm_df.RData")

hab_means_borm <- pred_hist_hab_borm  %>%
  filter (!species %in% c("Phycis_blennoides"), species %in% borm_suit) %>% # weird high values. borm_suit in percent_habitat change. need to combine these anyway
  
  group_by (species, scenario, division) %>%
  summarise (hab_future = mean(pred_mean),
             hab_hist = mean (hist_mean)) %>% 
  pivot_wider (names_from = scenario,
               values_from = hab_future, 
               names_prefix = "hab_") %>%
  pivot_longer (!c(species, division), 
                names_to = "period", 
                names_prefix = "hab_",
                values_to = "habitat")


top_hab_spp_borm <- hab_means_borm %>%
  group_by (species) %>%
  summarize (max_hab = max (habitat)) %>%
  
  left_join (spp_list, by = "species") %>% 
  top_n (7, max_hab)  # qual color palettes have 8-12 options

png ("Figures/Hab_change_overall_biomass_bormicon_commnames_suitable.png", width = 16, height = 9, res = 300, units = "in")
hab_means_borm %>%
  left_join (spp_list, by = "species") %>% 
  # use same levels as top_hab_spp for overall, for clarity in presentation
  mutate (Common_name = ifelse (species %in% top_hab_spp_borm$species, 
                                Common_name,
                                "Other"),
          # set factor order?
          period = factor (period, levels = c("hist", 245, 585)),
          Common_name = factor (Common_name, levels = c (top_hab_spp$Common_name, "Other"))
  ) %>% 
  ggplot (aes (x = period, y = habitat, fill = Common_name)) +
  facet_grid (.~division) +
  geom_bar (stat = "identity") +
  scale_fill_brewer (palette = "Dark2", name = "Species") +
  labs (y = "Suitable thermal habitat", x = "Period") +
  theme_bw() +
  theme (
    axis.text.x = element_text (size = 16, angle = 90),
    axis.text.y = element_text (size = 16),
    axis.title = element_text (size = 18),
    plot.title = element_text (size = 24),
    legend.text = element_text (size = 16),
    legend.title = element_text (size = 18),
    strip.text = element_text (size = 16)
  ) +
  ggtitle ("Total suitable thermal habitat in Iceland's EEZ by Bormicon region, all species")
dev.off()



### Risk exposure by region and sector  ----


# bring in GAM diagnostics for model suitability
MASE <- read_csv ("Models/GAM_performance_Borm_14_alltemp.csv")

suit_spp <- MASE %>%
  filter (MASE_GAM < 1, DM_GAM_p < 0.05) %>%
  pull (species)

# region and sector:
div_ops_risk <- pred_hist_hab_borm %>%
  mutate (division = as.character (division),
    perc_change = (pred_mean - hist_mean) / hist_mean, 
    log10_change = log10(pred_mean / hist_mean)) %>%
  inner_join (borm_op_props,
              by = c("species", "division")) %>%
  group_by (division, Sector, model, scenario) %>%
  summarise (risk = sum (log10_change * spp_prop_wt, na.rm = TRUE))


png ("Figures/Risk_exposure_region_sector_allspp.png", width = 16, height = 9, res = 300, units = "in") 
div_ops_risk %>%
  mutate (Sector = factor (Sector, levels = c ("Large quota boats", "Small quota boats", "Coastal fishing"))
          ) %>%
  ggplot () +
  geom_boxplot (aes (y = risk, x = division)) +
  facet_grid (scenario ~ Sector, scales = "free_x") +
  theme_bw () +
  geom_hline (yintercept  = 0, lty = 2) +
    theme (
      axis.text.x = element_text (size = 12, angle = 90),
      axis.text.y = element_text (size = 12),
      axis.title = element_text (size = 14),
      plot.title = element_text (size = 16),
      legend.text = element_text (size = 14),
      legend.title = element_text (size = 14)
    ) +
  labs (y = "Relative change in fishing opportunity", x = "Region") +
    ggtitle ("Catch-weighted regional climate impact risk by fisheries sector, all species")
dev.off()

# suitable species only
div_ops_risk_suitable <- pred_hist_hab_borm %>%
  filter (species %in% suit_spp) %>%
  mutate (division = as.character (division),
          perc_change = (pred_mean - hist_mean) / hist_mean, 
          log10_change = log10(pred_mean / hist_mean)) %>%
  inner_join (borm_op_props,
              by = c("species", "division")) %>%
  group_by (division, Sector, model, scenario) %>%
  summarise (risk = sum (log10_change * spp_prop_wt, na.rm = TRUE))

# also keep a version with species broken out to show species driving trends
div_ops_risk_with_spp <- pred_hist_hab_borm %>%
  filter (species %in% suit_spp) %>%
  mutate (division = as.character (division),
          perc_change = (pred_mean - hist_mean) / hist_mean,
          log10_change = log10(pred_mean / hist_mean)) %>%
  inner_join (borm_op_props, by = c("species", "division")) %>%
  # filter species with < 5% for a sector
  filter (spp_prop_wt > 0.05) %>%
  mutate (risk = perc_change * spp_prop_wt) %>%
  # would take average among models? median maybe to match boxplot
  group_by (species, scenario, division, Sector) %>%
  summarise (md_change = median (log10_change),
             prop_wt = first (spp_prop_wt)) %>%
  left_join (spp_list, by = "species") %>%
  mutate (Sector = factor (Sector, levels = c ("Large quota boats", "Small quota boats", "Coastal fishing")))


png ("Figures/Risk_exposure_region_sector_suitable_log10_3sector_spplabs.png", width = 16, height = 9, res = 300, units = "in") 
div_ops_risk_suitable %>%
  mutate (Sector = factor (Sector, levels = c ("Large quota boats", "Small quota boats", "Coastal fishing"))
                           ) %>%
  ggplot () +
  geom_boxplot (aes (y = risk, x = division)) +
  geom_text (aes (x = division, y = md_change, label = Common_name, size = prop_wt), data = div_ops_risk_with_spp) +
  facet_grid (scenario ~ Sector) +
  theme_bw () +
  geom_hline (yintercept  = 0, lty = 2) +
  scale_size_continuous (name = "Catch proportion", range = c(3, 8)) +
  theme (
    axis.text.x = element_text (size = 16),
    axis.text.y = element_text (size = 16),
    axis.title = element_text (size = 18),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 14),
    legend.title = element_text (size = 16),
    legend.text = element_text (size = 14)
    
  ) +
  labs (y = "Relative change in fishing opportunity", x = "Region") +
  ggtitle ("Catch-weighted regional climate impact risk by fisheries sector")
dev.off()




### plot opportunity/risk by region ----
div_risk <- pred_hist_hab_borm %>%
  mutate (division = as.character (division),
          perc_change = (pred_mean - hist_mean) / hist_mean) %>%
  inner_join (div_props, by = c("species", "division")) %>%
  group_by (division, model, scenario) %>%
  summarise (risk = sum (perc_change * spp_prop_wt, na.rm = TRUE)) 


div_risk_suitable <- pred_hist_hab_borm %>%
  filter (species %in% suit_spp) %>%
  mutate (division = as.character (division),
          perc_change = (pred_mean - hist_mean) / hist_mean, 
          log10_change = log10(pred_mean / hist_mean)) %>%
  inner_join (div_props, by = c("species", "division")) %>%
  group_by (division, model, scenario) %>%
  summarise (risk = sum (log10_change * spp_prop_wt, na.rm = TRUE)) 


# also keep a version with species broken out
div_risk_with_spp <- pred_hist_hab_borm %>%
  filter (species %in% suit_spp) %>%
  mutate (division = as.character (division),
          perc_change = (pred_mean - hist_mean) / hist_mean,
          log10_change = log10(pred_mean / hist_mean)) %>%
  inner_join (div_props, by = c("species", "division")) %>%
  # filter species with < 5% for a sector
  filter (spp_prop_wt > 0.05) %>%
  mutate (risk = perc_change * spp_prop_wt) %>%
  # would take average among models? median maybe to match boxplot
  group_by (species, scenario, division) %>%
  summarise (md_change = median (log10_change),
               prop_wt = first (spp_prop_wt)) %>%
  left_join (spp_list, by = "species")

png ("Figures/Risk_exposure_region_suitable_log10.png", width = 16, height = 9, res = 300, units = "in")  
div_risk_suitable %>%
  
  ggplot () +
  geom_boxplot (aes (x = factor(scenario), y = risk)) +
  #geom_point (aes (x = factor(scenario), y = risk), alpha = 0.5) +
  geom_text (aes (x = factor(scenario), y = md_change, label = Common_name, size = prop_wt), data = div_risk_with_spp, alpha = 0) +
  facet_grid (.~division) +
  theme_bw () +
  geom_hline (yintercept  = 0, lty = 2) +
  scale_size_continuous (name = "Catch proportion", range = c(3, 8)) +
  labs (y = "Relative change in fishing opportunity", x = "Scenario") +
  ggtitle ("Catch-weighted climate impact risk by region") +
  theme (
    axis.text.x = element_text (size = 16),
    axis.text.y = element_text (size = 16),
    axis.title = element_text (size = 18),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 14),
    legend.title = element_text (size = 16),
    legend.text = element_text (size = 14)
    
  )
dev.off() 



### plot overall opportunity/risk change by sector ----
# pred_hist_hab from plot_percent_habitat_change
load ("Data/pred_hist_hab_df.RData")

ops_risk <- pred_hist_hab %>%
  mutate (perc_change = (pred_mean - hist_mean) / hist_mean,
          log10_change = log10(pred_mean / hist_mean)) %>%
  inner_join (op_props,
              by = c("species")) %>%
  group_by (Sector, model, scenario) %>%
  summarise (risk = sum (log10_change * spp_prop_wt, na.rm = TRUE))


png ("Figures/Risk_exposure_sector_allspp.png", width = 16, height = 9, res = 300, units = "in")  
ops_risk %>%
  mutate (Sector = factor (Sector, levels = c ("Large quota boats", "Small quota boats", "Coastal fishing"))
  ) %>%
ggplot () +
  geom_boxplot (aes (x = factor(scenario), y = risk)) +
  #geom_point (aes (x = factor(scenario), y = risk), alpha = 0.5) +
  facet_grid (.~Sector) +
  theme_bw () +
  geom_hline (yintercept  = 0, lty = 2) +
  labs (y = "Relative change in fishing opportunity", x = "Scenario") +
  ggtitle ("Catch-weighted climate impact risk by fisheries sector") +
  theme (
    axis.text.x = element_text (size = 14),
    axis.text.y = element_text (size = 12),
    axis.title = element_text (size = 14),
    plot.title = element_text (size = 16)
    
  )
dev.off() 

# try this with suitable species only
# bring in GAM diagnostics for model suitability
MASE <- read_csv ("Models/GAM_performance_Borm_14_alltemp.csv")

suit_spp <- MASE %>%
  filter (MASE_GAM < 1, DM_GAM_p < 0.05) %>%
  pull (species)

ops_risk_suitable <- pred_hist_hab %>%
  filter (species %in% suit_spp) %>%
  mutate (perc_change = (pred_mean - hist_mean) / hist_mean, 
          log10_change = log10(pred_mean / hist_mean)) %>%
  inner_join (filter (op_props, 
                      !Sector %in% c("Def fund", "Research vessel", "Hook permit", "Quota permit", "Uncategorised")),
              by = c("species")) %>%
  group_by (Sector, model, scenario) %>%
  summarise (risk = sum (log10_change * spp_prop_wt, na.rm = TRUE)) 

# try to do something like Rogers where you plot the individual component species too. mean habitat change, with size proportional to spp_prop_wt
ops_risk_with_spp <- pred_hist_hab %>%
  filter (species %in% suit_spp) %>%
  mutate (perc_change = (pred_mean - hist_mean) / hist_mean, 
          log10_change = log10(pred_mean / hist_mean)) %>%
  inner_join (op_props,
              by = c("species")) %>%
  # filter out species with < 5% for a sector
  filter (spp_prop_wt > 0.01) %>%
  group_by (species, scenario, Sector) %>%
  summarise (md_change = median (log10_change),
             prop_wt = first (spp_prop_wt)) %>%
  left_join (spp_list, by = "species") %>% 
  mutate (Sector = factor (Sector, levels = c ("Large quota boats", "Small quota boats", "Coastal fishing"))
  )

png ("Figures/Risk_exposure_sector_suitable_spplabs_log10_3sector.png", width = 16, height = 9, res = 300, units = "in") 
ops_risk_suitable %>%
  mutate (Sector = factor (Sector, levels = c ("Large quota boats", "Small quota boats", "Coastal fishing"))) %>%
  ggplot () +
  geom_boxplot (aes (x = factor(scenario), y = risk)) +
  #geom_point (aes (x = factor(scenario), y = md_change, size = prop_wt), alpha = 0.5, data = ops_risk_with_spp) +
  geom_text (aes (x = factor (scenario), y = md_change, size = prop_wt, label = Common_name), alpha = 0.6, data = ops_risk_with_spp) +
  facet_grid (.~Sector) +
  theme_bw () +
  geom_hline (yintercept  = 0, lty = 2) +
  scale_size_continuous (name = "Catch proportion", range = c(3, 8)) +
  labs (y = "Relative change in fishing opportunity", x = "Scenario") +
  ggtitle ("Catch-weighted climate impact risk by fisheries sector") +

  theme (
    axis.text.x = element_text (size = 16),
    axis.text.y = element_text (size = 16),
    axis.title = element_text (size = 18),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 14),
    legend.title = element_text (size = 16),
    legend.text = element_text (size = 14)
    
  )
dev.off()
#############################3  


# look at vessels by 15 GRT, per Cat's request ----

# try to do iceland names?
isl_spp <- read_csv ("Data/Raw_data/species.csv") %>%
  rename (Spp_ID = tegund)

spp_list_isl <- left_join (spp_list, isl_spp, by = "Spp_ID")

# look at 15 vs larger, per cat's request
ldgs_15 <- ldgs %>%
  mutate (Vessel_size = ifelse (GRT > 15, "GRT > 15", "GRT <= 15")) %>%
  filter (!is.na (Vessel_size))
# doesn't really matter, none is exactly 30. but 412 are exactly 15

borm_op_props_15 <- ldgs_15 %>%
  # fix lumpfish, just adults m/f
  mutate (species = ifelse (species %in% c(950, 951), 48, species)) %>%
  filter (weight_total > 0, !is.na (latitude)) %>%
  left_join (harb_borm, by = "harbor") %>%
  dplyr::select (division, species, Vessel_size, weight_total) %>%
  group_by (division, Vessel_size) %>%
  mutate (tot_wt = sum (weight_total)) %>%
  group_by (division, Vessel_size, species) %>%
  mutate (spp_wt = sum (weight_total), 
          spp_prop_wt = spp_wt / tot_wt, 
          .keep = "unused") %>%
  ungroup() %>%
  distinct ()  %>%
  #convert spp_ID to scientific name
  rename (Spp_ID = species) %>%
  left_join (dplyr::select (spp_list, Spp_ID, species), by = "Spp_ID")
  
  


# suitable species only
div_ops_risk_suitable_15 <- pred_hist_hab_borm %>%
  filter (species %in% suit_spp) %>%
  mutate (division = as.character (division),
          perc_change = (pred_mean - hist_mean) / hist_mean, 
          log10_change = log10(pred_mean / hist_mean)) %>%
  inner_join (borm_op_props_15,
              by = c("species", "division")) %>%
  group_by (division, Vessel_size, model, scenario) %>%
  summarise (risk = sum (log10_change * spp_prop_wt, na.rm = TRUE))

# also keep a version with species broken out to show species driving trends
div_ops_risk_with_spp_15 <- pred_hist_hab_borm %>%
  filter (species %in% suit_spp) %>%
  mutate (division = as.character (division),
          perc_change = (pred_mean - hist_mean) / hist_mean,
          log10_change = log10(pred_mean / hist_mean)) %>%
  inner_join (borm_op_props_15, by = c("species", "division")) %>%
  # filter species with < 5% for a sector
  filter (spp_prop_wt > 0.05) %>%
  mutate (risk = perc_change * spp_prop_wt) %>%
  # would take average among models? median maybe to match boxplot
  group_by (species, scenario, division, Vessel_size) %>%
  summarise (md_change = median (log10_change),
             prop_wt = first (spp_prop_wt)) %>%
  left_join (spp_list, by = "species") 


png ("Figures/Risk_exposure_region_sector_suitable_log10_GRT15_spplabs.png", width = 16, height = 9, res = 300, units = "in") 
div_ops_risk_suitable_15 %>%

  ggplot () +
  geom_boxplot (aes (y = risk, x = division)) +
  geom_text (aes (x = division, y = md_change, label = Common_name, size = prop_wt), data = div_ops_risk_with_spp_15) +
  facet_grid (scenario ~ Vessel_size) +
  theme_bw () +
  geom_hline (yintercept  = 0, lty = 2) +
  scale_size_continuous (name = "Catch proportion", range = c(3, 8)) +
  theme (
    axis.text.x = element_text (size = 16),
    axis.text.y = element_text (size = 16),
    axis.title = element_text (size = 18),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 14),
    legend.title = element_text (size = 16),
    legend.text = element_text (size = 14)
    
  ) +
  labs (y = "Relative change in fishing opportunity", x = "Region") +
  ggtitle ("Catch-weighted regional climate impact risk by vessel size")
dev.off()

# gRT 15 by sector only ----
# overall props by operation
op_props_15 <- ldgs_15 %>%
  # fix lumpfish, just adults m/f
  mutate (species = ifelse (species %in% c(950, 951), 48, species)) %>%
  filter (weight_total > 0, !is.na (latitude)) %>%
  dplyr::select (species, Vessel_size, weight_total) %>%
  group_by (Vessel_size) %>%
  mutate (tot_wt = sum (weight_total)) %>%
  group_by (Vessel_size, species) %>%
  mutate (spp_wt = sum (weight_total), 
          spp_prop_wt = spp_wt / tot_wt, 
          .keep = "unused") %>%
  ungroup() %>%
  distinct ()  %>%
  # convert spp_ID to scientific name
  rename (Spp_ID = species) %>%
  left_join (dplyr::select (spp_list, Spp_ID, species), by = "Spp_ID")

ops_risk_suitable_15 <- pred_hist_hab %>%
  filter (species %in% suit_spp) %>%
  mutate (perc_change = (pred_mean - hist_mean) / hist_mean, 
          log10_change = log10(pred_mean / hist_mean)) %>%
  inner_join (op_props_15,
              by = c("species")) %>%
  group_by (Vessel_size, model, scenario) %>%
  summarise (risk = sum (log10_change * spp_prop_wt, na.rm = TRUE)) 

# try to do something like Rogers where you plot the individual component species too. mean habitat change, with size proportional to spp_prop_wt
ops_risk_with_spp <- pred_hist_hab %>%
  filter (species %in% suit_spp) %>%
  mutate (perc_change = (pred_mean - hist_mean) / hist_mean, 
          log10_change = log10(pred_mean / hist_mean)) %>%
  inner_join (op_props_15,
              by = c("species")) %>%
  # filter out species with < 5% for a sector
  filter (spp_prop_wt > 0.01) %>%
  group_by (species, scenario, Vessel_size) %>%
  summarise (md_change = median (log10_change),
             prop_wt = first (spp_prop_wt)) %>%
  left_join (spp_list, by = "species") 

png ("Figures/Risk_exposure_sector_suitable_log10_GRT15_spplabs.png", width = 16, height = 9, res = 300, units = "in") 
ops_risk_suitable %>%
  
  ggplot () +
  geom_boxplot (aes (x = factor(scenario), y = risk)) +
  #geom_point (aes (x = factor(scenario), y = md_change, size = prop_wt), alpha = 0.5, data = ops_risk_with_spp) +
  geom_text (aes (x = factor (scenario), y = md_change, size = prop_wt, label = Common_name), alpha = 0.6, data = ops_risk_with_spp) +
  facet_grid (.~Vessel_size) +
  theme_bw () +
  geom_hline (yintercept  = 0, lty = 2) +
  scale_size_continuous (name = "Catch proportion", range = c(3, 8)) +
  labs (y = "Relative change in fishing opportunity", x = "Scenario") +
  ggtitle ("Catch-weighted climate impact risk by vessel size") +
  
  theme (
    axis.text.x = element_text (size = 16),
    axis.text.y = element_text (size = 16),
    axis.title = element_text (size = 18),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 14),
    legend.title = element_text (size = 16),
    legend.text = element_text (size = 14)
    
  )
dev.off()


#############################################################################

# http://www.spectdata.com/index.php/2018/10/25/how-to-use-ggplot-to-plot-pie-charts-on-a-map/
library (maps)
library (mapdata)
library (sf)
library (rnaturalearth)
library (rnaturalearthdata)
library (rgeos)
library (viridis)


library (scatterpie)
# https://www.gl-li.com/2018/01/17/create-pie-plots-on-a-map-in-r/

# worldmap
world <- ne_countries(scale = "medium", returnclass = "sf")

harb_ops <- harb_ldgs %>%
  select (-species) %>%
  group_by (harbor, Sector) %>%
  mutate (value = sum (tot_wt), .keep = "unused") %>%
  ungroup() %>%
  distinct () 

ggplot() +
  geom_sf(data = world) +
  coord_sf (xlim = c(-25, -12), ylim = c (63,67), expand = FALSE ) +
  geom_bar (data = harb_ops, 
            aes (x = ))
  geom_point (data = harb_ops,
              aes (x = -longitude,
                   y = latitude, 
                   col = Sector, 
                   size = value), alpha = 0.5)
  geom_scatterpie (data = harb_ops, aes (x = longitude,
                        y = latitude),
                   cols = "Sector", long_format = TRUE)
  
  # plot harbors with bormicon regions
  borm <- read.csv('Data/Raw_data/subdivision_coords.csv') %>%
    # add division
    mutate (division = str)
  
  # iceland EEZ shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=5680
  eez <- st_read("Data/eez.shp")
  isl_eez <- fortify (eez)
  
  # plot polygons with eez overlaid
  
  # labels are just showing up in the middle of iceland, not separated by polygon. try making a separate dataframe outside?
  label_centroids <- test %>%
    group_by (SUBDIVISION) %>%
    summarise (lat = mean (lat, na.rm = TRUE),
               lon = mean (lon, na.rm = TRUE))
  test %>%
   
    ggplot () +
    geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
                 #data=test,
                 size = 0.3) +  
    #geom_point(col='yellow') +  # don't know what this does
    
    # add outlines
    geom_path(aes(lon,lat,group=SUBDIVISION), 
              data=test,
              size = 0.3) +
    
    geom_text(aes(lon,lat,label=SUBDIVISION),
              data = label_centroids,
              col = "black") +
    geom_polygon(data = map_data("world",'Iceland'), 
                 aes(long,lat,group = group),
                 col = 'black' ,fill = 'gray70',size = 0.3) +
    geom_polygon(data=map_data("world",'Greenland'), 
                 aes(long,lat,group = group), 
                 col = 'black', fill = 'gray70') +
    geom_sf (data  = eez, fill = NA, lwd = 1.5) +
    coord_sf( xlim=c(-40,0),ylim=c(60,70)) +
    theme_bw() +
    geom_point (aes (-longitude, latitude),
                data = ldgs_pts)
  
  
 