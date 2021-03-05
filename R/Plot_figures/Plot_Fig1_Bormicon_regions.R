# plot bormicon outlines with EEZ and survey points for study area figure 1

library (sf)
library (tidyverse)
library (raster)
library (rgeos) # for borm region outlines

# bormicon raster
borm_div_rcl <- raster ("Data/bormicon_divisions.grd")

# iceland EEZ shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=5680
eez <- st_read("Data/eez.shp")

# survey points
mfri_pred <- read_csv ("Data/MFRI_predictor_df.csv",
                       col_types = cols(
                         sample_id = col_factor(),
                         
                         stat_sq = col_factor(),
                         bormicon_region = col_factor(),
                         sst_dev = col_number(),
                         bt_dev = col_number(),
                         sst_max = col_number(),
                         sst_min = col_number(),
                         bt_max = col_number(),
                         bt_min = col_number()
                       )
) %>%
  filter (!(year == 2011 & season == "autumn"),
          !(year < 2000 & season == "autumn"))  %>%
  # round lat/lon to reduce overlapping points
  mutate (lat = round (lat, 2), lon = round (lon, 2)) %>%
  distinct_at (vars(lat, lon, season))


# convert bormicon raster to polygon outlines
# https://stackoverflow.com/questions/34756755/plot-outline-around-raster-cells

borm_r_poly <- rasterToPolygons(borm_div_rcl, dissolve = TRUE)
borm_outline <- sf::st_as_sf (borm_r_poly) %>% st_cast("LINESTRING")


fig1_borm <- ggplot() +
  
  geom_sf (data  = eez, fill = NA, lwd = 1, lty = 1) +
  geom_point (data = mfri_pred, aes (x = lon, y = lat), cex = 0.5) +
  geom_sf(data = borm_outline, lwd = 0.5, col = "red", lty = 2) +
  geom_polygon(data = map_data("world",'Iceland'), 
               aes(long,lat,group = group),
               col = 'black', fill = 'gray90',size = 0.3) +
  #scale_shape_manual (values = c (1, 3)) +
  # autumn is point, spring is plus
  theme_bw() +
  theme (legend.position = "none",
         axis.text.x = element_text (size = 10),
         axis.text.y = element_text (size = 10),
         axis.title = element_blank ()) +
  lims (y = c(59, 70), x = c(-34, -4))



ggsave ("Figures/Fig1_Map_EEZ_bormicon_survey_pts.eps", fig1_borm, width = 170, height = 170, units = "mm", dpi = 300)


png ("Figures/Bormicon_division_EEZ_map_biglabels.png", width = 16, height = 9, units = "in", res = 300)
ggplot() +
  geom_raster(data = borm_r_df , aes(x = x, y = y, fill = as.factor(layer))) +
  
  # geom_path(aes(lon,lat,group=SUBDIVISION), 
  #           data=filter (test, SUBDIVISION %in% c(1151, 1101, 1121)),
  #           size = 0.3, lty = 2, col = "gray30") +
  
  geom_polygon(data = map_data("world",'Iceland'), 
               aes(long,lat,group = group),
               col = 'gray80' ,fill = 'gray90',size = 0.3) +
  
  geom_sf (data  = eez, fill = NA, lwd = 0.5) +
  # geom_text(aes(lon,lat,label=division),
  #           data = div_centroids,
  #           col = "black", size = 18) +
  #coord_sf( xlim=c(-40,0),ylim=c(60,70)) +
  coord_sf( xlim=c(-35,-5),ylim=c(60,70)) +
  theme_bw() +
  scale_fill_brewer (palette = "Set3") +
  theme (legend.position = "none",
         axis.text.x = element_text (size = 14),
         axis.text.y = element_text (size = 12),
         axis.title = element_blank (),
         plot.title = element_text (size = 45)) +
  ggtitle ("Bormicon regions")

dev.off()


