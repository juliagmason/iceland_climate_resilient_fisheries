# Plot Iceland EEZ, survey points, and depth contours for Figure 1

library (sf)
library (tidyverse)
library (raster)
library (marmap) # for bathymetry lines



# iceland EEZ shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=5680
eez <- st_read("Data/eez.shp")


# Depth 

depth <- getNOAA.bathy(lon1 = -45, lon2 = 15,
                       lat1 = 50, lat2 = 85, 
                       resolution = 4)

plot (depth, deep = -6000, shallow = 0, step= 1000, label = TRUE)

# convert to df for ggplot
# https://stackoverflow.com/questions/50119592/plot-bathymetry-and-coastline-using-ggplot2-and-marmap
depth_m <- as.matrix (depth)
class (depth_m) <- "matrix"
depth_df <- depth_m %>%
  
  as.data.frame() %>%
  rownames_to_column(var = "lon") %>%
  gather(lat, value, -1) %>%
  mutate_all(funs(as.numeric)) 
# this throws a warming about dplyr and funs()


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


fig1_map <- ggplot() +
  
  geom_sf (data  = eez, fill = NA, lwd = 0.75, lty = 1) +
  geom_point (data = mfri_pred, aes (x = lon, y = lat, shape = season), cex = 1.5, fill = "white", alpha = 0.7) +
 
  scale_shape_manual (values = c (3, 21)) +
  #autumn is point, spring is plus
  # plot depth contours, 1000m
  geom_contour(aes(x = lon, y = lat, z = value), binwidth = 500, colour = "gray70", data = depth_df) +
  # plot iceland country polygon
  geom_polygon(data = map_data("world",'Iceland'), 
               aes(long,lat,group = group),
               col = 'black', fill = 'gray90', size = 0.3) +
  theme_bw() +
  theme (legend.position = "none",
         axis.text.x = element_text (size = 10),
         axis.text.y = element_text (size = 10),
         axis.title = element_blank ()) +
  lims (y = c(59, 70), x = c(-34, -4))

ggsave ("Figures/Fig1_Map_survey_pts.eps", fig1_map, width = 170, height = 170, units = "mm", dpi = 300)

ggsave("Figures/Fig1_Map_survey_pts.png", fig1_map, width = 170, height = 170, units = "mm", dpi = 300)


# 85 mm version?

fig1_map <- ggplot() +
  

  # plot depth contours, 1000m
  geom_contour(aes(x = lon, y = lat, z = value), binwidth = 500, colour = "gray70", data = depth_df, size = 0.25) +
               
  # plot iceland country polygon
  geom_polygon(data = map_data("world",'Iceland'), 
               aes(long,lat,group = group),
               col = 'black', fill = 'gray90', size = 0.3) +
  
  geom_sf (data  = eez, fill = NA, size = 0.5, lty = 1) +
  
  
  geom_point (data = mfri_pred, aes (x = lon, y = lat, shape = season), cex = 0.5, fill = "white") +
  scale_shape_manual (values = c (3, 21)) +
  #autumn is point, spring is plus
  
  theme_bw() +
  theme (legend.position = "none",
         axis.text.x = element_text (size = 6),
         axis.text.y = element_text (size = 6),
         axis.title = element_blank ()) +
  lims (y = c(59, 70), x = c(-34, -4))

ggsave("Figures/Fig1_Map_survey_pts.png", fig1_map, width = 85, height = 85, units = "mm", dpi = 300)
ggsave("Figures/Fig1_Map_survey_pts.eps", fig1_map, width = 85, height = 85, units = "mm", dpi = 300)
