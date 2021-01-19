# Turn MFRI Bormicon dataframe into polygons and raster
# 10/12/2020

# JGM

# table provided by Pamela Woods, 10/12/2020

# Divisions correspond to Figure 9.4 Gadget divisions of Stefánsson 2004 Development of structurally detailed statistically testable models of marine populations, "Habitat_regions_bormicon.pdf

# Subdivisions correspond to Figure 9.7

# https://stackoverflow.com/questions/48383990/convert-sequence-of-longitude-and-latitude-to-polygon-via-sf-in-r

library (sf)
library (tidyverse)
library (raster)

# overall table linking division, subdivision, and survey points. no points in 115
borm_table <- read_csv("Data/Raw_data/bormicon_table.csv") %>%
  dplyr::select (division, subdivision) %>% 
  unique() %>%
  mutate (subdivision = as.character(subdivision))

# missing 1141, 1151 subdivisions

# table with points defining the polygons of the subdivisions
borm_coords1 <- read_csv ("Data//Raw_data/subdivision_coords.csv")

# additional coordinates for 1145 and 1094 from Pamela
borm_coords2 <- read_csv ("Data/Raw_data/subdivision_coords2.csv")

# Clean borm_coords2--lots of overlapping points. ----

# did this manually. Points I can keep start at order 68 and go to 136. 
good_1094 <- borm_coords2 %>%
  filter (SUBDIVISION == 1094,
          order >= 68 & order < 137)

repair_1094 <- data.frame (
  SUBDIVISION = 1094,
  lat = c(60, 59, 59, 58.5, 58.5, 58.25, 58.25, 58, 58),
  lon = -1*c(29.5, 29.5, 30.5, 30.5, 31, 31, 31.5, 31.5, 32),
  order = c(1:9)
)
# also included the missing space for 1091. Was I saying 1096 but meant 1091 the whole time? I emailed Pamela about 1096 and have notes about 1096 below but can't find it. 1091 is in borm_coords??


good_1146 <- borm_coords2 %>%
  filter (SUBDIVISION == 1146,
          order >= 398)

repair_1146 <- data.frame (
  SUBDIVISION = 1146,
  lat = c(60.75, 60.5, 60.5, 60.25, 60.25, 60, 60, 59.75, 59.75, 59.5, 59.5,59, 59, 58.5, 58.5, 58.25, 58.25, 58, 58),
  lon = -1*c(27, 27, 27.5, 27.5, 28, 28, 28.5, 28.5, 29, 29, 29.5,29.5, 30.5, 30.5, 31, 31, 31.5, 31.5, 24),
  order = c(1:19)
)


borm_coords <- rbind (filter (borm_coords1, !SUBDIVISION %in% c(1146, 1094, 1091)),
                      repair_1094,
                      good_1094,
                      # not sure I still need this last point matching the first point to close the polygon, but including just in case
                      c(1094, repair_1094$lat[1], repair_1094$lon[1], "NA"),
                      repair_1146,
                      good_1146,
                      c(1146, repair_1146$lat[1], repair_1146$lon[1], "NA")
                      ) %>%
  #convert to lowercase to match other data frame
  rename (subdivision = SUBDIVISION) %>%
  left_join (borm_table, by = "subdivision") %>% 
  # If division is missing, replace with first 3 digits of subdivision
  mutate (division = ifelse (is.na (division),
                                    substr (subdivision, 1,3),
                                    division),
          subdivision = as.numeric (subdivision),
          lat = as.numeric (lat),
          lon = as.numeric (lon)
          ) 
# this still has a hole in it, for 1091 (1096??)


# save this for later use in analyzing rasters by bormicon region
save (borm_coords, file = "Data/borm_coords.RData")

# convert to sf polygons
# https://gis.stackexchange.com/questions/332427/converting-points-to-polygons-by-group
borm_sf <- borm_coords %>%
  st_as_sf (coords = c ("lon", "lat")) 

borm_polys <- st_sf (
  aggregate (
    borm_sf$geometry,
    list (borm_sf$subdivision),
    function (g) {
      st_cast (st_combine (g), "POLYGON")
    }
  )
)

borm_polys
plot (borm_polys) 

# save for using to join to landings
save (borm_polys, file = "Data/borm_polys.RData")

# try to add polygon names...this didn't work
poly_labels <- layer (sp.text (borm_polys$geometry, txt = borm_polys$Group.1, pos = 1))


# rasterize with template ----
#https://rdrr.io/cran/fasterize/man/fasterize.html
library (raster)
library (fasterize)

# standard raster template I've been using 
load ("Data/prediction_raster_template.RData")
subdiv_r <- fasterize (
  borm_polys,
  pred_r_template,
  field = "Group.1"
  
)

# reclassify to divisions ----

# 115 is not in the prediction data at all. going to join with 113--eastern extension. 112 is not well represented in the data (only 41 points for data with sst min/max). Going to join with 111 as a northern extension. 110 also not well represented (only 36 points), but not super clear how to handle it--represents Greenland shelf. I think makes more sense to join with 111 and 112 than with 109 for a larger prediction, but the portion of 110 that's within the EEZ is much more similar to 109. Will convert to 109 for now. 

# have to switch order of subdivision and division
borm_div <- borm_coords %>%
  dplyr::select (subdivision, division) %>%
  unique() %>%
  mutate (division = case_when (division == 115 ~ 113,
                                division == 112 ~ 111,
                                division == 110 ~ 109,
                                TRUE ~ as.numeric(division)))

borm_matrix <- data.matrix (borm_div)


borm_div_rcl <- raster::reclassify (subdiv_r, borm_matrix)
plot (borm_div_rcl)

# try to replace 109 gap--just didn't receive outlines for subdivision 1096. I'll define a box with the lat/lon boundaries of the gap, and fill any NA values in that box with 109
# https://gis.stackexchange.com/questions/281556/changing-raster-values-in-specific-zone-only

zone_109 <- extent(matrix (c(-30,59.5, -24, 63), nrow = 2))
plot (zone_109)

borm_div_rcl[zone_109][is.na(borm_div_rcl[zone_109])] <- 109

plot (borm_div_rcl)

# save raster----
borm_div_r <- writeRaster(borm_div_rcl, filename = "Data/bormicon_divisions.grd", overwrite = TRUE)

# plot ----

borm_div_rcl <- raster ("Data/bormicon_divisions.grd")
# can I plot this as a raster?
# https://datacarpentry.org/r-raster-vector-geospatial/02-raster-plot/
borm_r_poly <- rasterToPolygons(borm_div_rcl, dissolve = TRUE)

# https://erinbecker.github.io/r-raster-vector-geospatial/02-raster-plot/index.html
borm_r_pts <- rasterToPoints (borm_div_rcl, spatial = TRUE)
borm_r_df <- data.frame(borm_r_pts)

# redo label diagram?
div_centroids <- borm_coords %>%
  mutate (division = case_when (division == 115 ~ 113,
                                        division == 112 ~ 111,
                                        division == 110 ~ 109,
                                        TRUE ~ as.numeric(division))) %>%

  group_by (division) %>%
  summarise (lat = mean (lat, na.rm = TRUE),
             lon = mean (lon, na.rm = TRUE))



library (RColorBrewer)


# plot bormicon outlines with EEZ and survey points for study area figure ----

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

png ("Figures/Fig1_Map_EEZ_bormicon_survey_pts_season.png", width = 4, height = 4, units = "in", res = 300)

ggplot() +
 
  geom_sf (data  = eez, fill = NA, lwd = 1, lty = 1) +
  geom_point (data = mfri_pred, aes (x = lon, y = lat, shape = season), cex = 0.5) +
  geom_sf(data = borm_outline, lwd = 0.5, col = "red", lty = 2) +
  geom_polygon(data = map_data("world",'Iceland'), 
               aes(long,lat,group = group),
               col = 'black', fill = 'gray90',size = 0.3) +
  scale_shape_manual (values = c (1, 3)) +
  # autumn is point, spring is plus
  theme_bw() +
  theme (legend.position = "none",
         axis.text.x = element_text (size = 10),
         axis.text.y = element_text (size = 10),
         axis.title = element_blank ()) +
  lims (y = c(59, 70), x = c(-34, -4))

dev.off()


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


## plotting code from Pamela----

test <- read.csv('Data/Raw_data/subdivision_coords.csv')

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
  filter (SUBDIVISION %in% ldgs_sub) %>%
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
  theme_bw() 


ggplot() + #(data.frame(lat=0,lon=0), aes(lon,lat)) + 
  geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
               data=test,
               size = 0.3) +  
  geom_point(col='yellow') +  
  geom_path(aes(lon,lat,group=SUBDIVISION), 
            data=test,
            size = 0.3) +
   geom_text(aes(lon,lat,label=SUBDIVISION),
             data=summarise (group_by(test, SUBDIVISION, lat=mean(lat),
               lon=mean(lon)))) +
  # geom_polygon(data = map_data("world",'Iceland'), 
  #              aes(long,lat,group = group),
  #              col = 'black' ,fill = 'gray70',size = 0.3) +
  # geom_polygon(data=map_data("world",'Greenland'), 
  #              aes(long,lat,group = group), 
  #              col = 'black', fill = 'gray70') +
# this is my code, add ticks so I can see where the polygons meet
  geom_sf (data  = eez, fill = NA, lwd = 1.5) +
  scale_x_continuous(breaks = seq(min(test$lon, na.rm = TRUE), 
                                  max(test$lon, na.rm = TRUE), by = 1)) +
  scale_y_continuous(breaks = seq(min(test$lat, na.rm = TRUE), 
                                  max(test$lat, na.rm = TRUE), 
                                  by = 0.5)
  ) +
  # geom_polygon(data=geo::eyjar, col = 'black', fill = 'gray70',size = 0.3) +
  # geom_polygon(data=geo::faeroes, col = 'black', fill = 'gray70') +
  coord_map('mercator', xlim=c(-40,0),ylim=c(60,70)) +
  theme_bw() +
  theme(panel.border = element_rect(colour='black',size = 0.5),
        #legend.position=c(0.9, 0.85),
        panel.background = element_rect(fill='white'),#element_rect(fill='deepskyblue'),
        axis.ticks.length = unit(-5 , "points"),
        axis.ticks.margin = unit(10 * 1.25, "points"),
        axis.text.x = element_text(family='Arial',size=10),
        panel.grid=element_blank(),
        axis.title = element_blank())#,
        #legend.position = 'none')


### plot repaired polygons
test2 <-  borm_coords %>%
  mutate (lat = as.numeric (lat),
          lon = as.numeric (lon))

repair_centroids <- test2 %>%
  group_by (subdivision) %>%
  summarise (lat = mean (lat, na.rm = TRUE),
             lon = mean (lon, na.rm = TRUE))
ggplot () +
  geom_polygon(aes(lon,lat,group=subdivision, fill = as.factor(subdivision)),
               data=test2,
               size = 0.3) +  
  #geom_point(col='yellow') +  # don't know what this does
  
  # add outlines
  geom_path(aes(lon,lat,group=subdivision), 
            data=test2,
            size = 0.3) +
  
  geom_text(aes(lon,lat,label=subdivision),
            data = repair_centroids,
            col = "black") +
  geom_polygon(data = map_data("world",'Iceland'), 
               aes(long,lat,group = group),
               col = 'black' ,fill = 'gray70',size = 0.3) +
  geom_polygon(data=map_data("world",'Greenland'), 
               aes(long,lat,group = group), 
               col = 'black', fill = 'gray70') +
  geom_sf (data  = eez, fill = NA, lwd = 1.5) +
  coord_sf( xlim=c(-40,0),ylim=c(60,70)) +
  theme_bw() 

# plot by division
repair_div_centroids <- test2 %>%
  group_by (division) %>%
  summarise (lat = mean (lat, na.rm = TRUE),
             lon = mean (lon, na.rm = TRUE))

ggplot () +
  geom_polygon(aes(lon,lat,group=subdivision, fill = as.factor(division)),
               data=test2,
               size = 0.3) +  
  #geom_point(col='yellow') +  # don't know what this does
  
  # add outlines
  geom_path(aes(lon,lat,group=division), 
            data=test2,
            size = 0.3) +
  
  geom_text(aes(lon,lat,label=division),
            data = repair_div_centroids,
            col = "black") +
  geom_polygon(data = map_data("world",'Iceland'), 
               aes(long,lat,group = group),
               col = 'black' ,fill = 'gray70',size = 0.3) +
  geom_polygon(data=map_data("world",'Greenland'), 
               aes(long,lat,group = group), 
               col = 'black', fill = 'gray70') +
  geom_sf (data  = eez, fill = NA, lwd = 1.5) +
  coord_sf( xlim=c(-40,0),ylim=c(60,70)) +
  theme_bw() 

