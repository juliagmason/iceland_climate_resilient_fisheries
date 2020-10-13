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


good_1146 <- borm_coords2 %>%
  filter (SUBDIVISION == 1146,
          order >= 398)

repair_1146 <- data.frame (
  SUBDIVISION = 1146,
  lat = c(60.75, 60.5, 60.5, 60.25, 60.25, 60, 60, 59.75, 59.75, 59.5, 59.5,59, 59, 58.5, 58.5, 58.25, 58.25, 58, 58),
  lon = -1*c(27, 27, 27.5, 27.5, 28, 28, 28.5, 28.5, 29, 29, 29.5,29.5, 30.5, 30.5, 31, 31, 31.5, 31.5, 24),
  order = c(1:19)
)


borm_coords <- rbind (borm_coords1,
                      filter (borm_coords2, !SUBDIVISION %in% c(1146, 1094)),
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
  # if division is missing, replace with first 3 digits of subdivision
  mutate (division = ifelse (is.na (division),
                                    substr (subdivision, 1,3),
                                    division),
          subdivision = as.numeric (subdivision)
          ) %>%
  # get rid of subdivisions with no lat/lon. 1094, 1096, 1146 all NA; none is in borm_table. These are huge areas though, so try to get them!
  filter (!is.na(lat))




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

# 115 is not in the prediction data at all. going to join with 113.not in borm table either 
# also need to figure out something with 109

# have to switch order of subdivision and division
borm_div <- borm_coords %>%
  dplyr::select (subdivision, division) %>%
  unique() %>%
  mutate (division = ifelse (division == 115, 113, as.numeric(division)))

borm_matrix <- data.matrix (borm_div)


borm_div_rcl <- raster::reclassify (subdiv_r, borm_matrix)
plot (borm_div_rcl)

# try to replace 109 gap
# https://gis.stackexchange.com/questions/281556/changing-raster-values-in-specific-zone-only

zone_109 <- extent(matrix (c(-30,59.5, -24, 63), nrow = 2))
plot (zone_109)

borm_div_rcl[zone_109][is.na(borm_div_rcl[zone_109])] <- 109

plot (borm_div_rcl)

# save raster
borm_div_r <- writeRaster(borm_div_rcl, filename = "Data/bormicon_divisions.grd", overwrite = TRUE)



## plotting code from Pamela----

test <- read.csv('Data/Raw_data/subdivision_coords2.csv')


ggplot(data.frame(lat=0,lon=0), aes(lon,lat)) + 
  geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
               data=filter (test, SUBDIVISION %in% c(1091, 1092, 1093, 1094, 1095, 1145)),
               size = 0.3) +  
  geom_point(col='yellow') +  
  geom_path(aes(lon,lat,group=SUBDIVISION), 
            data=filter (test, SUBDIVISION %in% c(1091, 1092, 1093, 1094, 1095, 1146)),
            size = 0.3) +
  #  geom_text(aes(lon,lat,label=SUBDIVISION),
  #            data=ddply(test,~SUBDIVISION, summarise, lat=mean(lat),
  #              lon=mean(lon))) +
  geom_polygon(data = map_data("world",'Iceland'), 
               aes(long,lat,group = group),
               col = 'black' ,fill = 'gray70',size = 0.3) +
  geom_polygon(data=map_data("world",'Greenland'), 
               aes(long,lat,group = group), 
               col = 'black', fill = 'gray70') +
# this is my code, add ticks so I can see where the polygons meet
  scale_x_continuous(breaks = seq(min(test$lon, na.rm = TRUE), 
                                  max(test$lon, na.rm = TRUE), by = 1)) +
  scale_y_continuous(breaks = seq(min(test$lat, na.rm = TRUE), 
                                  max(test$lat, na.rm = TRUE), 
                                  by = 0.5)
  )
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
