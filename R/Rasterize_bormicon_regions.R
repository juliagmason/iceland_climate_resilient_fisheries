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
  #dplyr::select (division, subdivision) %>% 
  unique() %>%
  mutate (subdivision = as.character (subdivision))

# missing 1141, 1151 subdivisions

# table with points defining the polygons of the subdivisions
borm_coords1 <- read_csv ("Data//Raw_data/subdivision_coords.csv")

# additional coordinates for 1145 and 1094 from Pamela
borm_coords2 <- read_csv ("Data/Raw_data/subdivision_coords2.csv")

# Clean borm_coords2--lots of overlapping points. 

borm_coords <- test4 %>%#rbind (borm_coords1, borm_coords2) %>%
  #convert to lowercase to match other data frame
  rename (subdivision = SUBDIVISION) %>%
  left_join (borm_table, by = "subdivision") %>% 
  # if division is missing, replace with first 3 digits of subdivision
  mutate (division = ifelse (is.na (division),
                                    substr (subdivision, 1,3),
                                    division)
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
borm_matrix <- data.matrix (cbind(borm_table$subdivision, borm_table$division))


borm_div_rcl <- raster::reclassify (subdiv_r, borm_matrix)
plot (borm_div_rcl)

ifelse (between (lat))

# save raster
borm_div_r <- writeRaster(borm_div_rcl, filename = "Data/bormicon_divisions.grd")



## plotting code from Pamela----

test <- read.csv('Data/Raw_data/subdivision_coords2.csv')
test <- rbind (borm_coords1, borm_coords2)

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

# try to find where the terrible points start
test_1094 <- test %>% filter (SUBDIVISION == 1094)

# looks like some points are duplicated, this could be somewhat causing this
# https://stackoverflow.com/questions/6986657/find-duplicated-rows-based-on-2-columns-in-data-frame-in-r
test_1094 %>%
  group_by(lat, lon) %>% 
  mutate(dupe = n()>1) %>%
  filter (dupe == TRUE) %>% View()

test_1094_distinct <- test_1094 %>%
  distinct (lat, lon, .keep_all = TRUE)

ggplot(data.frame(lat=0,lon=0), aes(lon,lat)) + 
  geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
               data=test_1094_distinct,
               size = 0.3)
# this is worse...


# bottom border should be 58. where will it line up with 1146?
borm_coords2 %>%
  filter (SUBDIVISION == 1146) %>%
  summarize (min = min (lon)) # -12.5, -32

borm_coords2 %>%
  filter (SUBDIVISION == 1094,
          lat < 60) %>%
  summarize (min = min (lon), max = max(lon)) # -32, 30
  

# right border should be min of 1092
borm_coords1 %>%
  filter (SUBDIVISION == 1092) %>%
  summarize (min = min(lon)) # =29.5

# 8 ponts have NA order, but don't seem to help
ggplot(data.frame(lat=0,lon=0), aes(lon,lat)) + 
  geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
               data=test_1094,
               size = 0.3) +  
  geom_point(aes(lon,lat), data = filter (test_1094, is.na(order)), col = "black")


# problem points are all within the first 50, that's at least helpful! also all below 60, I think. also weird that NA order don't match point 1

# 139, last point, is also weird. should maybe end at 138. 

# below 50, min lon should be -32 for all of them, according to figure? weird then that 1146 has a 32 lon, shouldn't reach that far. this is i

# plot first few and last few to see where the polygon starts
ggplot(data.frame(lat=0,lon=0), aes(lon,lat)) + 
  geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
               data=test_1094,
               size = 0.3,
               fill = "darkgray") +  
  geom_point(aes(lon,lat, col = factor(order)), data = filter (test_1094, order %in% c(1:15,135:139)))
  

test_1094 %>% filter (lon == -29.5) %>% View()
# 136 is -29.5 and 60.25. probably want one that's -29.5 and 60, fill up some of the empty space. then go down to -29.5, 59, then across to -30.5 maybe? down to 58.5, left one, down one, left one, down to 58, 31.75?

repair_1094 <- data.frame (
  SUBDIVISION = 1094,
  lat = c(60, 59, 59, 58.5, 58.5, 58.25, 58.25, 58, 58),
  lon = as.numeric(c(-29.5, -29.5, -30.5, -30.5, -31, -31, -31.5, -31.5, -32)),
  order = c(1:9)
)

# this is the inflection point I want, with lon -32
test_1094[which (test_1094$lat == 62.5 & test_1094$lon == -32),]

ggplot(data.frame(lat=0,lon=0), aes(lon,lat)) + 
  geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
               data=test_1094,
               size = 0.3,
               fill = "darkgray") +  
  geom_point(aes(lon,lat, col = factor(order)), data = filter (test_1094, order ==68))

# so the points I can keep start at 68 and go to 136
good_1094 <- test_1094 %>%
  filter (order >= 68 & order < 137)

test2_1094 <- rbind (repair_1094, good_1094, c(1094, repair_1094$lat[1], repair_1094$lon[1], "NA"))
test2_1094$lon <- as.numeric (test2_1094$lon)
test2_1094$lat <- as.numeric (test2_1094$lat)

ggplot(data.frame(lat=0,lon=0), aes(lon,lat)) + 
  geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
               data=test2_1094,
               size = 0.3,
               fill = "darkgray") 
# lon is flipped? but just seems like how it's plotting. have to coerce to numeric


# 1146

# plot next to 1094 and 1092
test_good_1094 <- rbind (test2_1094, filter (test, SUBDIVISION == 1146), 
                         filter (test, SUBDIVISION == 1092))

ggplot(data.frame(lat=0,lon=0), aes(lon,lat)) + 
  geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
               data=filter (test_good_1094),
               size = 0.3)  +
  geom_point(aes(lon,lat), data = filter (test_good_1094, SUBDIVISION == 1146,
                                                               order ==478), col = "black") +
  scale_x_continuous(breaks = seq(min(test_good_1094$lon, na.rm = TRUE), max(test_good_1094$lon, na.rm = TRUE), by = 1)) +
  scale_y_continuous(breaks = seq(min(test_good_1094$lat, na.rm = TRUE), 
                                  max(test_good_1094$lat, na.rm = TRUE), 
                                  by = 0.5)
  )


ggplot(data.frame(lat=0,lon=0), aes(lon,lat)) + 
  geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
               data=filter (test_good_1094, SUBDIVISION %in% c(1094, 1092)),
               size = 0.3) +
  scale_x_continuous(breaks = seq(min(test_good_1094$lon, na.rm = TRUE), 
                                  max(test_good_1094$lon, na.rm = TRUE), 
                                  by = 1)
  ) 


ggplot(data.frame(lat=0,lon=0), aes(lon,lat)) + 
  geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
               data=filter (test, SUBDIVISION == 1146),
               size = 0.3)+  
  geom_point(aes(lon,lat, col = factor(order)), data = filter (test, SUBDIVISION == 1146,
                                                               order %in% c (1:3, 476:478)))

# what's my inflection point ?
test[which (test$SUBDIVISION == 1146 & test$lat == 60 & between(test$lon, -24.5, -23)),]

ggplot(data.frame(lat=0,lon=0), aes(lon,lat)) + 
  geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
               data=filter (test, SUBDIVISION == 1146),
               size = 0.3)+  
  geom_point(aes(lon,lat, col = factor(order)), data = filter (test, SUBDIVISION == 1146,
                                                               order ==398), col = "black")

# corner is point 398, 60, -24. last point is 478, 60.75 and -26.5. 

# so I want to start with 1 at 60.75, -27. then will just go down by 1 step each until we get to 59.5, 29.5. then follow 1094 until 58 and 31.5. then across to the right until under 398. 58, -24
repair_1146 <- data.frame (
  SUBDIVISION = 1146,
  lat = c(60.75, 60.5, 60.5, 60.25, 60.25, 60, 60, 59.75, 59.75, 59.5, 59.5,59, 59, 58.5, 58.5, 58.25, 58.25, 58, 58),
  lon = -1*c(27, 27, 27.5, 27.5, 28, 28, 28.5, 28.5, 29, 29, 29.5,29.5, 30.5, 30.5, 31, 31, 31.5, 31.5, 24),
  order = c(1:19)
)

good_1146 <- test %>%
  filter (SUBDIVISION == 1146,
          order >= 398)

test2_1146 <- rbind (repair_1146, good_1146, c(1146, repair_1146$lat[1], repair_1146$lon[1], "NA"))
test2_1146$lon <- as.numeric (test2_1146$lon)
test2_1146$lat <- as.numeric (test2_1146$lat)

ggplot(data.frame(lat=0,lon=0), aes(lon,lat)) + 
  geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
               data=test2_1146,
               size = 0.3,
               fill = "darkgray") 


test3 <- rbind (test2_1094, test2_1146, 
                         filter (test, SUBDIVISION == 1092))

ggplot(data.frame(lat=0,lon=0), aes(lon,lat)) + 
  geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
               data=test3,
               size = 0.3) 

test4 <- rbind (borm_coords1, test2_1094, test2_1146)

ggplot(data.frame(lat=0,lon=0), aes(lon,lat)) + 
  geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
               data=test4,
               size = 0.3) 
