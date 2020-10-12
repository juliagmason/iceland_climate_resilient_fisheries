# Turn MFRI Bormicon dataframe into polygons and raster

# https://stackoverflow.com/questions/48383990/convert-sequence-of-longitude-and-latitude-to-polygon-via-sf-in-r

library (sf)
library (tidyverse)
library (raster)

# overall table linking division, subdivision, and survey points
borm_table <- read_csv("Data/Raw_data/bormicon_table.csv") %>%
  dplyr::select (division, subdivision) %>% 
  unique()

# missing 1141, 1151 subdivisions

# table with points defining the polygons of the subdivisions
borm_coords <- read_csv ("Data/subdivision_coords.csv") %>%
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


# rasterize with template
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

# tried dissolving or joining to get divisions instead of subdivisions. Instead, I'll just reclassify once it's a raster. 
# https://stackoverflow.com/questions/39206395/replace-raster-classes-by-values-from-data-frame-in-r

# have to switch order of subdivision and division
borm_matrix <- data.matrix (cbind(borm_table$subdivision, borm_table$division))

borm_div_rcl <- raster::reclassify (subdiv_r, borm_matrix)
plot (borm_div_rcl)

# save raster
borm_div_r <- writeRaster(borm_div_rcl, filename = "Data/bormicon_divisions.grd")



## plotting code from Pamela----

test <- read.csv('Data/subdivision_coords.csv')

ggplot(data.frame(lat=0,lon=0), aes(lon,lat)) + 
  geom_polygon(aes(lon,lat,group=SUBDIVISION, fill = as.factor(SUBDIVISION)),
               data=test,
               size = 0.3) +  
  geom_point(col='yellow') +  
  geom_path(aes(lon,lat,group=SUBDIVISION), 
            data=test,
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
        axis.title = element_blank(),
        legend.position = 'none')
