# Bormicon check, is mean temp at depth changing over time?

library (raster)
library (sf)
library (sp)
library (lubridate)

eez <- st_read("Data/eez.shp")

gl_bt <- brick ("Data/glorys_bt_16_sample.grd")
bt_clip <- mask (gl_bt, eez)

# vector of dates
glorys_dates <- seq (ymd("1993-01-16"), ymd("2018-12-16"), by = "months")

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



# extract values in each bormicon subdivision polygon
v_br <- raster::extract (bt_clip, borm_spdf) # list of 38 subdivisions, with a 1:x, 1:228 "matrix" "array"

# Need to merge to larger divisions. Don't know how to do within a list, so convert to data frame
v_br_mn <- v_br %>%
  # this takes the sum for each time step within each subdivision. returns a list of 38, each with a vector of 1:228
  # actually I want a mean
  map (colMeans, na.rm = TRUE) %>%
  # convert this to a data frame with 38 rows (subdivisions) and 312 columns (time step values)
  unlist () %>%
  matrix (nrow = 312) %>% 
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
                values_to = "value")

# manually fix month column?
v_br_mn$date <- rep (glorys_dates, 38)

library (lubridate)
v_br_annual <- v_br_mn %>%
  group_by (division, year(date)) %>%
  summarize (mean_bt = mean (value, na.rm = TRUE)) %>%
  rename (year = `year(date)`)


png ("Figures/Bormicon_past_bottom_temp.png")
v_br_annual %>%
  ggplot (aes (x = year, y = mean_bt, col = factor(division))) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_continuous (breaks = seq (1995, 2015, by = 5)) +
  ggtitle ("Mean bottom temperature in each Bormicon region, 1993-2018")
dev.off()