## Radial compass plots
# 8/21/2020
# JGM

library (geosphere)
library (raster)
library (tidyverse)
# do with smooth_latlon 2000-2018 and 2061-2080

# from Morely

# load smooth_latlon spp
load ("Models/spp_Smooth_latlon.RData")


# empty df
centroids_df <- data.frame ()

for (spp in spp_Smooth_latlon$sci_name_underscore) {
  
  # load bricks
  hist <- brick (paste0("Models/Prediction_bricks/", spp, "_Smooth_latlon_2000_2018.grd"))
  pred_245 <- brick (paste0("Models/Prediction_bricks/", spp, "_Smooth_latlon_245_2061_2080.grd"))
  pred_585 <- brick (paste0("Models/Prediction_bricks/", spp, "_Smooth_latlon_585_2061_2080.grd"))
  
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

summary (centroids_df)
# bearing seems to be -180 to 180, not 0-360

# calculate circular mean?
#https://cran.r-project.org/web/packages/circular/circular.pdf

library (circular)

CA <- centroids_df %>%
  group_by (scenario) %>%
  summarise (mn.c = mean.circular(bearing),
             wmn.c = weighted.mean.circular (bearing, dist),
             mn = mean(bearing) + 360,
             wmn = weighted.mean(bearing, dist) + 360)
mean.circular(centroids_df)

centroids_df %>%
  mutate (bearing = ifelse (bearing < 0, bearing + 360, bearing)) %>%
  filter (dist < 100) %>%
ggplot(aes (x = bearing,
            y = dist), 
            #group = spp, 
            #colour = group), 
       label = species
) +
  coord_polar(start= 0) +
  geom_segment(aes (y = 0,
                    xend = bearing, 
                    yend = dist
  ), 
  size=1.2) +  
  geom_text(aes(label = species, 
                x = bearing,
                y = 200,  
                # not sure what angle is doing here but it seems right
                angle = ifelse (bearing < 180,
                                -bearing + 90,
                                -bearing + 270
                )
  ),
  size=5, 
  vjust=0.1) +
  facet_wrap (~ scenario) +
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
library (SDMTools) # not available for R version 4.0.2

# https://stackoverflow.com/questions/60415059/r-cant-install-sdmtools-for-r-version-3-6-2
# install.packages("remotes")
#remotes::install_version("SDMTools", "1.1-221")

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

# start here ----
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

