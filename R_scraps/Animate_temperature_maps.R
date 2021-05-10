# Animate temperature maps
# 11/9/2020
# JGM

library (raster)
library (animation)
library (tidyverse)
library (RColorBrewer)
library (gifski)

library (sf)
library (rnaturalearth)
library (rnaturalearthdata) # for country outlines

library (marmap) # for depth contour lines

# https://www.rdocumentation.org/packages/raster/versions/3.3-13/topics/animate

# oisst <- brick ("Data/OISST_MFRI.grd")
# # this is the survey data, 1985-2020
# 
# names (oisst) <- seq (as.Date ("1985-01-01"), as.Date ("2020-06-01"), by = "month")
# 
# dates <- seq (as.Date ("1985-01-01"), as.Date ("2020-06-01"), by = "month")
# #format(dates, "%B %Y")
# 
# animate (oisst, pause = 0.05, main = format(dates, "%Y-%m"))
# 
# # try just one year
# oisst_20 <- oisst[[415:426]]
# dates_20 <- dates[415:426]
# 
# animate (oisst_20, pause = 0.05, main = format(dates_20, "%Y-%m"))

# oisst is just iceland EEZ. do I have larger set for glorys?
gl_sst <- brick ("Data/glorys_sst_16_sample.grd")
dates <- seq (as.Date ("1993-01-01"), as.Date ("2018-12-01"), by = "month")

# test plot
pal <- rev(brewer.pal(11, "RdBu"))
plot (gl_sst[[1]], col = pal)


# country outlines
world <- ne_countries(scale = "medium", returnclass = "sf")  

# depth contour lines
depth <- getNOAA.bathy(lon1 = -45, lon2 = 15,
                       lat1 = 50, lat2 = 85, 
                       resolution = 4)

# test plot
plot (depth, deep = -6000, shallow = 0, step= 1000, label = TRUE)

# convert to df for ggplot
# https://stackoverflow.com/questions/50119592/plot-bathymetry-and-coastline-using-ggplot2-and-marmap
depth_m <- as.matrix (depth)
class (depth_m) <- "matrix"
depth_df <- depth_m %>%
        
        as.data.frame() %>%
        rownames_to_column(var = "lon") %>%
        gather(lat, value, -1) %>%
        mutate_all(list(as.numeric))


# https://github.com/raymondben/rrasteranim
sst_tmp <- gl_sst[[1]]
sst_xy <- as_tibble (coordinates (sst_tmp))

ggplot () +
        geom_raster (data = sst_xy %>%
                             mutate (SST = values (sst_tmp)), aes (x, y, fill = SST)) +
        scale_fill_distiller ( palette = "RdBu", direction = -1) +
        # plot depth contours, 1000m
        geom_contour(aes(x = lon, y = lat, z = value), binwidth = 1000, colour = "black", data = depth_df) +
        geom_sf (data = world, fill = "grey90") +
        coord_sf(xlim = c(-45, 15), ylim = c (50, 85), expand = FALSE )+
        theme_void ()


lims <- c(-45, 15, 50, 85)

# standardize colorbar
summary (as.vector(getValues(gl_sst[[277:312]])))
summary (as.vector(getValues(gl_sst)))

compose_frame <- function(idx, lims = NULL) {
        
        this_sst <- sst_xy %>%  mutate (SST = values (subset (gl_sst, idx)))

        p <- ggplot () +
                geom_raster (data = this_sst, aes (x, y, fill = SST)) +
                scale_fill_distiller ( palette = "RdBu", direction = -1,
                                       limits = c (-3, 17)) +
                # plot depth contours, 1000m
                geom_contour(aes(x = lon, y = lat, z = value), binwidth = 1000, colour = "black", data = depth_df) +
                geom_sf (data = world, fill = "grey90") +
                coord_sf(xlim = c(-45, 15), ylim = c (50, 85), expand = FALSE ) +
                theme_void () +
                annotate (geom = "text", label = format(dates[idx], "%b %Y"), x = -33, y = 78, size = 8) +
                theme (legend.position = "none")
        ## if x- and y- limits have been specified, apply them
        ##  this allows us to keep the plot axis limits consistent over frames
        if (!is.null(lims)) p <- p + xlim(lims[1:2]) + ylim(lims[3:4])
        p
}

## generate a list of ggplot objects
my_lims <- NULL
out_5 <- lapply(253:312, compose_frame)


png("frame%03d.png")
for (i in seq_len(length(out_5))) print(out_5[[i]])
dev.off()
png_files_5 <- sprintf("frame%03d.png", seq_len(length(out_5)))
gif_file_5 <- gifski(png_files_5, delay = 0.5)
#save_gif(makeplot(), gif_file, 1280, 720, res = 144)
unlink(png_files_5)
utils::browseURL(gif_file_5)





# annual mean
# https://stackoverflow.com/questions/31798389/sum-nlayers-of-a-rasterstack-in-r
indices <- as.numeric (format (dates, "%Y"))

oisst_annual_mn <- stackApply(oisst, indices, fun = mean)

system.time(
saveGIF(animate (oisst_annual_mn, pause = 0.25, main = 1985:2020, maxpixels = 20000),
        movie.name = "OISST_annual_mean_animation.gif",
        interval = 0.25
        )

)

# this didn't work,even over several days
system.time(
saveGIF(animate (oisst, pause = 0.08, main = format(dates, "%Y-%m"),
                 maxpixels = 20000), 
        movie.name = "OISST_monthly_animation.gif", 
        interval = 0.08)
)

# plot model projections ----

plot_cm_gif <- function (model, scenario, var) {
        # model is 4-digit climate model name
        # scenario is 245 or 585
        # var is sst or bt
        
        filename <- paste (model, scenario, var, "projection.grd", sep = "_")
        
        # open rasterbrick
        cm_brick <- brick (file.path("Data", "CMIP6_delta_projections", filename))
        
        # sequence of dates that match raster layers
        if (model == "CM26") {
               start_year <- 2001
               end_year <- 2080
               
               # want to start at 2005 for even 5 year increments. st
               start_skip <- 4

                 
        } else {
                
                start_year <- 2015
                end_year <- 2100
                
                start_skip <- 0

                }
 
              
        # sequence of years to take annual average
        avg_indices <- rep (start_year:end_year, each = 12)
        
        # take annual mean
        brick_mn <- stackApply (cm_brick, avg_indices, fun = mean)
        
        # make smaller?? every 5 years??
        brick_mn_5 <- subset (brick_mn, seq(start_skip + 1, 
                                            end_year - start_year + 1, 
                                            by = 5))
        
        # Another way of looking at this--take march of each year. March of every five years
        march_index <-  seq ((start_skip * 12 + 3), (end_year - start_year + 1) * 12, by = 12*5)
        
        brick_march <- subset (cm_brick, march_index)
        
        # save GIFs
        
        mov_name_mn <- paste ("Animation", model, scenario, var, "5ann_mean.gif", sep = "_")
        
        mov_name_march <- paste ("Animation", model, scenario, var, "March.gif", sep = "_")
        

        plot_title <- paste (toupper(model),
                                     scenario, 
                                     toupper(var), 
                                     seq((start_year + start_skip),
                                         end_year, 
                                         by = 5), 
                             sep = " ")

        
        #  save GIFs
   
        saveGIF(animate (brick_mn_5, 
                         pause = 0.25,
                         main = plot_title,
                         maxpixels = 10000,
                         zlim = c(-3.5, 20), 
                         xlim = c(-45, 15),
                         ylim = c(50, 85)),
                movie.name = mov_name_mn,
                interval = 0.25
        )
        
        saveGIF(animate (brick_march, 
                         pause = 0.25,
                         main = paste0(plot_title, " March"),
                         maxpixels = 10000,
                         zlim = c(-3.5, 20),
                         xlim = c(-45, 15),
                         ylim = c(50, 85)),
                
                movie.name = mov_name_march,
                interval = 0.25
        )
        
        print (c(model, scenario, var))
                
} # end function

plot_cm_gif (model = "gfdl",
             var = "bt", 
             scenario = 245)

library (tidyverse)

CM_list <- c("gfdl", "cnrm", "ipsl", "mohc", "CM26")

cm_expand <- expand_grid (climate_model = CM_list,
                          scenario = c(245, 585), 
                          var = c("sst", "bt")) %>%
        filter (!(climate_model == "CM26" & scenario == 245))

cm_expand_list <- list(
        model = cm_expand$climate_model,
        var = cm_expand$var,
        scenario = cm_expand$scenario)

pmap (cm_expand_list, plot_cm_gif)

vars <- c("sst", "bt")
lapply (vars, plot_cm_gif, model = "CM26", scenario = 585)

