# Animate temperature maps
# 11/9/2020
# JGM

library (raster)
library (animation)

# https://www.rdocumentation.org/packages/raster/versions/3.3-13/topics/animate

oisst <- brick ("Data/OISST_MFRI.grd")
# this is the survey data, 1985-2020

names (oisst) <- seq (as.Date ("1985-01-01"), as.Date ("2020-06-01"), by = "month")

dates <- seq (as.Date ("1985-01-01"), as.Date ("2020-06-01"), by = "month")
format(dates, "%B %Y")

animate (oisst, pause = 0.05, main = format(dates, "%Y-%m"))


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

