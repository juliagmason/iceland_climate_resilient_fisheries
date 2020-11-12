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
        movie.name = "OISST_annual_mean_animation.gif"
        )

)

saveGIF(animate (oisst, pause = 0.05, main = format(dates, "%Y-%m")), 
        movie.name = "Figures/OISST_monthly_animation.gif")
