# Check prediction rasters
# 8/10/2020
# JGM

library (raster)
library (sf)
library (mapdata)

# iceland EEZ shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=5680
eez <- st_read("Data/eez.shp")

# vector of dates
projected_dates <- seq (ymd("2015-01-01"), ymd("2100-12-12"), by = "months")

# load prediction bricks
# eventually these will be in Models/prediction_bricks

# for now, have temporary saved file in Data
tmp_brick <- brick ("Models/Prediction_bricks/Gadus_morhua_245_temp.grd")

plot (tmp_brick, 1)
# make color scale based on quantiles
col_breaks <- quantile (getValues(tmp_brick), probs = seq (0, 1, 0.1), na.rm = TRUE)
plot (tmp_brick, 1000, breaks = col_breaks, col = terrain.colors(11))

## take decade averages
# https://gis.stackexchange.com/questions/222443/find-mean-of-a-group-of-every-10-layers-in-raster-stack-in-r

# not evenly split into decades. would want 2015-2020, then 2021-2030 and so on. need to repeat first index 72 times, then 120. 
dec_index <- c (rep (1, 72), rep (2:9, each = 120))

dec_names <- c(
  "2015-2020",
  "2021-2030",
  "2031-2040",
  "2041-2050",
  "2051-2060",
  "2061-2070",
  "2071-2080",
  "2081-2090",
  "2091-2100"
)

dec_mean <- stackApply (tmp_brick, dec_index, fun = mean)
names (dec_mean) <- dec_names
dec_breaks <- quantile (getValues(dec_mean), probs = seq (0, 1, 0.1), na.rm = TRUE)
plot (dec_mean, breaks = dec_breaks, col = terrain.colors(11))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

# clip decades to island eez
dec_clip <- mask (dec_mean, eez)
dec_breaks_isl <- quantile (getValues(dec_clip), probs = c(seq (0, 0.9, 0.1), 0.95, 0.99, 0.999, 1), na.rm = TRUE)
plot (dec_clip, breaks = dec_breaks_isl, col = terrain.colors(14),
      xlim = c(-33, 0), ylim = c (58, 72))


vals1 <- getValues (tmp_brick[[1]])
summary (vals1) # max e38
# plot with color range excluding outliers
quantile (vals1, 0.75, na.rm = TRUE) + 1.5 * (quantile (vals1, 0.75, na.rm = TRUE) - quantile (vals1, 0.25, na.rm = TRUE))

allvals <- as.vector(getValues(tmp_brick))
summary (allvals)

# should my range exclude zeros?
max_scale <- quantile (allvals, 0.75, na.rm = TRUE) + 1.5 * (quantile (allvals, 0.75, na.rm = TRUE) - quantile (allvals, 0.25, na.rm = TRUE)) # 34

allvals_P <- allvals[which(allvals > 0)]
summary (allvals_P) # exactly the same?? more values have been exlcuded than just NA...

library (viridis)
library (maps)
library (mapdata)

plot (tmp_brick, 1, col = viridis(64), zlim = c (0, 35))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (tmp_brick, 100, col = viridis(64), zlim = c (0, 35))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (tmp_brick, 1000, col = viridis(64), zlim = c (0, 35))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (tmp_brick, 1000, col = viridis(64), zlim = c (0, 350))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

pred_clip <- mask (tmp_brick, eez)

plot (tmp_brick, 1000)
plot (log(tmp_brick), 1000)

vals <- getValues (pred_clip[[1000]])

# also quick checks with first layer for different cod models run through build_prediciton_rasters

# for tensor, have weird thing where exp() values > 710 are Inf, so messes up color bar. weird behavior in extreme north for biomass giving huge values. 
pred_stack_245[pred_stack_245 > 10000] <- NA

pred_clip <- mask (pred_stack_245, eez)
isl_breaks <- quantile (getValues(pred_clip), probs = seq (0, 1, 0.1), na.rm = TRUE)
plot (pred_clip, 1000, 
      breaks = isl_breaks, col = terrain.colors(11),
      xlim = c (-32, 0), ylim = c (60, 70))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

# plot full map
col_breaks <- quantile (getValues(pred_stack_245), probs = seq (0, 1, 0.1), na.rm = TRUE)
plot (pred_stack_245, breaks = col_breaks, col = terrain.colors(11))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

# zoom to get weird greenland corner
col_breaks <- quantile (getValues(pred_stack_245), probs = seq (0, 1, 0.1), na.rm = TRUE)
plot (pred_stack_245, breaks = col_breaks, col = terrain.colors(11),
      xlim = c(-33, 0), ylim = c (58, 72))
map('worldHires',add=TRUE, col='grey90', fill=TRUE)
