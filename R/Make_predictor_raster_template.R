# Make predictor raster template
# 7/28/2020
# JGM

# Bring in one of the completed SST projection grids and make an empty raster template with its same extent and grid

library (raster)

gfdl_245_bt <- brick ("Data/CMIP6_delta_projections/gfdl_245_bt_projection.grd")

# make template from sst data. code from PD sharks resample_env_data

pred_r_template <- raster()
ncol (pred_r_template) <- ncol (gfdl_245_sst) # 240
nrow (pred_r_template) <- nrow (gfdl_245_sst) # 140
xmin (pred_r_template) <- xmin (gfdl_245_sst) # -44.875
xmax (pred_r_template) <- xmax (gfdl_245_sst) # 14.875
ymin (pred_r_template) <- ymin (gfdl_245_sst) # 50.125
ymax (pred_r_template) <- ymax (gfdl_245_sst) # 84.875
res (pred_r_template) <- res (gfdl_245_sst) # 0.2489583 0.2482143 ....?
projection (pred_r_template) <- "+proj=longlat + datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

save (pred_r_template, file = "Data/prediction_raster_template.RData")