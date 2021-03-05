## convert to matlab bc i give up
# 5/18/2020
# JGM

library (R.matlab)

load ("Data/GINS_bottom_temp_seasonal.RData")
writeMat("../Documents/MATLAB/GINS/gins_bt.mat", gins_bt = gins_bt)

load ("Data/cnrm_lat.RData")
load ("Data/cnrm_lon.RData")
load ("Data/cnrm_sst.RData")
load ("Data/cnrm_bt.RData")
writeMat ("../Documents/MATLAB/CMIP6/cnrm_25_sst.mat", cnrm_sst = cnrm_sst_mn)
writeMat ("../Documents/MATLAB/CMIP6/cnrm_25_bt.mat", cnrm_bt = cnrm_bt_mn)
writeMat ("../Documents/MATLAB/CMIP6/cnrm_25_lat.mat", lat_cnrm = lat_cnrm)
writeMat ("../Documents/MATLAB/CMIP6/cnrm_25_lon.mat", lon_cnrm = lon_cnrm)

load ("Data/gfdl_bt.RData")
load ("Data/gfdl_sst.RData")
writeMat("../Documents/MATLAB/CMIP6/gfdl_bt.mat", gfdl_bt = gfdl_bt_mn)
writeMat("../Documents/MATLAB/CMIP6/gfdl_sst.mat", gfdl_sst = gfdl_sst_mn)
# also lat and lon
load ("Data/gfdl_lat.RData")
load ("data/gfdl_lon.RData")
writeMat("../Documents/MATLAB/CMIP6/gfdl_lat.mat", lat_gfdl = lat_gfdl)
writeMat("../Documents/MATLAB/CMIP6/gfdl_lon.mat", lon_gfdl = lon_gfdl)

load ("Data/mohc_bt.RData")
writeMat("../Documents/MATLAB/CMIP6/mohc_bt.mat", mohc_bt = mohc_bt_mn)
load ("Data/mohc_lat.RData")
load ("data/mohc_lon.RData")
writeMat("../Documents/MATLAB/CMIP6/mohc_lat.mat", lat_mohc = lat_mohc)
writeMat("../Documents/MATLAB/CMIP6/mohc_lon.mat", lon_mohc = lon_mohc)

load ("Data/ipsl_bt.RData")
load ("Data/ipsl_lat.RData")
load ("data/ipsl_lon.RData")
writeMat("../Documents/MATLAB/CMIP6/ipsl_bt.mat", ipsl_bt = ipsl_bt_mn)
writeMat("../Documents/MATLAB/CMIP6/ipsl_lat.mat", lat_ipsl = lat_ipsl)
writeMat("../Documents/MATLAB/CMIP6/ipsl_lon.mat", lon_ipsl = lon_ipsl)

load ("Data/awi_bt.RData")
load ("Data/awi_sst.RData")
writeMat("../Documents/MATLAB/CMIP6/awi_bt.mat", awi_bt = awi_bt_mn)
writeMat ("../Documents/MATLAB/CMIP6/awi_sst.mat", awi_sst = awi_sst_mn)

load ("Data/awi_lon.RData")
load ("Data/awi_lat.RData")
writeMat ("../Documents/MATLAB/CMIP6/awi_lat.mat", lat_awi = lat_awi)
writeMat ("../Documents/MATLAB/CMIP6/awi_lon.mat", lon_awi = lon_awi)