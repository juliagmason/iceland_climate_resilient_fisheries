# Seasonal taylor diagram for CMIP6 25km and 100km models , bottom temp GINS
# 5/21/2020

library (R.matlab)
library (openair)

# load Matlab .mat files 
GINS_mat <- readMat ("../Documents/MATLAB/GINS/gins_bt.mat")
AWI_mat <- readMat ("../Documents/MATLAB/CMIP6/awi_season_gins.mat") 
GFDL_mat <- readMat ("../Documents/MATLAB/CMIP6/gfdl_season_gins.mat")
CNRM_25_mat <- readMat ("../Documents/MATLAB/CMIP6/cnrm_25_season_gins.mat")

# 100km
IPSL_mat <- readMat ("../Documents/MATLAB/CMIP6/ipsl_season_gins.mat")
MOHC_mat <- readMat ("../Documents/MATLAB/CMIP6/mohc_season_gins.mat")

# extract data array from list
GINS <- array (unlist (GINS_mat$gins.bt), dim = c (241, 141, 4)) # this one is still in list form
AWI <- AWI_mat$awi.bt.seas
GFDL <- GFDL_mat$gfdl.bt.seas
CNRM <- CNRM_25_mat$cnrm.bt.seas

IPSL <- IPSL_mat$ipsl.bt.seas
MOHC <- MOHC_mat$mohc.bt.seas

# run TDs, use whole range for GINS?
# Iceland crop
# North atlantic 
mod_ISL = c (array (AWI), # array makes them into a column
             array (GFDL),
             array (CNRM),
             array (IPSL),
             array (MOHC)
             )

obs_ISL = rep (array (GINS), 5)

group_ISL = c(rep ("AWI", length (array (AWI))),
              rep ("GFDL", length (array (AWI))),
              rep ("CNRM", length (array (AWI))),
              rep ("IPSL", length (array (AWI))),
              rep ("MOHC", length (array (AWI)))
              )

type_ISL = rep (c(rep ("Winter", length (array (AWI[,,1]))), 
                  rep ("Spring", length (array (AWI[,,1]))),
                  rep ("Summer", length (array (AWI[,,1]))), 
                  rep ("Fall", length (array (AWI[,,1])))
),
5)

data_mat_ISL = data.frame (obs = obs_ISL,
                           mod = mod_ISL,
                           group = group_ISL,
                           type = type_ISL)

data_mat_ISL$group <- as.factor (data_mat_ISL$group)
data_mat_ISL$type <- factor (data_mat_ISL$type, levels = c ("Winter", "Spring", "Summer", "Fall"))

TD_out_ISL <- TaylorDiagram (data_mat_ISL,
                             obs="obs",
                             mod="mod",
                             group="group",
                             type = "type",
                             normalise=TRUE,
                             main = "Taylor Diagram, CMIP6 vs. GINS \n 1985-2012"
)

