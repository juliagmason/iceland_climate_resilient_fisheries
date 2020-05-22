# Seasonal taylor diagram for CMIP6 25km and 100km models 
# 5/14/2020

library (R.matlab)

# load Matlab .mat files 
ESRL_mat <- readMat ("../Documents/MATLAB/CMIP6/ESRL_circshift.mat")
AWI_mat <- readMat ("../Documents/MATLAB/CMIP6/AWI_season_ESRL.mat") 
GFDL_mat <- readMat ("../Documents/MATLAB/CMIP6/GFDL_season_ESRL.mat")
CNRM_25_mat <- readMat ("../Documents/MATLAB/CMIP6/CNRM_25_season_ESRL.mat")

# 100km
IMN4_mat <- readMat ("../MATLAB/CMIP6/IMN4_season_ESRL.mat")
IMN5_mat <- readMat ("../MATLAB/CMIP6/IMN5_season_ESRL.mat")
IPSL_mat <- readMat ("../MATLAB/CMIP6/IPSL_season_ESRL.mat")
CNRM_mat <- readMat ("../MATLAB/CMIP6/CNRM_season_ESRL.mat")
MOHC_mat <- readMat ("../MATLAB/CMIP6/MOHC_100_season_ESRL.mat")

# extract data array from list
ESRL <- ESRL_mat$ESRL.circshift
AWI <- AWI_mat$AWI.season
GFDL <- GFDL_mat$GFDL.season
CNRM_25 <- CNRM_25_mat$CNRM.season

IMN4 <- IMN4_mat$IMN4.season
IMN5 <- IMN5_mat$IMN5.season
IPSL <- IPSL_mat$IPSL.season
CNRM <- CNRM_mat$CNRM.season
MOHC <- MOHC_mat$MOHC.100.season

# something weird with IMN4, a ton of zeros, but I think they're all in a zone over russia?

# I'm going to run two versions, north atlantic and closer crop for iceland waters. North Atlantic is [397:801,521:720], Iceland is [601:661, 536:801]. For each of these, I want four panels, one for each season. 

## Run taylor diagram----
library (openair)

# can use "type" to divide by season. so I need to figure out what order things get placed with the array function. 


# North atlantic 
mod_NAtl = c (array (AWI[397:801,521:720,]), # array makes them into a column
           array (GFDL[397:801,521:720,]),
           array (IPSL[397:801,521:720,]),
           array (IMN4[397:801,521:720,]),
           array (IMN5[397:801,521:720,]),
           array (CNRM[397:801,521:720,]),
           array (MOHC[397:801,521:720,])
  )
  
obs_NAtl = rep (array (ESRL[397:801,521:720,]), 7)
  
group_NAtl = c(rep ("AWI", 81000*4),
            rep ("GFDL", 81000*4),
            rep ("IPSL", 81000*4),
            rep ("IMN4.8", 81000*4),
            rep ("IMN5.0", 81000*4),
            rep ("CNRM", 81000*4),
            rep ("MOHC", 81000*4)
  )

type_NAtl = rep (c(rep ("Winter", 81000), 
                   rep ("Spring", 81000),
                   rep ("Summer", 81000), 
                   rep ("Fall", 81000)
                   ),
                 7)
  
data_mat_NAtl = data.frame (obs = obs_NAtl,
                         mod = mod_NAtl,
                         group = group_NAtl,
                         type = type_NAtl)
  
data_mat_NAtl$group <- as.factor (data_mat_NAtl$group)
data_mat_NAtl$type <- factor (data_mat_NAtl$type, levels = c ("Winter", "Spring", "Summer", "Fall"))
levels (data_mat_NAtl$group) <-  c ("Winter", "Spring", "Summer", "Fall")
  
TD_out_NAtl <- TaylorDiagram (data_mat_NAtl,
                       obs="obs",
                       mod="mod",
                       group="group",
                       type = "type",
                       normalise=TRUE,
                       main = "Taylor Diagram, CMIP6 25km and 100km vs. OISST \n North Atlantic, 1982-2012"
)
  

# Iceland crop
# North atlantic 
mod_ISL = c (array (AWI[540:801, 601:661,]), # array makes them into a column
              array (GFDL[540:801, 601:661,]),
             array (CNRM_25[540:801, 601:661,])
)
#               array (IPSL[540:801, 601:661,]),
#               array (IMN4[540:801, 601:661,]),
#               array (IMN5[540:801, 601:661,]),
#               array (CNRM[540:801, 601:661,]),
#               array (MOHC[540:801, 601:661,])
# )

obs_ISL = rep (array (ESRL[540:801, 601:661,]), 3)

group_ISL = c(rep ("AWI", length (array (AWI[540:801, 601:661,]))),
               rep ("GFDL", length (array (AWI[540:801, 601:661,]))),
              rep ("CNRM", length (array (AWI[540:801, 601:661,])))
)
              
#               
#                rep ("IPSL", length (array (AWI[540:801, 601:661,]))),
#                rep ("IMN4.8", length (array (AWI[540:801, 601:661,]))),
#                rep ("IMN5.0", length (array (AWI[540:801, 601:661,]))),
#                rep ("CNRM", length (array (AWI[540:801, 601:661,]))),
#                rep ("MOHC", length (array (AWI[540:801, 601:661,])))
# )

type_ISL = rep (c(rep ("Winter", length (array (AWI[540:801, 601:661,1]))), 
                   rep ("Spring", length (array (AWI[540:801, 601:661,1]))),
                   rep ("Summer", length (array (AWI[540:801, 601:661,1]))), 
                   rep ("Fall", length (array (AWI[540:801, 601:661,1])))
),
3)

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
                              main = "Taylor Diagram, CMIP6 AWI, CNRM, GFDL vs OISST \n Iceland crop, 1982-2012"
)

