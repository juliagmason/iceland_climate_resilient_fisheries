# Seasonal taylor diagram for CMIP6 25km and 100km models 
# 5/14/2020

library (R.matlab) # for reading matlab files
library (openair) # for taylor diagrams
library (tidyverse) # for case_when

# problem--all different dimensions. so that might be why CM 2.6 looks so off from everything else. 
# CM 2.6: 1000 x 1018 x 4 CM_season_ESRL

plot_TD_fun <- function (model_list, obs_var, figure_name) {
  # model is vector of 4 letter abbreviations, all caps
  # obs_var is either surface or bottom temperature--OISST or GINS
  # figure_name is how I want to save the figure
  
  # empty data frame to store 
  TD_vals <- data.frame ()
  
  if (obs_var == "OISST") {
    # load OISST. readMat brings in a list, the first element of which is the array I want
    # double check if ESRL_season_GFDL is the right one, but has the right dimensions
    OISST_mat <- # read nc file in python scripts instead???
      #readMat("../Documents/Python Scripts/oisst_month_climatology_1982_2011.nc.mat") not supporteded
    #readMat ("../Documents/MATLAB/CMIP6/ESRL_season_GFDL.mat")
    OISST <- OISST_mat[[1]]
    
    # crop to square around Iceland
    OISST_ISL_crop <- array(OISST[397:801,521:720,])
    
  } else {
    # this one is wrong, find seasonal gins shaped to CMIP6
    GINS_mat <- readMat ("../Documents/MATLAB/CMIP6/gins_bt_clim_reshape.mat") # 140 x 240 x 12 should be right... reshaped to OISST. 
    GINS <- GINS_mat[[1]]
    GINS_ISL_crop <- array (GINS[540:801, 601:661,])
  }
  
  
  
  for (model in model_list) {
    
    
    # I have all the cmip6 files stored in one folder, but CM 2.6 and 2.1 in separate folders
    mat_file <- case_when (
      model == "CM2_6" ~ "../Documents/MATLAB/CM2_6/CM26_hist_reshape.mat",
      model == "CM2_1" ~ "../Documents/MATLAB/CM2_1/CM21m_season_ESRL.mat",
      TRUE ~ paste0 ("../Documents/MATLAB/CMIP6/", model, "_hist_sst_reshape.mat")
                     )
    
    mat_list <- readMat (mat_file)
    
    model_vals <- mat_list[[1]] # should be 1440 x 720 x 4 for CMIP6. CM2.6 is 140x240x12, north atl crop. 
    
    dim(model_vals)
    
    model_ISL_crop <- array(model_vals) #[397:801,521:720,])
    
    # make dataframe
    TD_model <- data.frame (
      # observation --OISST values
      obs =  if (obs_var == "OISST") {
        OISST_ISL_crop
      } else {GINS_ISL_crop},
      # modeled values
      mod = model_ISL_crop,
      # model name
      group = rep (model, length (model_ISL_crop)),
      # season 
      type = rep (c("Winter", "Spring", "Summer", "Fall"), each = length (model_ISL_crop)/4)
    )
    
    # append to data frame
    TD_vals <- rbind (TD_vals, TD_model)
    
  } # end model loop
  
  # model and season names are characters; switch to factors
  TD_vals$group <- as.factor (TD_vals$group)
  TD_vals$type <- factor (TD_vals$type, levels = c ("Winter", "Spring", "Summer", "Fall"))

  
  # plot and save
  save_path <- file.path ("Figures", figure_name)
  
  png (save_path, width = 16, height = 9, units = "in", res = 300)  
  #print(
  TaylorDiagram (TD_vals,
                 obs="obs",
                 mod="mod",
                 group="group",
                 type = "type",
                 normalise=TRUE,
                 main = paste ("Taylor Diagram, CMs vs.", 
                               obs_var, "\n Iceland crop, 1982-2012")
  )
  #)
  dev.off()
} # end function 

cm_ensemble <- c ("CM2_6", "CNRM", "GFDL", "MOHC", "IPSL")

plot_TD_fun (model_list = cm_ensemble,
             obs_var = "OISST", 
             figure_name = "TD_CM_Ensemble_OISST.png")

plot_TD_fun (model_list = cm_ensemble,
             obs_var = "OISST", 
             figure_name = "TD_CM_Ensemble_OISST_Natl_check.png")
