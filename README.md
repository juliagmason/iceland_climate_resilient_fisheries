# README

**Preprint:** Julia G. Mason, Pamela J. Woods, Magnús Thorlacius, Kristinn Guðnason, Vincent S. Saba, Patrick J. Sullivan, Kristin M. Kleisner (2021), Projecting climate-driven shifts in demersal fish habitat in Iceland’s waters. doi: [https://doi.org/10.1101/2021.03.04.433927](https://doi.org/10.1101/2021.03.04.433927)

**Contact:** Julia Mason: jmason@edf.org

## Directories
**Download climate data:** Scripts in python and MATLAB to download and standardize ocean temperature data from CMIP6, CM 2.6, OISST, GINS, and GLORYS. 

**Data setup:** Scripts for cleaning and compiling predictor data used for fitting GAMs. Includes scripts for rasterizing climate data and calculating species thermal affinity indices. Note: bottom trawl survey data are not shared here per MOU between EDF and MFRI.

**Models:** GAM objects and summary tables of GAM diagnostics.

**Plot figures:** Code for plotting manuscript and supplemental figures

Other directories (Notebooks, spp niche annual trends, scraps) were not used for this manuscript. 

## Workflow
Step1_Fit_GAMs_Function.R: Fits presence-absence and log-biomass Generalized Additive Models for species of interest. Includes code for model validation and evaluation, including training/testing, variable importance calculation, AUC, TSS, and MASE

Step2_Predict_ensemble_rasters.R: Uses GAMs from Step1 to make suitable thermal habitat projections with climate models on a 0.25 x 0.25 degree grid in Iceland's Exclusive Economic Zone. Also backcasts suitable thermal habitat on this grid for the historical 2000-2018 period. 

Step3_Calculate_habitat_change.R: Summarizes values from rasters in Step 2, returns a data frame with historical and projected future mean suitable thermal habitat values for each species, climate model, and scenario. Also contains code for statistical analysis of relationship between thermal affinity indices and projected habitat change. 

Step4_Calculate_centroid_change.R: Summarizes values from rasters in Step2, returns a data frame with the distance and bearing of the shift in centroid of distribution between the historical and projected future period for each species, climate model, and scenario. Also contains code for statistical analysis of relationship between thermal affinity indices and projected habitat centroid change. 
