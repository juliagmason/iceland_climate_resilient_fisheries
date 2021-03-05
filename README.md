# README

*Preprint* Julia G. Mason, Pamela J. Woods, Magnús Thorlacius, Kristinn Guðnason, Vincent S. Saba, Patrick J. Sullivan, Kristin M. Kleisner (2021), Projecting climate-driven shifts in demersal fish habitat in Iceland’s waters.

*Contact* Julia Mason: jmason@edf.org

## Directories
*Data setup* Scripts for cleaning and compiling input data used for fitting GAMs. Note: bottom trawl survey data are not shared here per MOU between EDF and MFRI.
*Models* GAM objects and summary tables of GAM diagnostics. 

## Workflow
Step1_Fit_GAMs_Function.R: Fits presence-absence and log-biomass Generalized Additive Models for species of interest. Includes code for model validation and evaluation, including training/testing, variable importance calculation, AUC, TSS, and MASE

Step2_Predict_ensemble_rasters.R: Uses GAMs from Step1 to make suitable thermal habitat projections with climate models on a 0.25 x 0.25 degree grid in Iceland's Exclusive Economic Zone. Also backcasts suitable thermal habitat on this grid for the historical 2000-2018 period. 

Step3_Calculate_habitat_change.R: 
