# Function to run GAMs on full data for all species
# 9/16/2020

# JGM

library (mgcv)
library (tidyverse)
library (forecast) # for RWF forecast
library (dismo) # for PA evaluations

# load data----
# presence absence data. this would still have 2011 and pre-2000 autumn samples. 
# This is a presence/absence matrix, where each column is a species scientific name, with an ID column for sample ID. every sample is represented, so absences have been filled in with zeros. 
mfri_pa <- read_csv ("Data/PA_MFRI.csv",
                     col_types = cols(
                       sample_id = col_factor()
                     ))

# abundance data
# this is biomass data, where each row is the observed biomass of a species in a sample. absences are not included. Species are indicated by numeric species IDs, not scientific name. 
mfri_abun <- read_csv ("Data/MFRI_comb_survey.csv",
                       col_types = cols(
                         sample_id = col_factor(),
                         species = col_factor(),
                         stat_sq = col_factor()
                       )
)%>%
  filter (!(year == 2011 & season == "autumn"),
          !(year < 2000 & season == "autumn")) %>% # remove autumn 2011 [labor strike] and pre-2000 autumn [sites weren't standard yet]
  group_by (sample_id, species) %>%
  summarize (n_tot = sum(n_per_nautmile),
             kg_tot = sum(kg_per_nautmile)) %>%
  mutate (n_log = log(n_tot),
          kg_log = log(kg_tot))


# predictor variables
# I compiled these in Build_MFRI_predictor_df.R. 
mfri_pred <-  read_csv ("Data/MFRI_predictor_df.csv",
                        col_types = cols(
                          sample_id = col_factor(),
                          stat_sq = col_factor(),
                          bormicon_region = col_factor(),
                          sst_dev = col_number(),
                          bt_dev = col_number(),
                          sst_max = col_number(),
                          sst_min = col_number(),
                          bt_max = col_number(),
                          bt_min = col_number()
                        )
) %>%
  filter (!(year == 2011 & season == "autumn"),
          !(year < 2000 & season == "autumn")
  )  # remove autumn 2011, 131 samples. Also cutting out autumn pre-2000 (as of 7/23/2020. so did this with MASE fitting, but not with original run through of full gams)

# species list
# This relates species IDs to scientific names
spp_list <- read_csv ("Data/species_eng.csv",
                      col_types = cols(
                        Spp_ID = col_factor()
                      ))


# Function to run GAM, full data, all species. 

fit_gam_fun <- function (model_terms, naive_terms, directory, spp_names) {
  # model_terms will be a concatenated list
  # directory will be the model name I've been using
  # spp_names should be scientific names separated by an underscore
  
  # empty data frame for saving variable importance info
  #var_imp_all_spp <- data.frame()

  # empty data frame for saving model summary statistics
  model_stats <- data.frame()
  
  for (i in 1:length(spp_names)) {
    
    # match species sci name to ID code in spp_list
    spp_id <- as.numeric(as.character(spp_list$Spp_ID[which (spp_list$sci_name_underscore == spp_names[i])])) # this fixes weird factor problem
    
    # signal where we are in the process 
    print(paste(i, spp_names[i], spp_id, Sys.time()))  
   
    
    # ==================
    # Data setup ----
    # ==================

    #Presence/absences for selected species, attach to predictor df. Select based on sci_name
   spp_PA <- mfri_pa %>%
     dplyr::select (sample_id, all_of(spp_names[i])) %>%
     left_join (mfri_pred, by = "sample_id")
   colnames (spp_PA)[2] <- "Presence"


   # non-zero biomass data; this will have variable size. Select based on spp_ID code.
   spp_B <- mfri_abun %>%
     filter (species == spp_id) %>%
     left_join (mfri_pred, by = "sample_id")


   # set gamma to penalize for wiggliness (From Morely github code and Simon wood)
   # used in Morely et al. 2018 https://github.com/pinskylab/project_velocity/blob/master/6_model_fitting_loop.R; cites https://people.maths.bris.ac.uk/~sw15190/mgcv/tampere/mgcv-advanced.pdf
   gamma_PA <- log (nrow (spp_PA)) / 2
   gamma_B <- log (nrow (spp_B)) / 2

   
   # ==================
   # Fit full GAM ----
   # ==================
   
   #  set.seed(10) # needed for gamma maybe? used in Morely code.
   # 
   # # Presence-absence model
   # formula_PA <- as.formula (paste0 ("Presence ~", (paste (model_terms, collapse = " + "))))
   # gam_PA <- gam (formula_PA,
   #                family = "binomial",
   #                data = spp_PA,
   #                gamma = gamma_PA)
   # 
   # 
   # # log biomass model:
   #  formula_LB <- as.formula (paste0 ("kg_log ~", (paste (model_terms, collapse = " + "))))
   #  gam_LB <- gam (formula_LB,
   #                 family = "gaussian",
   #                 data = spp_B,
   #                 gamma = gamma_B)
   # 
   # 
   # 
   #  # save models in appropriate folder
   #  save (gam_PA, file = paste0("Models/", directory, "/", spp_names[i], "_PA.Rdata"))
   #  save (gam_LB, file = paste0("Models/", directory, "/", spp_names[i], "_LB.Rdata"))
   # #  
   # 
   #  already fit models, just load
    load (file.path("Models", directory, paste0(spp_names[i], "_PA.Rdata"))) # drop _full for depth_temp and bormicon
    load (file.path("Models", directory, paste0(spp_names[i], "_LB.Rdata")))

    formula_PA <- as.formula (paste0 ("Presence ~", (paste (model_terms, collapse = " + "))))
    formula_LB <- as.formula (paste0 ("kg_log ~", (paste (model_terms, collapse = " + "))))

    
    # ==================
    # Calculate variable importance ----
    # ==================
    
    # # defining as the deviance explained in full model minus deviance explained in model dropping the variable of interest. e.g. McHenry et al. 2019
    # var_imp <- data.frame()
    # 
    # 
    # for (x in model_terms) {
    # 
    #   # single variable model
    #   G_PA <- gam (as.formula (paste0 ("Presence ~", x)),
    #                family = gaussian,
    #                data = spp_PA,
    #                gamma = gamma_PA)
    # 
    #   G_LB <- gam (as.formula (paste0 ("kg_log ~", x)),
    #             family = gaussian,
    #             data = spp_B,
    #             gamma = gamma_B)
    # 
    #   # full model without that variable
    # 
    #   G_drop_PA <- gam (as.formula (paste0 ("Presence ~", (paste (model_terms[! model_terms %in% x], collapse = " + ")))),
    #                     family = gaussian,
    #                     gamma = gamma_PA,
    #                     data = filter (spp_PA, !is.na (x))
    #                     )
    # 
    #   G_drop_LB <- gam (as.formula (paste0 ("kg_log ~", (paste (model_terms[! model_terms %in% x], collapse = " + ")))),
    #                  family = gaussian,
    #                  gamma = gamma_B,
    #                  data = filter (spp_B, !is.na (x)))
    # 
    # 
    #   # extract summaries, put together in temporary df. two rows, for lb and pa
    #   df <- data.frame (species = rep(spp_names[i], 2),
    #                     Var = rep(x, 2),
    #                     GAM = c("PA", "LB"),
    #                     R2_s = c(round (summary(G_PA)$dev.expl * 100, digits = 2),
    #                              round (summary(G_LB)$dev.expl * 100, digits = 2)
    #                              ),
    # 
    #                     R2_d =  c(round (summary(G_drop_PA)$dev.expl * 100, digits = 2),
    #                               round (summary(G_drop_LB)$dev.expl * 100, digits = 2)
    #                               ),
    # 
    #                     Diff_f = c(round (summary (gam_PA)$dev.expl * 100 - summary(G_drop_PA)$dev.expl * 100, 2),
    #                                round (summary (gam_LB)$dev.expl * 100 - summary(G_drop_LB)$dev.expl * 100, 2)
    #                                )
    #                     ) # end df
    # 
    #   # rbind to full data frame
    #   var_imp <- rbind (var_imp, df)
    # 
    # } # end variable importance loop
    # 
    # # rbind to build var_imp table
    # var_imp_all_spp <- rbind (var_imp_all_spp, var_imp)
    
    
    
    # ==================
    # Validate model with training/testing ----
    # ==================
    
    # combine PA and biomass data for splitting training/testing
    full_data_spp <- spp_PA %>%
      left_join (spp_B, by = "sample_id") %>% # add abundance data
      replace_na (list(n_tot = 0, kg_tot = 0, n_log = 0, kg_log = 0)) %>% # replace all NAs (absences) with zero. [what about biomass of 1? none have]
      right_join (mfri_pred, by = "sample_id") # add predictor variables and cut out faulty autumn samples. should have 27524 
    
    # models with salinity only go up to 2012
    #cutoff_yr <- ifelse (grepl ("sal", model_terms), 2007, 2013)
    
    train_spp_PA <-  spp_PA %>% 
      filter (year <= 2013)
    
    train_spp_B <- spp_B %>%
      filter (year <= 2013)
    
    # need combined PA and B for test dataset
    test_spp <- spp_B %>%
      dplyr::select (sample_id, kg_log, kg_tot) %>%
      right_join (spp_PA, by = "sample_id") %>%
      replace_na (list (kg_log = 0, kg_tot = 0)) %>%
      filter (year > 2013)
  
    
    gamma_PA_train <- log (nrow (train_spp_PA)) / 2
    gamma_B_train <- log (nrow (train_spp_B)) / 2
    
    # Presence-absence model
    gam_PA_train <- update (gam_PA, 
                            data = train_spp_PA,
                            gamma = gamma_PA_train)
      
    # log biomass model:
    gam_LB_train <- update (gam_LB,
                   data = train_spp_B,
                   gamma = gamma_B_train)
    
    
    # predict on testing data ----
    #do I want to have a different set of predictions I use for AUC vs. MASE?? yes??
    
    gampred_PA <- predict.gam (gam_PA_train, test_spp, type = "response")
    
    Eresid <- mean (exp (residuals.gam (gam_LB_train))) # why mean instead of median?
    
    # for biomass, issue with bormicon regions, where model breaks if test data has regions not in the training data. just filter out new bormicon regions from test data? This means I'll have several sets of predictions I'm using. One on the full test data to use for P/A validation--AUC, TSS etc. And for bormicon models, a smaller set to compare thermpred predictions. 
    
    if ("bormicon_region" %in%  model_terms) {
      
      # subset testing data for only bormicon regions the model was trained on
      test_spp_Borm <- test_spp %>%
        filter (bormicon_region %in% train_spp_B$bormicon_region)
      
      gampred_LB <- predict.gam (gam_LB_train, test_spp_Borm, type = "response")
      
      gampred_PA_borm <- predict.gam (gam_PA_train, test_spp_Borm, type = "response")
      
      ThermPred_spp <- gampred_PA_borm * (exp (gampred_LB)) * Eresid
      
    } else {
      
      gampred_LB <- predict.gam (gam_LB_train, test_spp, type = "response")
      
      ThermPred_spp <- gampred_PA * (exp (gampred_LB)) * Eresid
      
    }
    
    
    
    # ==================
    # PA sens/spec tests ----
    # ==================
    
    ## Model diagnostic statistics for PA--AUC, TSS, Kappa----
    
    #evaluate takes a vector of the predictions at observed/true presences and observed absences
    p_test <- as.vector(gampred_PA[which (test_spp$Presence == 1)])
    a_test <- as.vector(gampred_PA[which (test_spp$Presence == 0)])
    
    e_test <- evaluate (p_test, a_test)
    
    conf <- as.data.frame (e_test@confusion)
    
    p_train <- gam_PA$fitted.values[which(train_spp_PA$Presence == 1)]
    a_train <- gam_PA$fitted.values[which(train_spp_PA$Presence == 0)]
    
    e_train <- evaluate(p_train, a_train)
    
    # find prevalence threshold for training data, and then threshold from testing data that's closest to it (from Morely code)
    prev_th_train <- threshold (e_train, stat = "prevalence")
    th_diff <- abs (e_test@t - prev_th_train)
    e_ind <- which (th_diff == min(th_diff))
    
    
    # ==================
    ## Test performance against naive models ----
    # ==================
    
    ## Random walk forecast ----
    if ("bormicon_region" %in%  naive_terms) {
      
      RWF_forecast <- rwf (train_spp_B$kg_tot, drift = T, h = nrow (test_spp_Borm), lambda = NULL, biasadj = F) 
      
    } else {
      
      RWF_forecast <- rwf (train_spp_B$kg_tot, drift = T, h = nrow (test_spp), lambda = NULL, biasadj = F) 
      
    }
    
    ## Naive GAM (no environmental variables) forecast ----
    formula_PA_naive <- as.formula (paste0 ("Presence ~", (paste (naive_terms, collapse = " + "))))
    gam_PA_naive <- update (gam_PA_train, 
                            formula = formula_PA_naive)
  
    
    formula_LB_naive <- as.formula (paste0 ("kg_log ~", (paste (naive_terms, collapse = " + "))))
    gam_LB_naive <- update (gam_LB_train,
                            forumla = formula_LB_naive) 
   
    # Residuals and thermpred for naive gam
    Eresid_naive <- mean (exp (residuals.gam (gam_LB_naive)))
    
    # same issue for biomass
    # for biomass, issue with bormicon regions. just filter out new bormicon regions from test data?
    if ("bormicon_region" %in%  model_terms) {
      
      gampred_LB_naive_borm <- predict.gam (gam_LB_naive, test_spp_Borm, type = "response")
      
      gampred_PA_naive_borm <- predict.gam (gam_PA_naive, test_spp_Borm, type = "response")
      
      ThermPred_spp_naive <- gampred_PA_naive_borm * (exp (gampred_LB_naive_borm)) * Eresid_naive
      
    } else {
      
      gampred_LB_naive <- predict.gam (gam_LB_naive, test_spp, type = "response")
      
      gampred_PA_naive <- predict.gam (gam_PA_naive, test_spp, type = "response")
      
      ThermPred_spp_naive <- gampred_PA_naive * (exp (gampred_LB_naive)) * Eresid_naive
      
    }
    
    
  
    ## Calculate MAE for MASE ----
    # Mean absolute error, absolute value of expected - observed. Use total instead of log because exponentiated the GAM predictions
    if ("bormicon_region" %in%  model_terms) {
      MAE_gam <- abs(ThermPred_spp - test_spp_Borm$kg_tot) 
      
      MAE_gam_naive <- abs (ThermPred_spp_naive - test_spp_Borm$kg_tot)
      
      MAE_rwf <- abs (RWF_forecast$mean - test_spp_Borm$kg_tot)
      
    } else {
      MAE_gam <- abs(ThermPred_spp - test_spp$kg_tot) 
      
      MAE_gam_naive <- abs (ThermPred_spp_naive - test_spp$kg_tot)
      
      MAE_rwf <- abs (RWF_forecast$mean - test_spp$kg_tot)
    }
    
    ## Diebold-Mariano test  ----
    
    # gam vs naive gam
    if ("bormicon_region" %in%  model_terms) {
      E1 <- ThermPred_spp_naive - test_spp_Borm$kg_tot
      E2 <- ThermPred_spp - test_spp_Borm$kg_tot
    } else {
      E1 <- ThermPred_spp_naive - test_spp$kg_tot
      E2 <- ThermPred_spp - test_spp$kg_tot
    }
    
    # need complete cases; remove NA
    E1_cc <- E1 [ which (!is.na(E1) & !is.na(E2))]
    E2_cc <- E2 [ which (!is.na(E1) & !is.na(E2))]
    
    dm_gam <- dm.test (E1_cc, E2_cc, h = 1, power = 1, alternative = "greater")
    
    # gam vs rwf
    if ("bormicon_region" %in%  model_terms) {
      E1_rw <- RWF_forecast$mean - test_spp_Borm$kg_tot
      E2_rw <- ThermPred_spp - test_spp_Borm$kg_tot
    } else {
      E1_rw <- RWF_forecast$mean - test_spp$kg_tot
      E2_rw <- ThermPred_spp - test_spp$kg_tot
    }
    
    E1_cc_rw <- E1_rw [ which (!is.na(E1_rw) & !is.na(E2_rw))]
    E2_cc_rw <- E2_rw [ which (!is.na(E1_rw) & !is.na(E2_rw))]
    
    dm_rw <- dm.test (E1_cc_rw, E2_cc_rw, h = 1, power = 1, alternative = "greater")
    
    
    # ==================
    # Populate data frame ----
    # ==================
    spp_stats <- data.frame (
      
      ### general info
      species = spp_names[i],
      n_presence = length (which (spp_PA$Presence == 1)),
      
      ### presence/absence performance ----
      
      # dev expl and AIC from full model 
      PA_dev = round (summary (gam_PA)$dev.expl, 2),
      PA_AIC = round (AIC(gam_PA),2),
      
      # AUC, TSS, Kappa from training/testing 
      AUC = round(e_test@auc, 2),
      # TSS should be sensitivity + specificity - 1. Morely does tss_max from full model. going to do max from test?
      # https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2664.2006.01214.x
      TSS_mx = round (max (with (conf, (tp/(tp + fn) + (tn)/(tn+fp) - 1))), 2),
      TSS_th = round (with (conf[e_ind,], (tp/(tp + fn) + (tn)/(tn+fp) - 1)), 2),
      
      Kap_mx = round (max(e_test@kappa), 2),
      Kap_th = round (e_test@kappa[e_ind], 2),
      
      ### log biomass performance ----
      # dev expl and AIC from full model 
      LB_dev = round (summary (gam_LB)$dev.expl, 2),
      LB_AIC = round (AIC(gam_LB),2),
      #correlation from Morely code. have to remove any NAs -- this is essentially R2
      # LB_cor = round (cor (gampred_LB[which (test_spp$Presence ==1 & !is.na(gampred_LB))], test_spp$kg_log[which (test_spp$Presence == 1 & !is.na(gampred_LB))])^2, 2),
      
      ### MASE and DM ----
      
      # MASE is ratio of MAE from full gam and naive GAM. Want < 1 to trust full GAM. 
      # for Naive GAM
      MASE_GAM = mean (MAE_gam, na.rm = TRUE) / mean (MAE_gam_naive, na.rm = TRUE),
      DM_GAM_p = round (dm_gam$p.value, 2),
      DM_GAM_stat = round (dm_gam$statistic, 2),
      
      
      # for Random walk
      MASE_RWF = mean (MAE_gam, na.rm = TRUE) / mean (MAE_rwf, na.rm = TRUE),
      DM_RW_p = round (dm_rw$p.value, 2),
      DM_RW_stat = round (dm_rw$statistic, 2)
      
      
    ) # end stats df
    
  
    #rbind model stats to full data frame
    model_stats <- rbind (model_stats, spp_stats)

  } # end species for loop
  
  # save variable importance and model stats to csv
 # write.csv (var_imp_all_spp, file = paste0 ("Models/var_imp_", directory, ".csv"), row.names = FALSE)
  write.csv (model_stats, file = paste0("Models/GAM_performance_", directory, ".csv"), row.names = FALSE)
  
} # end function 







# ==================
# Run functions on models ----
# ==================

# ** note ** I ran most of these before combining the naive MASE code, so the bormicon with all the temperature variables is set up correctly


# fit on temp/depth model ----
# I already fit 25 spp on temp_depth with all temp variables (now excluding bt_min), but will overwrite. Directory would be Depth_Temp
td_model_terms <- c("s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)", "s(sst_max)",  "s(sst_min)", "s(bt_max)")

td_spp <-
  mfri_abun %>%
  group_by (species) %>%
  summarize (count = n()) %>%
  # match levels to spp_list to fix factor issue
  #mutate (species = factor (species, levels = levels(spp_list$Spp_ID))) %>%
  filter (count > 60, !species %in% c(41, 92)) %>%
  rename (Spp_ID = species) %>%
  left_join (spp_list, by = 'Spp_ID')

# save this spp list
td_spp_names <- as.vector(td_spp$sci_name_underscore)
save (td_spp_names, file = "Models/spp_Depth_Temp.RData")

load ("Models/spp_Depth_Temp.RData")

fit_gam_fun(model_terms = td_model_terms, 
            directory = "Depth_Temp", 
            spp_names = td_spp_names)

# smooth lat/lon model ----
sll_terms <- c ("s(lat, lon)", "year", "s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)")
load ("Models/spp_Smooth_latlon.RData")

fit_gam_fun (model_terms = sll_terms,
             directory = "Smooth_latlon", 
             spp_names = spp_Smooth_latlon$sci_name_underscore)

# full model with season, salinity, year ----
#did this with 25 spp
full_terms <- c("year", "season", "te(lon,lat)", "s(surface_temp)", "s(bottom_temp)", "s(gins_sal)", "s(tow_depth_begin)")

spp_25 <- spp_list %>%
  filter (n_autumn > 24 & n_spring > 35)

fit_gam_fun(model_terms = full_terms, 
            directory = "First_full_tensor_season", 
            spp_names = spp_25$sci_name_underscore)

# bormicon and all temperature variables ----

borm_terms <- c("bormicon_region", "year", "s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)", "s(sst_dev)", "s(bt_dev)", "s(sst_max)",  "s(sst_min)", "s(bt_max)", "s(bt_min)")

fit_gam_fun(model_terms = borm_terms, 
            directory = "Bormicon_allTemp", 
            spp_names = spp_25$sci_name_underscore)


# smooth lat/lon but with tensor spline, following pat convo
dir.create ("Models/Tensor_simple")
load ("Models/spp_Smooth_latlon.RData")
te_simple_terms <- c ("te(lon, lat)", "s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)")

fit_gam_fun(model_terms = te_simple_terms, 
            directory = "Tensor_simple", 
            spp_names = spp_Smooth_latlon$sci_name_underscore)

dir.create ("Models/Smooth_latlon_drop_year")
load ("Models/spp_Smooth_latlon.RData")
sll_dropyear_terms <- c ("s(lat, lon)", "s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)")

fit_gam_fun(model_terms = sll_dropyear_terms, 
            directory = "Smooth_latlon_drop_year", 
            spp_names = spp_Smooth_latlon$sci_name_underscore)


# bormicon 14 regions, add'l pat convo
dir.create ("Models/Borm_14")
load ("Models/spp_Smooth_latlon.RData")
Borm_14_terms <- c ("bormicon_region", "s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)")

fit_gam_fun(model_terms = Borm_14_terms, 
            directory = "Borm_14", 
            spp_names = spp_Smooth_latlon$sci_name_underscore)

# with fancy temperatures
dir.create ("Models/Borm_14_alltemp")
load ("Models/spp_Smooth_latlon.RData")
Borm_14_alltemp_terms <- c ("bormicon_region", "s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)", "s(sst_dev)", "s(bt_dev)", "s(sst_max)",  "s(sst_min)", "s(bt_max)")
borm14_naive <- c("bormicon_region", "s(tow_depth_begin)")

# broke for 45 myoxechpalus, in all circumstances
borm_spp <- filter (spp_Smooth_latlon, 
                    ! sci_name_underscore %in% c("Myoxocephalus_scorpius", "Amblyraja_hyperborea", "Rajella_fyllae,", "Lumpenus_lampretaeformis"))

# make life a bit easier by taking out NAs for GLORYS temps
mfri_pred <-  mfri_pred %>%
  filter (!is.na (sst_max)) # 20721

# broke on A. hyperborea for the variable importance or something. just going to fit the gams for the rest

fit_gam_fun(model_terms = Borm_14_alltemp_terms, 
            naive_terms = borm14_naive,
            directory = "Borm_14_alltemp", 
            spp_names = rev(borm_spp$sci_name_underscore))





# ==================
### Compare dev expl and AIC across all models, on standard dataset ----
# ==================

mfri_pred_complete <- mfri_pred %>%
  filter (complete.cases (year, lat, lon, tow_depth_begin, bottom_temp, surface_temp, season, bormicon_region, sst_max, sst_min, bt_max, sst_dev, bt_dev, gins_sal))

compare_GAM_performance_fun <- function (model_name, spp_names) {
  
  model_perf_comp <- data.frame ()
  
  for (model in model_name) {
    
    # signal where we are in the process 
    print(paste(model, Sys.time())) 
    
    model_perf <- data.frame ()
    
    for (i in 1:length(spp_names)) {
      
      # match species sci name to ID code in spp_list
      spp_id <- as.numeric(as.character(spp_list$Spp_ID[which (spp_list$sci_name_underscore == spp_names[i])])) # this fixes weird factor problem
      
      
      
      # Data subset from standardized set
      #Presence/absences for selected species, attach to predictor df. Select based on sci_name
      spp_PA_std <- mfri_pa %>%
        dplyr::select (sample_id, all_of(spp_names[i])) %>%
        left_join (mfri_pred_complete, by = "sample_id")
      colnames (spp_PA_std)[2] <- "Presence"
      
      
      # non-zero biomass data; this will have variable size. Select based on spp_ID code.
      spp_B_std <- mfri_abun %>%
        filter (species == spp_id) %>%
        left_join (mfri_pred_complete, by = "sample_id")
      
      gamma_PA_std <- log (nrow (spp_PA_std)) / 2
      gamma_B_std <- log (nrow (spp_B_std)) / 2
      
      # load previously fit GAMs
      if (model %in% c ("First_full_tensor_season", "Tensor_drop_season", "Smooth_latlon")) {
        load (file.path("Models", model, paste0(spp_names[i], "_PA_full.Rdata")))
        load (file.path("Models", model, paste0(spp_names[i], "_LB_full.Rdata")))
      } else {
        load (file.path("Models", model, paste0(spp_names[i], "_PA.Rdata")))
        load (file.path("Models", model, paste0(spp_names[i], "_LB.Rdata")))
      }
      
      # for some of the GAMs that I fit with this function later, need to specify formula
      formula_PA <- formula (gam_PA)
      formula_LB <- formula (gam_LB)
      
      # update on standard data
      set.seed (10)
      gam_PA_std <- update (gam_PA, 
                            data = spp_PA_std,
                            gamma = gamma_PA_std)
      gam_LB_std <- update (gam_LB, 
                            data = spp_B_std,
                            gamma = gamma_B_std)
      
      # fill data frame
      spp_df <- data.frame (
        species = rep(spp_names[i],2),
        response_var = c ("PA", "LB"),
        AIC = c (AIC(gam_PA_std), AIC (gam_LB_std)),
        dev_exp = c (summary (gam_PA_std)$dev.expl,
                     summary (gam_LB_std)$dev.expl)
      )
      
      model_perf <- rbind (model_perf, spp_df)
      
    } # end spp for loop
    
    model_perf$model_name = model
    
    model_perf_comp <- rbind (model_perf_comp, model_perf)
    
  } # end model for loop
  
  write.csv (model_perf_comp, file = "Models/GAM_overall_std_performance_25spp.csv", row.names = FALSE)
  
} # end function 

model_list <- c ("Borm_14", "Borm_14_alltemp", "Depth_Temp", "First_full_tensor_season", "Smooth_latlon", "Smooth_latlon_drop_year", "Tensor_drop_season", "Tensor_simple")    

spp_25 <- spp_list %>%
  filter (n_autumn > 24 & n_spring > 35) 

#spp_names <- filter (spp_Smooth_latlon, sci_name_underscore != "Myoxocephalus_scorpius")

compare_GAM_performance_fun (
  model_name = model_list,
  spp_names = spp_25$sci_name_underscore
)

## plot ----
model_comp_std <- read_csv ("Models/GAM_overall_std_performance_25spp.csv")

png ("Figures/model_std_comp_AIC.png", width = 16, height = 9, units = "in", res = 300)
model_comp_std %>%
  ggplot (aes (x = reorder(model_name, AIC, mean), y = AIC)) +
  geom_boxplot () +
  geom_jitter ()+
  facet_wrap (~factor(response_var, levels = c("PA", "LB")), scales = "free") +

  #switch = "y") +
  theme_bw () +
  ggtitle ("AIC with standardized data") +
  labs (x = "") +
  theme (
    axis.text = element_text (size = 16),
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
    axis.title = element_text (size = 18),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 18)
  )
dev.off()

png ("Figures/model_std_comp_devexp.png", width = 16, height = 9, units = "in", res = 300)
model_comp_std %>%
  ggplot (aes (x = reorder(model_name, dev_exp, mean), y = dev_exp)) +
  geom_boxplot () +
  geom_jitter ()+
  facet_wrap (~factor(response_var, levels = c("PA", "LB")), scales = "free") +
  
  #switch = "y") +
  theme_bw () +
  ggtitle ("Percent deviance explained with standardized data") +
  labs (x = "") +
  theme (
    axis.text = element_text (size = 16),
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
    axis.title = element_text (size = 18),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 18)
  )
dev.off()

