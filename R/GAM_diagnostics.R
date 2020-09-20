# GAM diagnostic tests
# 7/27/20
# JGM

# Run GAMs for each species on training data. Perform diagnostic tests with predictions on test data and save model summaries and tests in a table

library (tidyverse)
library (mgcv)
library (forecast)
library (dismo)

# Morely et al. 2018 code:
# https://github.com/pinskylab/project_velocity/blob/master/6_model_fitting_loop.R


# load data----
# presence absence data. this would still have 2011 and pre-2000 autumn samples
mfri_pa <- read_csv ("Data/PA_MFRI.csv",
                     col_types = cols(
                       sample_id = col_factor()
                     ))

# abundance data
mfri_abun <- read_csv ("Data/MFRI_comb_survey.csv",
                       col_types = cols(
                         sample_id = col_factor(),
                         species = col_factor(),
                         stat_sq = col_factor()
                       )
)%>%
  filter (!(year == 2011 & season == "autumn"),
          !(year < 2000 & season == "autumn")) %>% # remove autumn 2011 and pre-2000 autumn
  group_by (sample_id, species) %>%
  summarize (n_tot = sum(n_per_nautmile),
             kg_tot = sum(kg_per_nautmile)) %>%
  mutate (n_log = log(n_tot),
          kg_log = log(kg_tot))


# predictor variables
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
spp_list <- read_csv ("Data/species_eng.csv",
                      col_types = cols(
                        Spp_ID = col_factor()
                      ))

# I'm going to run these diagnostics on all species with enough data, ordered by number of presences
spp_totals <- mfri_abun %>% 
  group_by (species) %>% 
  summarize (count = n()) %>% 
  filter (count > 50) %>%  # need to have enough observations for the model: https://stackoverflow.com/questions/36719799/mgcv-gam-error-model-has-more-coefficients-than-data
  filter (!species %in% c(41, 72, 97, 35, 92, 49, 89)) # cut out problematic species that filtering by # observations doesn't catch, e.g. Pandalus has no presences in test data. 72 more coefficients


# write function ----
# function to run training/testing models and store diagnostics for each species, based on GAM formula. Use spp IDs because that's what's in the abundance dataset. 

# running into all sort of problems with factors, so do this with sci_name
 

gam_diag_fun <- function (model_terms, naive_terms, spp_names) {
  
  # empty data frame to hold GAM info
  GAM_diag <- data.frame() 
  
  for (i in 1:length(spp_names)) { # actually nicer to print a number than the species name for keeping track
    
    spp_id <- as.numeric(as.character(spp_list$Spp_ID[which (spp_list$sci_name_underscore == spp_names[i])])) # this fixes weird factor problem
    
    # signal where we are in the process
    print(paste(i, spp_names[i], spp_id, Sys.time()))  
    
    # ==================
    # Data setup ----
    # ==================
    
    #Presence/absence data for selected species
    PA_spp <- mfri_pa %>%
      # https://tidyselect.r-lib.org/reference/faq-external-vector.html need to use all_of for externally named variable
      dplyr::select (sample_id, all_of(spp_names[i])) 
    colnames (PA_spp)[2] <- "Presence"
    
    
    # biomass data--need presence-only training set and presence/absence testing set
    LB_spp <- mfri_abun %>%
      filter (species == spp_id)
    
    full_data_spp <- PA_spp %>%
      left_join (LB_spp, by = "sample_id") %>% # add abundance data
      replace_na (list(n_tot = 0, kg_tot = 0, n_log = 0, kg_log = 0)) %>% # replace all NAs (absences) with zero. [what about biomass of 1? none have]
      right_join (mfri_pred, by = "sample_id") # add predictor variables and cut out faulty autumn samples. should have 27524 
    
    # divide into testing and training, where training is before 2013 (roughly 80/20)
    train_spp <-  full_data_spp %>% 
      filter (year <= 2013)
    
    # presence-only data for training
    train_spp_B <- train_spp %>%
      filter (n_tot > 0)
    
    test_spp <-  full_data_spp %>%
      filter (year > 2013)
    
    # set gamma to penalize for wiggliness (From Morely github code and Simon wood)
    # used in Morely et al. 2018 https://github.com/pinskylab/project_velocity/blob/master/6_model_fitting_loop.R; cites https://people.maths.bris.ac.uk/~sw15190/mgcv/tampere/mgcv-advanced.pdf
    gamma_PA <- log (nrow (train_spp)) / 2
    gamma_B <- log (nrow (train_spp_B)) / 2
    
    
    # ==================
    # Fit  GAM ----
    # ==================
    
    set.seed(10) # needed for gamma maybe?
    
    # fit on training data
    # log biomass model:
    formula_LB <- as.formula (paste0 ("kg_log ~", model_terms))
    gam_LB <- gam (formula_LB,
                   family = "gaussian", 
                   data = train_spp_B,
                   gamma = gamma_B)
    
    # Presence-absence model
    formula_PA <- as.formula (paste0 ("Presence ~", model_terms))
    gam_PA <- gam (formula_PA,
                   family = "binomial", 
                   data = train_spp,
                   gamma = gamma_PA)
    
    # predict on testing data
    gampred_LB <- predict.gam (gam_LB, test_spp, type = "response")
    gampred_PA <- predict.gam (gam_PA, test_spp, type = "response")
    
    # ==================
    # PA sens/spec tests ----
    # ==================
    
    ## Model diagnostic statistics for PA--AUC, TSS, Kappa----
    
    #evaluate takes a vector of the predictions at observed/true presences and observed absences
    p_test <- as.vector(gampred_PA[which (test_spp$Presence == 1)])
    a_test <- as.vector(gampred_PA[which (test_spp$Presence == 0)])
    
    e_test <- evaluate (p_test, a_test)
    
    conf <- as.data.frame (e_test@confusion)
    
    #p_train <- gam_PA$fitted.values[which(train_spp$Presence == 1)]
    #a_train <- gam_PA$fitted.values[which(train_spp$Presence == 0)]
    
    #e_train <- evaluate(p_train, a_train)
    
    # find prevalence threshold for training data, and then threshold from testing data that's closest to it (from Morely code)
    #prev_th_train <- threshold (e_train, stat = "prevalence")
    #th_diff <- abs (e_test@t - prev_th_train)
    #e_ind <- which (th_diff == min(th_diff))
    
    
    # ==================
    ## Test performance against naive models ----
    # ==================
    
    ## Random walk forecast ----
    Naive_forecast <- rwf (train_spp$kg_tot, drift = T, h = nrow (test_spp), lambda = NULL, biasadj = F) 
    
    ## Naive GAM (no environmental variables) forecast ---
    formula_LB_naive <- as.formula (paste0 ("kg_log ~", naive_terms))
    gam_LB_naive <- gam (formula_LB_naive,
                         family = "gaussian", 
                         data = train_spp_B,
                         gamma = gamma_B)
    
    formula_PA_naive <- as.formula (paste0 ("Presence ~", naive_terms))
    gam_PA_naive <- gam (formula_PA_naive,
                         family = "binomial", 
                         data = train_spp,
                         gamma = gamma_PA)
    
    gampred_LB_naive <- predict.gam (gam_LB_naive, test_spp, type = "response")
    gampred_PA_naive <- predict.gam (gam_PA_naive, test_spp, type = "response")
    
    # Residuals and thermpred for naive gam
    Eresid_naive <- mean (exp (residuals.gam (gam_LB_naive)))
    
    ThermPred_spp_naive <- gampred_PA_naive * (exp (gampred_LB_naive)) * Eresid_naive
    
    # Residuals and thermpred for full gam. KK took inverse log of LB residuals, and then one mean value for each species
    Eresid <- mean (exp (residuals.gam (gam_LB)))
    
    # ThermPred is PA prediction * log (LB prediction) * log (LB residual). MAE is absolute value of thermPred - observed biomass.
    ThermPred_spp <- gampred_PA * (exp (gampred_LB)) * Eresid
    
    
    ## Calculate MAE for MASE ----
    MAE_gam <- abs(ThermPred_spp - test_spp$kg_tot) 
    
    MAE_gam_naive <- abs (ThermPred_spp_naive - test_spp$kg_tot)
    
    MAE_rwf <- abs (Naive_forecast$mean - test_spp$kg_tot)
    
    
    ## Diebold-Mariano test  ----
    
    # gam vs naive gam
    E1 <- ThermPred_spp_naive - test_spp$kg_tot
    E2 <- ThermPred_spp - test_spp$kg_tot
    
    # need complete cases; remove NA
    E1_cc <- E1 [ which (!is.na(E1) & !is.na(E2))]
    E2_cc <- E2 [ which (!is.na(E1) & !is.na(E2))]
    
    dm_gam <- dm.test (E1_cc, E2_cc, h = 1, power = 1, alternative = "greater")
    
    # gam vs rwf

    E1_rw <- Naive_forecast$mean - test_spp$kg_tot
    E2_rw <- ThermPred_spp - test_spp$kg_tot
    
    E1_cc_rw <- E1_rw [ which (!is.na(E1_rw) & !is.na(E2_rw))]
    E2_cc_rw <- E2_rw [ which (!is.na(E1_rw) & !is.na(E2_rw))]
    
    dm_rw <- dm.test (E1_cc_rw, E2_cc_rw, h = 1, power = 1, alternative = "greater")
    
    # ==================
    # Populate data frame ----
    # ==================
    spp_df <- data.frame (
      
      ### general info
      species = spp_names[i],
      n_presence = length (which (full_data_spp$Presence == 1)),
      
      ### presence/absence performance
      PA_dev = round (summary (gam_PA)$dev.expl, 2),
      AUC = round(e_test@auc, 2),
      # TSS should be sensitivity + specificity - 1. Morely does tss_max from full model. going to do max from test?
      # https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2664.2006.01214.x
      TSS_mx = round (max (with (conf, (tp/(tp + fn) + (tn)/(tn+fp) - 1))), 2),
      #TSS_th = round (with (conf[e_ind,], (tp/(tp + fn) + (tn)/(tn+fp) - 1)), 2),
      
      Kap_mx = round (max(e_test@kappa), 2),
      #Kap_th = round (e_test@kappa[e_ind], 2),
      
      ### log biomass performance
      LB_dev = round (summary (gam_LB)$dev.expl, 2),
      
      #correlation from Morely code. have to remove any NAs
      LB_cor = round (cor (gampred_LB[which (test_spp$Presence ==1 & !is.na(gampred_LB))], test_spp$kg_log[which (test_spp$Presence == 1 & !is.na(gampred_LB))])^2, 2),
      
      ### MASE and DM
      
      # for Naive GAM
      MASE_GAM = round (mean (MAE_gam, na.rm = TRUE) / mean (MAE_gam_naive, na.rm = TRUE), 2),
      DM_GAM_p = round (dm_gam$p.value, 2),
      DM_GAM_stat = round (dm_gam$statistic, 2),
     
      
      # for Random walk
      MASE_RWF = round (mean (MAE_gam, na.rm = TRUE) / mean (MAE_rwf, na.rm = TRUE), 2),
      DM_RW_p = round (dm_rw$p.value, 2),
      DM_RW_stat = round (dm_rw$statistic, 2)

      
    ) # end df
    
    # build GAM df
    GAM_diag <- rbind (GAM_diag, spp_df)
    
  } # end for loop
  
  # hard coding this not sure what to name
  write.csv (GAM_diag, file = "Models/Temp_Depth_GAM_diagnostics.csv", row.names = FALSE)    
  
} # end function


# perform on temperature and depth only model ----
td_model_terms <- "s(surface_temp) + s(bottom_temp) + s(tow_depth_begin) +  s(sst_max) + s(sst_min) + s(bt_max)"
td_naive_terms <- "s(tow_depth_begin)"

td_spp <-
  mfri_abun %>% 
  group_by (species) %>% 
  summarize (count = n()) %>% 
  # match levels to spp_list to fix factor issue
  #mutate (species = factor (species, levels = levels(spp_list$Spp_ID))) %>%
  filter (count > 60) %>%
  filter (!species %in% c(41, 49, 72, 92, 97)) 

td_spp_names <- spp_list$sci_name_underscore[which (spp_list$Spp_ID %in% td_spp$species)] 


gam_diag_fun (model_terms = td_model_terms, naive_terms = td_naive_terms, spp_names = td_spp_names)
  
# there's something wrong with c. rupestris with p_train, maybe about missing values and years. cutting out p_train threshold values for now. 


## check stats----
full_GAM_diag <- read.csv("Models/GAM_diagnostics_all_spp.csv")
td_GAM_diag <- read.csv ("Models/Temp_Depth_GAM_diagnostics.csv")

full_good <- full_GAM_diag %>%
  filter (MASE_GAM < 1 & DM_GAM_p < 0.05) # 30

td_good <- td_GAM_diag %>%
  filter (MASE_GAM < 1 & DM_GAM_p < 0.05) # 46

td_good$species[which (!td_good$species %in% full_good$Species)]
# cod, molva, brosme, ray, monkfish, halibut, limanda

full_good$Species[which (!full_good$Species %in% td_good$species)]
# M_aegle, Pollack, clupea, scomber, blue ling

# shared spp
full_good$Species[which (full_good$Species %in% td_good$species)]
# lemon, lump, merlan, sebastes marinus, a minor, whiffia, silus, mentella, molva dyp

summary (full_GAM_diag$PA_dev)
summary (td_GAM_diag$PA_dev)

summary (full_GAM_diag$LB_dev)
summary (td_GAM_diag$LB_dev)

# full definitely outperforms td in terms of dev expl. 