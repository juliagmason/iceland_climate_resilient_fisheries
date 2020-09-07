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
mfri_pred <- read_csv ("Data/MFRI_predictor_df.csv",
                       col_types = cols(
                         sample_id = col_factor(),
                         
                         stat_sq = col_factor(),
                         bormicon_region = col_factor()
                       )
) %>%
  filter (!(year == 2011 & season == "autumn"),
          !(year < 2000 & season == "autumn"))  # remove autumn 2011, 131 samples. Also cutting out autumn pre-2000 (as of 7/23/2020. so did this with MASE fitting, but not with original run through of full gams)

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


# Mega table to hold model diagnostics
n = rep(NA, length(spp_totals$species))

GAM_diag <- data.frame(
  
  # basic accounting info
  Species = n,
  N_Presences = n,
  
  # Presence/Absence skill
  PA_dev = n,
  AUC = n,
  TSS_mx = n,
  TSS_th = n,
  Kap_mx = n,
  Kap_th = n,
  
  # log Biomass 
  LB_dev = n,
  LB_cor = n,
  
  # Error with full GAM vs naive (no env variable) GAM
  MASE_GAM = n,
  DM_GAM_p = n,
  DM_GAM_stat = n,
  
  # Error with full GAM vs. random walk function
  MASE_RWF = n,
  DM_RW_p = n,
  DM_RW_stat = n,
  
  # Error with full GAM vs. random walk with lambda
  MASE_RWL = n,
  DM_RWL_p = n,
  DM_RWL_stat = n,
 
  # disregard, was looking at how the number of trailing zeros affected the random walk 
  Train_last_zeros = n
)


for (i in 1:length(spp_totals$species)) {
  
  # link to spp code
  spp <- as.numeric(as.character(spp_totals$species[i])) # no idea why this as.character is suddenly necessary
  
  # link species code to scientific name in PA dataset
  sci_name <- spp_list$sci_name_underscore[which (spp_list$Spp_ID == spp)]
  
  # Record species name
  GAM_diag$Species[i] <- sci_name
  
  # signal where we are in the process
  print(paste(i, sci_name, spp, Sys.time()))
  
  # ==================
  # Data setup ----
  # ==================
  
  #Presence/absence data for selected species
  PA_spp <- mfri_pa %>%
    # https://tidyselect.r-lib.org/reference/faq-external-vector.html need to use all_of for externally named variable
    dplyr::select (sample_id, all_of(sci_name)) 
  colnames (PA_spp)[2] <- "Presence"
  
  
  # biomass data--need presence-only training set and presence/absence testing set
  LB_spp <- mfri_abun %>%
    filter (species == spp)
  
  full_data_spp <- PA_spp %>%
    left_join (LB_spp, by = "sample_id") %>% # add abundance data
    replace_na (list(n_tot = 0, kg_tot = 0, n_log = 0, kg_log = 0)) %>% # replace all NAs (absences) with zero. [what about biomass of 1? none have]
    right_join (mfri_pred, by = "sample_id") # add predictor variables and cut out faulty autumn samples. should have 27524 
  
  # divide into testing and training
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
  
  # Record total number of presences
  GAM_diag$N_Presences[i] <- length (which (full_data_spp$Presence == 1))
  
  # ==================
  # Fit full GAM ----
  # ==================
  
  set.seed(10) # needed for gamma maybe?
  
  # fit on training data
  # log biomass model:
  gam_LB <- gam (kg_log ~ s(lat, lon) + year + s(surface_temp) + s(bottom_temp) + s(tow_depth_begin) + bormicon_region,
                 family = "gaussian", 
                 data = train_spp_B,
                 gamma = gamma_B)
  
  # Presence-absence model
  gam_PA <- gam (Presence ~ s(lat, lon) + year + s(surface_temp) + s(bottom_temp) + s(tow_depth_begin) + bormicon_region,
                 family = "binomial", 
                 data = train_spp,
                 gamma = gamma_PA)
  
  # predict on testing data
  gampred_LB <- predict.gam (gam_LB, test_spp, type = "response")
  gampred_PA <- predict.gam (gam_PA, test_spp, type = "response")
  
  # ==================
  # Full GAM diagnostics ----
  # ==================
  
  # grab deviance explained 
  GAM_diag$PA_dev[i] <- round (summary (gam_PA)$dev.expl, 2)
  
  ## Model diagnostic statistics for PA--AUC, TSS, Kappa----
  
  #evaluate takes a vector of the predictions at observed/true presences and observed absences
  p_test <- as.vector(gampred_PA[which (test_spp$Presence == 1)])
  a_test <- as.vector(gampred_PA[which (test_spp$Presence == 0)])
  
  e_test <- evaluate (p_test, a_test)
  
  GAM_diag$AUC[i] <- round(e_test@auc, 2)
  
  conf <- as.data.frame (e_test@confusion)
  
  p_train <- gam_PA$fitted.values[which(train_spp$Presence == 1)]
  a_train <- gam_PA$fitted.values[which(train_spp$Presence == 0)]
  
  e_train <- evaluate(p_train, a_train)
  
  
  # find prevalence threshold for training data, and then threshold from testing data that's closest to it (from Morely code)
  prev_th_train <- threshold (e_train, stat = "prevalence")
  th_diff <- abs (e_test@t - prev_th_train)
  e_ind <- which (th_diff == min(th_diff))
  
  # TSS should be sensitivity + specificity - 1. Morely does tss_max from full model. going to do max from test?
  # https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2664.2006.01214.x
  GAM_diag$TSS_mx[i] <- round (max (with (conf, (tp/(tp + fn) + (tn)/(tn+fp) - 1))), 2)
  GAM_diag$TSS_th[i] <- round (with (conf[e_ind,], (tp/(tp + fn) + (tn)/(tn+fp) - 1)), 2)
  
  GAM_diag$Kap_mx[i] <- round (max(e_test@kappa), 2)
  GAM_diag$Kap_th[i] <- round (e_test@kappa[e_ind], 2)
  
  ## Model diagnostic for biomass----
  
  GAM_diag$LB_dev[i] <- round (summary (gam_LB)$dev.expl, 2)
  
  #correlation from Morely code. have to remove any NAs
  GAM_diag$LB_cor[i] <- round (cor (gampred_LB[which (test_spp$Presence ==1 & !is.na(gampred_LB))], test_spp$kg_log[which (test_spp$Presence == 1 & !is.na(gampred_LB))])^2, 2)
  
  
  
  ## Test performance against naive models ----
  ## Random walk forecast
  
  Naive_forecast_lambda <-rwf(train_spp$kg_tot, drift=T, h= nrow (test_spp), lambda = -0.5, biasadj=T) 
  # with drift = F, this is just the last value observed, repeated h times. with drift = T, random walk based on the last value, but seems totally dependent on the last value. 
  
  # try with and without biasadj = T
  Naive_forecast <- rwf (train_spp$kg_tot, drift = T, h = nrow (test_spp), lambda = NULL, biasadj = F) 
  
  # don't see any difference with biasadj = T, with either log or total biomass. when I have both biasadj and lambda = -0.5, it does make a difference. much larger drift with biasadj = T
  
  ## Naive GAM (no environmental variables)
  gam_LB_naive <- gam (kg_log ~ s(lat, lon) + year + s(tow_depth_begin),
                       family = "gaussian", 
                       data = train_spp_B,
                       gamma = gamma_B)
  
  gam_PA_naive <- gam (Presence ~ s(lat, lon) + year + s(tow_depth_begin),
                       family = "binomial", 
                       data = train_spp,
                       gamma = gamma_PA)
  
  gampred_LB_naive <- predict.gam (gam_LB_naive, test_spp, type = "response")
  gampred_PA_naive <- predict.gam (gam_PA_naive, test_spp, type = "response")
  
  Eresid_naive <- mean (exp (residuals.gam (gam_LB_naive)))
  
  ThermPred_spp_naive <- gampred_PA_naive * (exp (gampred_LB_naive)) * Eresid_naive
  
  # Residuals and thermpred for full gam. KK took inverse log of LB residuals, and then one mean value for each species
  Eresid <- mean (exp (residuals.gam (gam_LB)))
  
  # ThermPred is PA prediction * log (LB prediction) * log (LB residual). MAE is absolute value of thermPred - observed biomass.
  ThermPred_spp <- gampred_PA * (exp (gampred_LB)) * Eresid
  
  
  ## Calculate MAE
  MAE_gam <- abs(ThermPred_spp - test_spp$kg_tot) 
  
  MAE_gam_naive <- abs (ThermPred_spp_naive - test_spp$kg_tot)
  
  MAE_rwf <- abs (Naive_forecast$mean - test_spp$kg_tot)
  
  MAE_rwf_lambda <- abs (Naive_forecast_lambda$mean^-0.5 - test_spp$kg_tot) # should it be transformed? abs (Naive_forecast_lambda$mean^-0.5 - test_spp$kg_tot)
  
  ## Calculate MASE and DM test ----
  GAM_diag$MASE_GAM[i] = round (mean (MAE_gam, na.rm = TRUE) / mean (MAE_gam_naive, na.rm = TRUE), 2)
  
  ## Diebold-Mariano test
  E1 <- ThermPred_spp_naive - test_spp$kg_tot
  E2 <- ThermPred_spp - test_spp$kg_tot
  
  E1_cc <- E1 [ which (!is.na(E1) & !is.na(E2))]
  E2_cc <- E2 [ which (!is.na(E1) & !is.na(E2))]
  
  dm_gam <- dm.test (E1_cc, E2_cc, h = 1, power = 1, alternative = "greater")
  
  GAM_diag$DM_GAM_stat[i] <- round (dm_gam$statistic, 2)
  GAM_diag$DM_GAM_p[i] <- round (dm_gam$p.value, 2)
  
  # MASE and DM for rwf
  
  GAM_diag$MASE_RWF[i]  <-  round (mean (MAE_gam, na.rm = TRUE) / mean (MAE_rwf, na.rm = TRUE), 2)
  
  ## Diebold-Mariano test
  E1_rw <- Naive_forecast$mean - test_spp$kg_tot
  E2_rw <- ThermPred_spp - test_spp$kg_tot
  
  E1_cc_rw <- E1_rw [ which (!is.na(E1_rw) & !is.na(E2_rw))]
  E2_cc_rw <- E2_rw [ which (!is.na(E1_rw) & !is.na(E2_rw))]
  
  dm_rw <- dm.test (E1_cc_rw, E2_cc_rw, h = 1, power = 1, alternative = "greater")
  
  GAM_diag$DM_RW_stat[i] <- round (dm_rw$statistic, 2)
  GAM_diag$DM_RW_p[i] <- round (dm_rw$p.value, 2)
  
  # rwf lambda. Likely won't use this--Charles Perretti on 7/22/2020 said choosing the lambda (box cox transformation) is somewhat arbitrary, but if you do use it, also use biasadj = T
  
  GAM_diag$MASE_RWL[i]  <-  round (mean (MAE_gam, na.rm = TRUE) / mean (MAE_rwf_lambda, na.rm = TRUE), 2)
  
  E1_rwL <- Naive_forecast_lambda$mean - test_spp$kg_tot
  E2_rwL <- ThermPred_spp - test_spp$kg_tot
  
  E1_cc_rwL <- E1_rw [ which (!is.na(E1_rwL) & !is.na(E2_rwL))]
  E2_cc_rwL <- E2_rw [ which (!is.na(E1_rwL) & !is.na(E2_rwL))]
  
  dm_rwL <- dm.test (E1_cc_rwL, E2_cc_rwL, h = 1, power = 1, alternative = "greater")
  
  GAM_diag$DM_RWL_stat[i] <- round (dm_rwL$statistic, 2)
  GAM_diag$DM_RWL_p[i] <- round (dm_rwL$p.value, 2)
  
  # I'm curious how much the last value or number of trailing zeros of the training set affects naive forecast. Count how many trailing zeros--total number minus the last non-zero row number. 
  
  GAM_diag$Train_last_zeros[i] <- nrow (train_spp) - max (which (train_spp$kg_log != 0))

}


write.csv (GAM_diag, file = "Models/GAM_diagnostics_all_spp.csv", row.names = FALSE)

## check stats----
GAM_diag <- read.csv("Models/GAM_diagnostics_all_spp.csv")

GAM_diag %>%
  filter (MASE_GAM < 1 & DM_GAM_p < 0.05) %>%
  View()
