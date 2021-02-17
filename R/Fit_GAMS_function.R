# Function to run GAMs on full data for all species
# 9/16/2020

# JGM

library (mgcv)
library (tidyverse)
library (forecast) # for RWF forecast
library (dismo) # for PA evaluations
library (beepr) # to notify when scripts finish

# load data----

# presence absence data.  
# This is a presence/absence matrix, where each column is a species scientific name and each row is a survey tow. The first column is an ID column for sample ID. Every sample is represented, so absences have been filled in with zeros. I constructed this in Write_full_PA_MFRI_csv.R. This would still have 2011 and pre-2000 autumn samples.

mfri_pa <- read_csv ("Data/PA_MFRI.csv",
                     col_types = cols(
                       sample_id = col_factor()
                     ))

# abundance data
# this is biomass data, where each row is the calculated n and kg per nautical mile of a species of a size class in a survey tow. Absences are not included. Species are indicated by numeric species IDs, not scientific name. I'm not distinguishing between species size classes, so will take a total n_per_nautmile and kg_per_nautmile for each survey tow.
mfri_abun <- read_csv ("Data/MFRI_comb_survey.csv",
                       col_types = cols(
                         sample_id = col_factor(),
                         species = col_factor(),
                         stat_sq = col_factor()
                         )
                       ) %>% 
  # remove autumn 2011 [labor strike] and pre-2000 autumn [sites weren't standard yet]
  filter (!(year == 2011 & season == "autumn"),
          !(year < 2000 & season == "autumn")
          ) %>% 
  # get one biomass value for each species and survey tow
  group_by (sample_id, species) %>%
  summarize (n_tot = sum(n_per_nautmile),
             kg_tot = sum(kg_per_nautmile)
             ) %>%
  # calculate log biomass
  mutate (n_log = log(n_tot),
          kg_log = log(kg_tot))


# predictor variables
# I compiled these in Build_MFRI_predictor_df.R. Each row is a survey tow, should match mfri_pa
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
  # remove autumn 2011 [labor strike] and pre-2000 autumn [sites weren't standard yet]
  filter (!(year == 2011 & season == "autumn"),
          !(year < 2000 & season == "autumn")
          ) %>%
          # as of 12/17/2020, also filter out NAs for borm_14 model predictors to speed things up, and so I can replace relevant fake zeroes. now should have 19759 rows
  drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "sst_dev"))
    
 

# species list
# This relates species IDs to scientific names
spp_list <- read_csv ("Data/species_eng.csv",
                      col_types = cols(
                        Spp_ID = col_factor()
                      ))



# Function to run GAM, full data, all species----

fit_gam_fun <- function (model_terms, naive_terms, directory, sci_name) {
  # model_terms will be a concatenated list
  # directory will be the model name I've been using, e.g. "Borm_14_alltemp"
  # sci_name is scientific name separated by an underscore, or a vector of scientific names if using for loop options
  
  # if running fresh and want to run full GAMs, variable importance, and model summary stats together, easier to use an internal for loop to save multiple CSVs. Would just need to uncomment the below and replace instances of sci_name with sci_name[i]

  # empty data frame for saving variable importance info
  var_imp_all_spp <- data.frame()

  # empty data frame for saving model summary statistics
  model_stats <- data.frame()

  for (i in 1:length(sci_name)) {
    
    # match species sci name to ID code in spp_list
    spp_id <- as.numeric(as.character(spp_list$Spp_ID[which (spp_list$sci_name_underscore == sci_name[i])]))# this fixes weird factor problem
    
    # signal where we are in the process 
    print(paste(sci_name[i], Sys.time()))
   
    
    # ==================
    # Data setup ----
    # ==================

    #Presence/absences for selected species. Grab relevant P/A column and attach to predictor df. 
   spp_PA <- mfri_pa %>%
     dplyr::select (sample_id, all_of(sci_name[i])) %>%
     right_join (mfri_pred, by = "sample_id") # use right join to cut out faulty fall samples
   colnames (spp_PA)[2] <- "Presence"


   # non-zero biomass data; this will have variable size. Select based on spp_ID code.
   spp_B <- mfri_abun %>%
     filter (species == spp_id) %>%
     inner_join (mfri_pred, by = "sample_id")
   
   # replace some absences with very near-zero number so can have predictions in all bormicon regions
   
   # https://github.com/pinskylab/project_velocity/blob/master/6_model_fitting_loop.R line 87
   # determine which regions have zero presences
   empty_regions <- spp_PA %>%
     group_by (bormicon_region) %>%
     summarise (count = sum (Presence)) %>%
     filter (count == 0) %>%
     pull (bormicon_region)
   
   # I need to work from spp_PA to get the number of observations from each region. then I just need to add 10% of n to the end of spp_B. but need random subset of spp_PA to attach spatial information. 
   
   if (length (empty_regions) > 0) { 
     set.seed (10)
     
     #https://stackoverflow.com/questions/64034962/randomly-replacing-percentage-of-values-per-group-with-na-in-r-dataframe
     fake_zeros <- spp_PA %>%
       filter (bormicon_region %in% empty_regions) %>%
       group_by (bormicon_region) %>%
       # replace a random 10% of kg_log values with log(1e-10) (approx -23)
       mutate (kg_log = replace (Presence, sample (row_number(), size = ceiling (0.1 * n()), replace = FALSE), log(1e-10)), 
               #add columns to match spp_B
               species = as.factor(spp_id),
               n_tot = exp(kg_log), 
               kg_tot = exp(kg_log),
               n_log = kg_log
               ) %>%
       # move columns to match spp_B
       relocate (c(species, n_tot, kg_tot, n_log, kg_log), .after =sample_id) %>%
       dplyr::select (-Presence) %>%
       # only take the replaced rows
       filter (kg_log != 0) 
     
     # append fake zeros to spp_B
     spp_B <- rbind (spp_B, fake_zeros)
     
   }
   

   # set gamma to penalize for wiggliness (From Morely github code and Simon wood)
   # used in Morely et al. 2018 https://github.com/pinskylab/project_velocity/blob/master/6_model_fitting_loop.R; cites https://people.maths.bris.ac.uk/~sw15190/mgcv/tampere/mgcv-advanced.pdf
   gamma_PA <- log (nrow (spp_PA)) / 2
   gamma_B <- log (nrow (spp_B)) / 2

   
   # ==================
   # Fit full GAM ----
   # ==================
   
  #   set.seed(10) # needed for gamma maybe? used in Morely code.
  # 
  #  # Presence-absence model
  #  formula_PA <- as.formula (paste0 ("Presence ~", (paste (model_terms, collapse = " + "))))
  #  gam_PA <- gam (formula_PA,
  #                 family = "binomial",
  #                 data = spp_PA,
  #                 gamma = gamma_PA)
  # 
  # 
  # # log biomass model:
  # formula_LB <- as.formula (paste0 ("kg_log ~", (paste (model_terms, collapse = " + "))))
  # gam_LB <- gam (formula_LB,
  #                family = "gaussian",
  #                data = spp_B,
  #                gamma = gamma_B)
  # 
  # 
  # 
  # # save models in appropriate folder
  # save (gam_PA, file = paste0("Models/", directory, "/", sci_name[i], "_PA.Rdata"))
  # save (gam_LB, file = paste0("Models/", directory, "/", sci_name[i], "_LB.Rdata"))

  #} # end function here if just fitting models, not revisiting model diagnostics


  # already fit models, just load
  load (file.path("Models", directory, paste0(sci_name[i], "_PA.Rdata")))
  load (file.path("Models", directory, paste0(sci_name[i], "_LB.Rdata")))

  formula_PA <- as.formula (paste0 ("Presence ~", (paste (model_terms, collapse = " + "))))
  formula_LB <- as.formula (paste0 ("kg_log ~", (paste (model_terms, collapse = " + "))))

    
    # ==================
    # Calculate variable importance ----
    # ==================
    
    # defining as the deviance explained in full model minus deviance explained in model dropping the variable of interest. e.g. McHenry et al. 2019
    var_imp <- data.frame()


    for (x in model_terms) {

      # single variable model
      G_PA <- gam (as.formula (paste0 ("Presence ~", x)),
                   family = gaussian,
                   data = spp_PA,
                   gamma = gamma_PA)

      G_LB <- gam (as.formula (paste0 ("kg_log ~", x)),
                family = gaussian,
                data = spp_B,
                gamma = gamma_B)

      # full model without that variable

      G_drop_PA <- gam (as.formula (paste0 ("Presence ~", (paste (model_terms[! model_terms %in% x], collapse = " + ")))),
                        family = gaussian,
                        gamma = gamma_PA,
                        data = filter (spp_PA, !is.na (x))
                        )

      G_drop_LB <- gam (as.formula (paste0 ("kg_log ~", (paste (model_terms[! model_terms %in% x], collapse = " + ")))),
                     family = gaussian,
                     gamma = gamma_B,
                     data = filter (spp_B, !is.na (x)))


      # extract summaries, put together in temporary df. two rows, for lb and pa
      df <- data.frame (species = rep(sci_name[i], 2),
                        Var = rep(x, 2),
                        GAM = c("PA", "LB"),
                        R2_s = c(round (summary(G_PA)$dev.expl * 100, digits = 2),
                                 round (summary(G_LB)$dev.expl * 100, digits = 2)
                                 ),

                        R2_d =  c(round (summary(G_drop_PA)$dev.expl * 100, digits = 2),
                                  round (summary(G_drop_LB)$dev.expl * 100, digits = 2)
                                  ),

                        Diff_f = c(round (summary (gam_PA)$dev.expl * 100 - summary(G_drop_PA)$dev.expl * 100, 2),
                                   round (summary (gam_LB)$dev.expl * 100 - summary(G_drop_LB)$dev.expl * 100, 2)
                                   )
                        ) # end df

      # rbind to full data frame
      var_imp <- rbind (var_imp, df)

    } # end variable importance loop

    # rbind to build var_imp table
    var_imp_all_spp <- rbind (var_imp_all_spp, var_imp)

    
    
    # ==================
    # Validate model with training/testing ----
    # ==================

    # Split data into ~80% as training data and ~20% most recent as testing data
    # For most models, cutoff year is 2013. Models with salinity only go up to 2012
    #cutoff_yr <- ifelse (grepl ("sal", model_terms), 2007, 2013)
    
    train_spp_PA <-  spp_PA %>% 
      filter (year <= 2013)
    
    train_spp_B <- spp_B %>%
      filter (year <= 2013)
    
    # replace additional fake zeroes if needed? breaking for a few spp
    empty_regions_train <- train_spp_PA %>%
      group_by (bormicon_region) %>%
      summarise (count = sum (Presence)) %>%
      filter (count == 0) %>%
      pull (bormicon_region)

    if (length (empty_regions_train) > 0) { 
      set.seed (10)
      
      #https://stackoverflow.com/questions/64034962/randomly-replacing-percentage-of-values-per-group-with-na-in-r-dataframe
      fake_zeros_train <- train_spp_PA %>%
        filter (bormicon_region %in% empty_regions_train) %>%
        group_by (bormicon_region) %>%
        mutate (kg_log = replace (Presence, sample (row_number(), size = ceiling (0.1 * n()), replace = FALSE), log(1e-10)), 
                #add columns to match spp_B
                species = as.factor(spp_id),
                n_tot = exp(kg_log), 
                kg_tot = exp(kg_log),
                n_log = kg_log
                ) %>%
        # move columns to match spp_B
        relocate (c(species, n_tot, kg_tot, n_log, kg_log), .after =sample_id) %>%
        dplyr::select (-Presence) %>%
        # only take the replaced rows
        filter (kg_log != 0) 
      
      # append fake zeros to spp_B
      train_spp_B <- rbind ( train_spp_B, fake_zeros_train)
      
    }
    
    # need combined PA and B for test dataset
    test_spp <- spp_B %>%
      dplyr::select (sample_id, kg_log, kg_tot) %>%
      right_join (spp_PA, by = "sample_id") %>%
      replace_na (list (kg_log = 0, kg_tot = 0)) %>%
      filter (year > 2013)
  
    # new gamma for training set
    gamma_PA_train <- log (nrow (train_spp_PA)) / 2
    gamma_B_train <- log (nrow (train_spp_B)) / 2
    
    set.seed (10)
    
    # Presence-absence model
    gam_PA_train <- update (gam_PA, 
                            data = train_spp_PA,
                            gamma = gamma_PA_train)
      
    # log biomass model
    gam_LB_train <- update (gam_LB,
                   data = train_spp_B,
                   gamma = gamma_B_train)
    
    
    # predict on testing data ----
    
    gampred_PA <- predict.gam (gam_PA_train, test_spp, type = "response")
    
    Eresid <- mean (exp (residuals.gam (gam_LB_train))) # why mean instead of median? why lb instead of PA?
    # Also looking at median Eresid because I'm getting some wildly different Eresids for some species (think this is because of fake zeros)
    Eresid_med <- median (exp (residuals.gam (gam_LB_train)))
    
      
    gampred_LB <- predict.gam (gam_LB_train, test_spp, type = "response")
    
    # Combine PA and LB predictions as thermal prediction
    ThermPred_spp <- gampred_PA * (exp (gampred_LB)) * Eresid
    
    ThermPred_spp_med <- gampred_PA * (exp (gampred_LB)) * Eresid_med
      

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
    # # Test performance against naive models ----
    # ==================

    # Random walk forecast ----

    RWF_forecast <- rwf (train_spp_B$kg_tot, drift = T, h = nrow (test_spp), lambda = NULL, biasadj = F)


    # Naive GAM (no environmental variables) forecast ----
    formula_PA_naive <- as.formula (paste0 ("Presence ~", (paste (naive_terms, collapse = " + "))))
    gam_PA_naive <- update (gam_PA_train,
                            formula = formula_PA_naive)


    formula_LB_naive <- as.formula (paste0 ("kg_log ~", (paste (naive_terms, collapse = " + "))))
    gam_LB_naive <- update (gam_LB_train,
                            formula = formula_LB_naive)

   
    # Residuals and thermpred for naive gam
    Eresid_naive <- mean (exp (residuals.gam (gam_LB_naive))) # crazy!!
    Eresid_naive_med <- median (exp (residuals.gam (gam_LB_naive)))
    
    gampred_LB_naive <- predict.gam (gam_LB_naive, test_spp, type = "response")
    gampred_PA_naive <- predict.gam (gam_PA_naive, test_spp, type = "response")
    
    ThermPred_spp_naive <- gampred_PA_naive * (exp (gampred_LB_naive)) * Eresid_naive
    ThermPred_spp_naive_med <- gampred_PA_naive * (exp (gampred_LB_naive)) * Eresid_naive_med
      
  
    ## Calculate MAE for MASE ----
    # Mean absolute error, absolute value of expected - observed. Use total instead of log because exponentiated the GAM predictions

    MAE_gam <- abs(ThermPred_spp - test_spp$kg_tot) 
    
    MAE_gam_naive <- abs (ThermPred_spp_naive - test_spp$kg_tot)
    
    MAE_gam_med <- abs (ThermPred_spp_med - test_spp$kg_tot)
    MAE_gam_naive_med <- abs (ThermPred_spp_naive_med - test_spp$kg_tot)
    
    MAE_rwf <- abs (RWF_forecast$mean - test_spp$kg_tot)

    ## Diebold-Mariano test  ----
    
    # gam vs naive gam

    E1 <- ThermPred_spp_naive - test_spp$kg_tot
    E2 <- ThermPred_spp - test_spp$kg_tot

    # need complete cases; remove NA
    E1_cc <- E1 [ which (!is.na(E1) & !is.na(E2))]
    E2_cc <- E2 [ which (!is.na(E1) & !is.na(E2))]
    
    dm_gam <- dm.test (E1_cc, E2_cc, h = 1, power = 1, alternative = "greater")
    
    # also test MAE with median Eresid
    E1_med <- ThermPred_spp_naive_med - test_spp$kg_tot
    E2_med <- ThermPred_spp_med - test_spp$kg_tot
    
    # need complete cases; remove NA
    E1_cc_med <- E1_med [ which (!is.na(E1_med) & !is.na(E2_med))]
    E2_cc_med <- E2_med [ which (!is.na(E1_med) & !is.na(E2_med))]
    
    dm_gam_med <- dm.test (E1_cc_med, E2_cc_med, h = 1, power = 1, alternative = "greater")
    
    # gam vs rwf

    E1_rw <- RWF_forecast$mean - test_spp$kg_tot
    E2_rw <- ThermPred_spp - test_spp$kg_tot

    E1_cc_rw <- E1_rw [ which (!is.na(E1_rw) & !is.na(E2_rw))]
    E2_cc_rw <- E2_rw [ which (!is.na(E1_rw) & !is.na(E2_rw))]

    dm_rw <- dm.test (E1_cc_rw, E2_cc_rw, h = 1, power = 1, alternative = "greater")
    
    
    # ==================
    # Populate data frame ----
    # ==================
    spp_stats <- data.frame (
      
      ## general info
      species = sci_name[i],
      n_presence = length (which (spp_PA$Presence == 1)),
      
      ## presence/absence performance ----

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

      ## log biomass performance ----
      # dev expl and AIC from full model
      LB_dev = round (summary (gam_LB)$dev.expl, 2),
      LB_AIC = round (AIC(gam_LB),2),
      #correlation from Morely code. have to remove any NAs -- this is essentially R2
      LB_cor = round (cor (gampred_LB[which (test_spp$Presence ==1 & !is.na(gampred_LB))], 
                           test_spp$kg_log[which (test_spp$Presence == 1 & !is.na(gampred_LB))])^2, 2),

      # MASE and DM ----
      
      # MASE is ratio of MAE from full gam and naive GAM. Want < 1 to trust full GAM. 
      # for Naive GAM
      MASE_GAM = mean (MAE_gam, na.rm = TRUE) / mean (MAE_gam_naive, na.rm = TRUE),
      DM_GAM_p = dm_gam$p.value,
      DM_GAM_stat = dm_gam$statistic,
      
      
      MASE_GAM_med = mean (MAE_gam_med, na.rm = TRUE) / mean (MAE_gam_naive_med, na.rm = TRUE),
      DM_GAM_p_med = dm_gam_med$p.value,
      DM_GAM_stat_med = dm_gam_med$statistic,
      
      # for Random walk
      MASE_RWF = mean (MAE_gam, na.rm = TRUE) / mean (MAE_rwf, na.rm = TRUE),
      DM_RW_p = dm_rw$p.value,
      DM_RW_stat = dm_rw$statistic

      
    ) # end stats df
    
  
    # rbind model stats to full data frame
    model_stats <- rbind (model_stats, spp_stats)

  } # end species for loop if using

  # save variable importance and model stats to csv if using for loop
  write.csv (var_imp_all_spp, file = paste0 ("Models/var_imp_", directory, ".csv"), row.names = FALSE)
  write.csv (model_stats, file = paste0("Models/GAM_performance_", directory, ".csv"), row.names = FALSE)
  
} # end function 







# ==================
# Run functions on models ----
# ==================

# Full model run ----
dir.create ("Models/Borm_14_alltemp")

# 61 species with enough data for temp and depth models
load ("Models/spp_Smooth_latlon.RData")

# filter out species that break for bormicon region
borm_spp <- filter (spp_Smooth_latlon, 
                    ! sci_name_underscore %in% c("Myoxocephalus_scorpius", "Amblyraja_hyperborea", "Rajella_fyllae,", "Lumpenus_lampretaeformis"))

Borm_14_alltemp_terms <- c ("bormicon_region", "s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)", "s(sst_dev)", "s(bt_dev)", "s(sst_max)",  "s(sst_min)", "s(bt_max)")

borm14_naive <- c("bormicon_region", "s(tow_depth_begin)")

system.time(lapply (borm_spp$sci_name_underscore, fit_gam_fun,
          model_terms = Borm_14_alltemp_terms, 
          naive_terms = borm14_naive,
          directory = "Borm_14_alltemp")
          ); beep()
          
# try comparing borm_14_alltemp with drop borm depth/temp interaction term. set interaction term as new model, and borm_14_altemp as naive.
dir.create ("Models/Borm_14_alltemp_intn_test")

TxD_terms_noborm <- c ("s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)", "s(sst_dev)", "s(bt_dev)", "s(sst_max)",  "s(sst_min)", "s(bt_max)", "ti(bottom_temp, tow_depth_begin)")

fit_gam_fun (sci_name = borm_spp$sci_name_underscore, 
        directory = "Borm_14_alltemp_intn_test",
        model_terms = TxD_terms_noborm,
          naive_terms = Borm_14_alltemp_terms); beep()



dir.create ("Models/Borm_14_alltemp")
load ("Models/spp_Smooth_latlon.RData")

# broke for 45 myoxechpalus, in all circumstances
borm_spp <- filter (spp_Smooth_latlon, 
                    ! sci_name_underscore %in% c("Myoxocephalus_scorpius", "Amblyraja_hyperborea", "Rajella_fyllae,", "Lumpenus_lampretaeformis"))

Borm_14_alltemp_terms <- c ("bormicon_region", "s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)", "s(sst_dev)", "s(bt_dev)", "s(sst_max)",  "s(sst_min)", "s(bt_max)")

# Tried two naive models--one without temperature, and one without bormicon region, per PJW suggestion
borm14_naive <- c("bormicon_region", "s(tow_depth_begin)")

drop_borm_naive <- c("s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)", "s(sst_dev)", "s(bt_dev)", "s(sst_max)",  "s(sst_min)", "s(bt_max)")

# didn't set this up well to work with pmap. use sapply to make a data frame. 
system.time( drop_borm_mase_test <- as.data.frame (
  sapply (borm_spp$sci_name_underscore, fit_gam_fun,
          model_terms = Borm_14_alltemp_terms, 
          naive_terms = drop_borm_naive,
          directory = "Borm_14_alltemp")
  )
); beep()

# this interpreted spp names as the columns
write.csv (t(drop_borm_mase_test), file = "Models/Drop_borm_MASE_test.csv", row.names = FALSE)

dbmt <- as.data.frame(t(drop_borm_mase_test))

drop_borm_suit <- dbmt[which (dbmt$MASE_GAM_med < 1  & dbmt$DM_GAM_p_med < 0.05),]
drop_borm_suit <- dbmt[which (dbmt$MASE_GAM < 1  & dbmt$DM_GAM_p < 0.05),]
drop_borm_unsuit <- borm_spp$sci_name_underscore[which (!borm_spp$sci_name_underscore %in% drop_borm_suit$species)]

MASE <- read_csv ("Models/GAM_performance_Borm_14_alltemp.csv")

spp_suit <- MASE %>%
  filter (MASE_GAM < 1, DM_GAM_p < 0.05)

borm_unsuit <- borm_spp %>% filter (!sci_name_underscore %in% spp_suit$species) %>% pull (sci_name_underscore)


# move below to a supplemental script

# ==================
### Compare dev expl and AIC across all models, on standard dataset ----
# ==================

# different predictor variables have different NAs, so models have different number of inputs. Fit models again on a standard complete dataset to compare AIC

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

