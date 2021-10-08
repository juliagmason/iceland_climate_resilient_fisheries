# Function to run GAMs on full data for all species
# 9/16/2020

# JGM

library (mgcv) # for GAMs. will break tidyverse::select
library (tidyverse)
library (forecast) # for Diebold-Mariano test
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
# I compiled these in Build_MFRI_predictor_df.R. Each row is a survey tow
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
          !(year < 2000 & season == "autumn")) %>%
  dplyr::select (sample_id, year, tow_depth_begin, bottom_temp, surface_temp, bormicon_region, sst_max, sst_min, bt_max, bt_min, rugosity, rugosity1, rugosity6) %>%
  # filter out NAs for borm_14 model predictors to speed things up. now should have 19759 rows
  drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max"))


# relate abundance data to predictor data and fill in zeros for absences
# just do species with enough observations for model
# 53 species with enough data for temp and depth models
load ("Models/spp_Bormicon.RData") # called borm_spp, table with scientific name and numeric species identifier

full_spp_sample <- expand_grid((mfri_pa$sample_id), borm_spp$Spp_ID) 
colnames (full_spp_sample) <- c("sample_id", "species")

mfri_abun_full <-  mfri_abun %>%
  # will have to recalculate logs now that I'm including zeros
  dplyr::select (-c(kg_log, n_log, n_tot)) %>%
  filter (species %in% borm_spp$Spp_ID) %>%
  full_join (full_spp_sample, by = c("sample_id", "species")) %>%
  replace_na (list (kg_tot = 0)) %>%
  # right join to cut out faulty samples
  right_join (mfri_pred, by = "sample_id")
    

# Function to run GAM, full data, all species----

fit_gam_fun <- function (sci_name, directory, model_terms, naive_terms, fit_gam = TRUE, var_imp = TRUE, gam_stats = TRUE) {
  # sci_name is scientific name separated by an underscore, or a vector of scientific names if using for loop options
  # model_terms will be a concatenated list
  # directory will be the model name I've been using, e.g. "Borm_14_alltemp"
  
  
 
  # #empty data frame for saving variable importance info if var_imp = TRUE
   var_imp_all_spp <- data.frame()

  # empty data frame for saving model summary statistics if gam_stats = TRUE
  model_stats <- data.frame()

  for (i in 1:length(sci_name)) {

    # match species sci name to ID code in spp_list
    spp_id <- as.numeric(as.character(borm_spp$Spp_ID[which (borm_spp$sci_name_underscore == sci_name[i])]))# this fixes weird factor problem

    # signal where we are in the process 
    print(paste(i, sci_name[i], spp_id, Sys.time())) 
   
    
    # ==================
    # Set up training and testing data ----
    # ==================

    spp_data_train <- mfri_abun_full %>%
      filter (species == spp_id, 
              year <= 2013) %>% 
      drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "bt_max"))
    
    spp_data_test <- mfri_abun_full %>%
      filter (species == spp_id, 
              year > 2013) %>% 
      drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "bt_max"))

   
   # ==================
   # Fit full GAM ----
   # ==================
    
    formula_gam <- as.formula (paste0 ("kg_tot ~", (paste (model_terms, collapse = " + "))))

    # If fitting GAMs for the first time
    if (fit_gam == TRUE) {
      # time this, highly variable 
      start_time <- Sys.time()
      
      gam_tw <- gam (formula = formula_gam,
                     family = tw(link = "log"),
                     data = spp_data_train,
                     method = "REML",
                     select = TRUE)
      
      
      
      save (gam_tw,  file = paste0("Models/", directory, "/", sci_name, ".Rdata"))
      
      end_time <- Sys.time()
      
    } else if (fit_gam == FALSE) { # If already fit GAMs and loading again for additional tests
      
      load (paste0("Models/", directory, "/", sci_name[i], ".Rdata"))
      
    }

    
    # ==================
    # Calculate variable importance ----
    # ==================

    # defining as the deviance explained in full model minus deviance explained in model dropping the variable of interest. e.g. McHenry et al. 2019
    
    # https://r.789695.n4.nabble.com/variance-explained-by-each-term-in-a-GAM-td836513.html
    # Simon Wood--need to use the same smoothing terms as the full model, otherwise smoothers will change to compensate for the dropped variable
    
    if (var_imp == TRUE) {
      
      var_imp_df <- data.frame()
      
      
      for (x in model_terms) {
        
        
        # full model without that variable
        # need to keep the smoothers constant, https://r.789695.n4.nabble.com/variance-explained-by-each-term-in-a-GAM-td836513.html
        full_sp <- gam_nb$sp
        
        # can't figure out how to just drop one, full_spp[-x] doesn't work. would have to do it by index
        term_index = which (model_terms == x)
        
        drop_sp <- full_sp[-term_index]
        
        G_drop <- gam (as.formula (paste0 ("kg_tot ~", (paste (model_terms[! model_terms %in% x], collapse = " + ")))),
                       #sp = drop_sp,
                       family = tw(link = "log"),
                       data = spp_data_train,
                       method = "REML")
        
        
        
        # extract summaries, put together in temporary df. two rows, for lb and pa
        df <- data.frame (species = sci_name[i],
                          Var = x, 
                          R2_d = round (summary(G_drop)$dev.expl * 100, digits = 2),
                          Diff_f = round (summary (gam_nb)$dev.expl * 100 - summary(G_drop)$dev.expl * 100, 2)
        )
        
        # rbind to full data frame
        var_imp_df <- rbind (var_imp_df, df)
        
      } # end variable importance loop
      
      # rbind to build var_imp table
      var_imp_all_spp <- rbind (var_imp_all_spp, var_imp_df)
      
    } # end var.imp ifelse

    
    # ==================
    # Populate data frame ----
    # ==================
    if (gam_stats == TRUE) {
      
      
      # Calculate prediction error ----
      predict_tw <- predict.gam (gam_tw, spp_data_test, type = "response")
      
      MAE_tw <- mean(abs(predict_tw - spp_data_test$kg_tot))
      
      
      # Calculate prediction error of a naive GAM with static variables only
      gam_naive <- update (gam_tw, formula = as.formula(paste0 ("kg_tot ~", (paste (naive_terms, collapse = " + ")))))
      
      predict_naive <- predict.gam (gam_naive, spp_data_test, type = "response")
      
      MAE_naive <- mean (abs (predict_naive - spp_data_test$kg_tot))
      
      E1 <- predict_naive - spp_data_test$kg_tot
      E2 <- predict_tw - spp_data_test$kg_tot
      
      # need complete cases; remove NA
      E1_cc <- E1 [ which (!is.na(E1) & !is.na(E2))]
      E2_cc <- E2 [ which (!is.na(E1) & !is.na(E2))]
      
      dm_gam <- dm.test (E1_cc, E2_cc, h = 1, power = 1, alternative = "greater")
      
      # Calculate mean absolute squared error, ratio of MAE/naive MAE
      MASE = MAE_tw / MAE_naive
      
      
      spp_stats <- data.frame (
        
        ## general info
        species = sci_name[i],
        n_presence = length (which (spp_data_train$kg_tot > 0)),
        dev_ex = round (summary (gam_tw)$dev.expl, 2),
        AIC = round (AIC(gam_tw),2),
        
        dev_ex_naive = round (summary (gam_naive)$dev.expl, 2),
        
        
        # MASE and DM ----
        # MASE is ratio of MAE from full gam and naive GAM. Want < 1 to trust full GAM. 
        # for Naive GAM
        MAE = round(MAE_nb, 3),
        MAE_naive = round(MAE_naive, 3),
        MASE = round(MAE_tw / MAE_naive,2),
        DM_GAM_p = round(dm_gam$p.value, 2),
        DM_GAM_stat = round(dm_gam$statistic, 2)
        
        
      ) # end stats df
      
      
      # rbind model stats to full data frame
      model_stats <- rbind (model_stats, spp_stats)
      
    } # end gam.stats ifelse
    
  } # end species for loop 
  
  # save csvs
  if (var_imp == TRUE) {
    write.csv (var_imp_all_spp, file = paste0 ("Models/var_imp_sp_", directory, ".csv"), row.names = FALSE)
  }
  
  if (gam_stats == TRUE) {
    write.csv (model_stats, file = paste0("Models/GAM_performance_", directory, ".csv"), row.names = FALSE)
  }
  
} # end function




# ==================
# Run functions on models ----
# ==================

system.time (fit_gam_fun (sci_name = borm_spp$sci_name_underscore, 
                         directory = "Rug_tw_LL",
                         fit_gam = TRUE,
                         var_imp = TRUE, 
                         gam_stats = TRUE, 
                         model_terms = c("s(rugosity)", "s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)", "s(sst_max)", "s(sst_min)", "s(bt_max)"),
                         naive_terms = c("s(rugosity)", "s(tow_depth_begin)")
                         )
); beep()
