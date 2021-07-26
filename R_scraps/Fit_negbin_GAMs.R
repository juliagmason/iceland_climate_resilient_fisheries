# Separate script for negbin so i don't keep making dumb mistakes
# 5/28/2021

library (mgcv)
library (tidyverse)
library (forecast)

# load data----

# species list
# This relates species IDs to scientific names
spp_list <- read_csv ("Data/species_eng.csv",
                      col_types = cols(
                        Spp_ID = col_factor()
                      ))

# full presence and biomass data
# read in new rug
mfri_rug <- read_csv ("Data/MFRI_predictor_df.csv",
                                 col_types = cols(
                                   sample_id = col_factor()
                                   )
                      ) %>%
                        dplyr::select (sample_id, rugosity1, rugosity6)


load ("Data/abun_full_borm.RData")

mfri_abun_full_borm <- mfri_abun_full_borm %>%
  left_join (mfri_rug, by = "sample_id")

# outlier_pts <- mfri_abun_full_borm %>%
#   filter (
#     
#   )
# 
# mfri_abun_full_borm_rm_outliers <- mfri_abun_full_borm %>%
#   filter (species == 1 & kg_tot > 500 |
#           species == 2 & kg_tot > 400 |
#           species == 5 & kg_tot > 1000 |
#           species == 39 & kg_tot > 20 |
#           species == 57 & kg_tot > 0.75 |
#           species == 75 & kg_tot > 0.4 |
#           species == 81 & kg_tot > 0.1 
#           )
# 
# mfri_abun_full_borm <- mfri_abun_full_borm %>%
#   filter (! sample_id %in% mfri_abun_full_borm_rm_outliers$sample_id)




# ==================
# Function to fit models ----
# ==================

fit_nb_fun <- function (sci_name, directory, model_terms, fit_gam = TRUE, var_imp = TRUE, gam_stats = TRUE) {
  
  # model_terms will be a concatenated list
  # directory will be the model name I've been using, e.g. "Borm_14_alltemp"
  # sci_name is a scientific name separated by an underscore, or a vector of scientific names if using for loop options
  
  # if running fresh and want to run full GAMs, variable importance, and model summary stats together, easier to use an internal for loop to save multiple CSVs. Would just need to uncomment the below and replace instances of sci_name with sci_name[i]
  
  # #empty data frame for saving variable importance info
  var_imp_all_spp <- data.frame()
  
  # empty data frame for saving model summary statistics
  model_stats <- data.frame()
  
  for (i in 1:length(sci_name)) {
  
  spp_id <- as.numeric(as.character(spp_list$Spp_ID[which (spp_list$sci_name_underscore == sci_name[i])]))# this fixes weird factor problem
  
  spp_data_train <- mfri_abun_full_borm %>%
    filter (species == spp_id, 
            year <= 2013) %>% 
    drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "bt_max"))
  
  spp_data_test <- mfri_abun_full_borm %>%
    filter (species == spp_id, 
            year > 2013) %>% 
    drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "bt_max"))
  
  formula_nb <- as.formula (paste0 ("kg_tot ~", (paste (model_terms, collapse = " + "))))
  
  # If fitting GAMs for the first time
  if (fit_gam == TRUE) {
    # time this, highly variable 
    start_time <- Sys.time()
    
    gam_nb <- gam (kg_tot  ~ s(rugosity) + s(surface_temp) + s(bottom_temp) + s(tow_depth_begin) + s(sst_max) + s(sst_min) + s(bt_max),
                   family = nb(link = "log"),
                   data = spp_data_train,
                   method = "REML",
                   select = TRUE)
    
    
    
    save (gam_nb,  file = paste0("Models/", directory, "/", sci_name, ".Rdata"))
    
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
      
      # single variable model
      
      # G <- gam (as.formula (paste0 ("kg_tot ~", x)),
      #           family = nb(link = "log"),
      #           data = spp_data_train,
      #           method = "REML")
      
      # full model without that variable
      # need to keep the smoothers constant, https://r.789695.n4.nabble.com/variance-explained-by-each-term-in-a-GAM-td836513.html
      full_sp <- gam_nb$sp
      
      # can't figure out how to just drop one, full_spp[-x] doesn't work. would have to do it by index
      term_index = which (model_terms == x)
      
      drop_sp <- full_sp[-term_index]
      
      G_drop <- gam (as.formula (paste0 ("kg_tot ~", (paste (model_terms[! model_terms %in% x], collapse = " + ")))),
                     sp = drop_sp,
                      family = nb(link = "log"),
                      data = spp_data_train,
                      method = "REML")
      

    
      # extract summaries, put together in temporary df. two rows, for lb and pa
      df <- data.frame (species = sci_name[i],
                        Var = x, 
                        #R2_s = round (summary(G)$dev.expl * 100, digits = 2),
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
    predict_nb <- predict.gam (gam_nb, spp_data_test, type = "response")
    
    MAE_nb <- mean(abs(predict_nb - spp_data_test$kg_tot))
    
    
    # Calculate prediction error of a naive GAM with static variables only
    gam_naive <- update (gam_nb, formula = as.formula("kg_tot ~ s(rugosity) +  s(tow_depth_begin)"))
    
    predict_naive <- predict.gam (gam_naive, spp_data_test, type = "response")
    
    MAE_naive <- mean (abs (predict_naive - spp_data_test$kg_tot))
    
    E1 <- predict_naive - spp_data_test$kg_tot
    E2 <- predict_nb - spp_data_test$kg_tot
    
    # need complete cases; remove NA
    E1_cc <- E1 [ which (!is.na(E1) & !is.na(E2))]
    E2_cc <- E2 [ which (!is.na(E1) & !is.na(E2))]
    
    dm_gam <- dm.test (E1_cc, E2_cc, h = 1, power = 1, alternative = "greater")
    
    # Calculate mean absolute squared error, ratio of MAE/naive MAE
    MASE = MAE_nb / MAE_naive
    
    DM_GAM_p = dm_gam$p.value
    
    # Initial abbreviated data storage for model comparison
    # stats_nb <- data.frame (
    #   species = sci_name,
    #   AIC = round(AIC(gam_nb), 2),
    #   dev_ex = round (summary (gam_nb)$dev.expl, 2),
    #   MAE = round (MAE_nb, 2),
    #   MASE_suit = ifelse (MASE < 1 & DM_GAM_p < 0.05, 1, 0),
    #   time = end_time - start_time
    # )
    
    spp_stats <- data.frame (
    
    ## general info
    species = sci_name[i],
    n_presence = length (which (spp_data_train$kg_tot > 0)),
    dev_ex = round (summary (gam_nb)$dev.expl, 2),
    AIC = round (AIC(gam_nb),2),
    
    dev_ex_naive = round (summary (gam_naive)$dev.expl, 2),
    
    
    # MASE and DM ----
    # MASE is ratio of MAE from full gam and naive GAM. Want < 1 to trust full GAM. 
    # for Naive GAM
    MAE = round(MAE_nb, 3),
    MAE_naive = round(MAE_naive, 3),
    MASE = round(MAE_nb / MAE_naive,2),
    DM_GAM_p = round(dm_gam$p.value, 2),
    DM_GAM_stat = round(dm_gam$statistic, 2)
    
    
  ) # end stats df
  
  
  # rbind model stats to full data frame
  model_stats <- rbind (model_stats, spp_stats)
  
  } # end gam.stats ifelse
  
} # end species for loop if using
  if (var_imp == TRUE) {
  write.csv (var_imp_all_spp, file = paste0 ("Models/var_imp_sp_", directory, ".csv"), row.names = FALSE)
  }
  
  if (gam_stats == TRUE) {
  write.csv (model_stats, file = paste0("Models/GAM_performance_", directory, ".csv"), row.names = FALSE)
  }
  
} # end function

########################################################################################################

# sample with one species
system.time (fit_nb_fun (sci_name = "Gadus_morhua", directory = "Rug_nb", fit_gam = FALSE, gam_stats = TRUE, var_imp = FALSE, model_terms = c("s(rugosity)", "s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)", "s(sst_max)", "s(sst_min)", "s(bt_max)")))
# 3 mins per species

system.time (fit_nb_fun (sci_name = borm_55, directory = "Rug_nb_btmin", fit_gam = FALSE, gam_stats = TRUE, var_imp = FALSE, model_terms = c("s(rugosity)", "s(surface_temp)", "s(bt_min)", "s(tow_depth_begin)", "s(sst_max)", "s(sst_min)", "s(bt_max)")))

system.time (fit_nb_fun (sci_name = borm_55, directory = "Borm_14_nb", fit_gam = FALSE, gam_stats = TRUE, var_imp = FALSE, model_terms = c("bormicon_region", "s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)", "s(sst_max)", "s(sst_min)", "s(bt_max)")))

system.time (fit_nb_fun (sci_name = borm_55, directory = "Borm_14_nb_btmin", fit_gam = FALSE, gam_stats = TRUE, var_imp = FALSE, model_terms = c("bormicon_region", "s(surface_temp)", "s(bt_min)", "s(tow_depth_begin)", "s(sst_max)", "s(sst_min)", "s(bt_max)")))


load ("Models/spp_Smooth_latlon.RData")

# filter out species that break for bormicon region
borm_spp <- filter (spp_Smooth_latlon, 
                    ! sci_name_underscore %in% c("Myoxocephalus_scorpius", "Amblyraja_hyperborea", "Rajella_fyllae,", "Lumpenus_lampretaeformis", "Ammodytes_marinus", "Mallotus_villosus", "Micromesistius_poutassou", "Argentina_silus", "Scomber_scombrus", "Clupea_harengus"))

borm_55 <- borm_spp$sci_name_underscore


system.time (fit_nb_fun (sci_name = borm_55, directory = "Rug_nb", fit_gam = FALSE, var_imp = TRUE, gam_stats = TRUE, model_terms = c("s(rugosity)", "s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)", "s(sst_max)", "s(sst_min)", "s(bt_max)")))

system.time (fit_nb_fun (sci_name = borm_55, directory = "Rug_nb", fit_gam = FALSE, var_imp = FALSE, gam_stats = TRUE, model_terms = c("s(rugosity)", "s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)", "s(sst_max)", "s(sst_min)", "s(bt_max)"))) #  13739.84 

# run for rugosity
dir.create ("Models/Rug_nb") # renamed Rug_nb_btmin this will have btmin
rug_ls <- expand.grid (sci_name = borm_spp$sci_name_underscore, 
                       directory = "Rug_nb") %>%
  as.list()

system.time (rug_nb_df <- pmap_dfr (rug_ls, fit_nb_fun)); beep() # 4106.81 for btmin, 4517.12  for bt
row.names (rug_nb_df) <- NULL
save (rug_nb_df, file = "Models/nb_rug_stats.RData")

dir.create ("Models/Rug1_nb")
rug_ls <- expand.grid (sci_name = borm_spp$sci_name_underscore, 
                       directory = "Rug1_nb") %>%
  as.list()

system.time (rug1_nb_df <- pmap_dfr (rug_ls, fit_nb_fun)); beep() # 7510.76
row.names (rug1_nb_df) <- NULL
save (rug1_nb_df, file = "Models/nb_rug1_stats.RData")

dir.create ("Models/Rug6_nb")
rug_ls <- expand.grid (sci_name = borm_spp$sci_name_underscore, 
                       directory = "Rug6_nb") %>%
  as.list()

system.time (rug6_nb_df <- pmap_dfr (rug_ls, fit_nb_fun)); beep() # 5346.39 
row.names (rug6_nb_df) <- NULL
save (rug6_nb_df, file = "Models/nb_rug6_stats.RData")

# run on nb_btmin, time will be wrong, just about gam naive
nb_btmin_files <- list.files ("Models/Borm_14_nb_btmin/")
nb_btmin_spp <- map_chr(nb_btmin_files, scan_fun)
nb_btmin_ls <- expand.grid (sci_name = nb_btmin_spp, 
                            directory = "Borm_14_nb_btmin") %>% 
  as.list()

system.time (nb_btmin_df <- pmap_dfr (nb_btmin_ls, fit_nb_fun)); beep() # 3423.07 
row.names (nb_btmin_df) <- NULL
save (nb_btmin_df, file = "Models/nb_btmin_20spp.RData")


# fit remaining btmin
btmin_remain <- borm_spp$sci_name_underscore[which (!borm_spp$sci_name_underscore %in% nb_btmin_spp)]
nb_btmin_ls <- expand.grid (sci_name = btmin_remain, 
                            directory = "Borm_14_nb_btmin") %>% 
  as.list()

system.time (nb_btmin_remain <- pmap_dfr (nb_btmin_ls, fit_nb_fun)); beep() # 2262.18
row.names (nb_btmin_remain) <- NULL
save (nb_btmin_remain, file = "Models/nb_btmin_remain.RData") # this is actually the same 20 spp??

nb_btmin_df <- rbind (nb_btmin_df, nb_btmin_remain)
save (nb_btmin_df, file = "Models/nb_btmin_borm_stats.RData")


# run on remaining borm spp
nb_ls <- expand.grid (sci_name = borm_spp$sci_name_underscore[21:55],
                           directory = "Borm_14_nb") %>%
  as.list()

system.time(nb_remain_df <- pmap_dfr (nb_ls, fit_nb_fun)); beep() # 2339.87 
row.names (nb_remain_df) <- NULL
save (nb_remain_df, file = "Models/nb_remaining_Borm_stats.RData")

load ("Models/nb_20spp.RData")
nb_borm <- rbind (nb_df, nb_remain_df)
save (nb_borm, file = "Models/nb_borm_stats.RData")

# run special for cod rm outliner
dir.create ("Models/Borm_14_nb_RMoutliers")

directory <- "Borm_14_nb_RMoutliers"

spp_data_train <- mfri_abun_full_borm %>%
  filter (species == 1, 
          year <= 2013,
          kg_tot < 500) %>% 
  drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "bt_max"))

# rugosity for outliers
dir.create ("Models/Rug_nb_RMoutliers")
outlier_ls <- expand.grid (sci_name = c("Gadus_morhua", "Melanogrammus_aeglefinus", "Sebastes_marinus", "Chimaera_monstrosa", "Rhinonemus_cimbrius", "Myctophidae", "Cottunculus_microps"), 
                           directory = "Rug_nb_RMoutliers") %>% 
  as.list()

system.time(rug_outlier_df <- pmap_dfr (outlier_ls, fit_nb_fun)); beep() 
row.names (rug_outlier_df) <- NULL
save (rug_outlier_df, file = "Models/Rug_nb_outliers_stats.RData")

# will error go down if I take out the two 500 points?? goes from 8.2 to 7.96
spp_data_test <- mfri_abun_full_borm %>%
  filter (species == spp_id, 
          year > 2013,
          kg_tot < 500) %>% 
  drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "bt_max"))

# also try for haddock

spp_data_train <- mfri_abun_full_borm %>%
  filter (species == 2, 
          year <= 2013,
          kg_tot < 500) %>% 
  drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "bt_max"))


# will error go down if I take out the two 500 points?? goes from 8.2 to 7.96
spp_data_test <- mfri_abun_full_borm %>%
  filter (species == 2, 
          year > 2013,
          kg_tot < 500) %>% 
  drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "bt_max"))


