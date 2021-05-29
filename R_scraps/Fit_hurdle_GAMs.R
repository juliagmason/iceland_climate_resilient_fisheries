# Hurdle models
# 5/24/2021
# JGM

library (tidyverse)
library (mgcv)
library (forecast) # for RWF forecast and DM
library (beepr) # to notify when scripts finish

# species list
# This relates species IDs to scientific names
spp_list <- read_csv ("Data/species_eng.csv",
                      col_types = cols(
                        Spp_ID = col_factor()
                      ))



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
mfri_pred_borm <-  read_csv ("Data/MFRI_predictor_df.csv",
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
  dplyr::select (sample_id, year, tow_depth_begin, bottom_temp, surface_temp, bormicon_region, sst_max, sst_min, bt_max, bt_min, rugosity) %>%
# as of 12/17/2020, also filter out NAs for borm_14 model predictors to speed things up, and so I can replace relevant fake zeroes. now should have 19759 rows
drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max"))

# load borm species
load ("Models/spp_Smooth_latlon.RData")

# broke for 45 myoxechpalus, in all circumstances
borm_spp <- filter (spp_Smooth_latlon, 
                    ! sci_name_underscore %in% c("Myoxocephalus_scorpius", "Amblyraja_hyperborea", "Rajella_fyllae,", "Lumpenus_lampretaeformis", "Ammodytes_marinus", "Mallotus_villosus", "Micromesistius_poutassou", "Argentina_silus", "Scomber_scombrus", "Clupea_harengus"))

# make a df of all species x sample_id 

full_spp_sample <- expand_grid((mfri_pa$sample_id), borm_spp$Spp_ID) 
colnames (full_spp_sample) <- c("sample_id", "species")

mfri_abun_full_borm <-  mfri_abun %>%
  # will have to recalculate logs now that I'm including zeros
  dplyr::select (-c(kg_log, n_log, n_tot)) %>%
  filter (species %in% borm_spp$Spp_ID) %>%
  full_join (full_spp_sample, by = c("sample_id", "species")) %>%
  replace_na (list (kg_tot = 0)) %>%
  # right join to cut out faulty samples
  right_join (mfri_pred_borm, by = "sample_id")

save (mfri_abun_full_borm, file = "Data/abun_full_borm.RData")

## Try zero_inflated Poisson----
#https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/ziP.html

model_formula <- c ("bormicon_region", "s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)", "s(sst_max)",  "s(sst_min)", "s(bt_max)")

system.time(b <- gam (as.integer(kg_tot) ~ bormicon_region + s(surface_temp) + s(bottom_temp) + s(tow_depth_begin) + s(sst_max) + s(sst_min) + s(bt_max),
          family = ziP(),
          data = filter (mfri_abun_full_borm, species == 1)))
# Error in eval(family$initialize) : 
#Non-integer response variables are not allowed with ziP 


##################################################################################

# Tweedie gam----
# https://rdrr.io/cran/mgcv/man/Tweedie.html

load ("Data/abun_full_borm.RData")

spp_list <- read_csv ("Data/species_eng.csv",
                      col_types = cols(
                        Spp_ID = col_factor()
                      ))


fit_tw_fun <- function (sci_name, directory) {
  
  spp_id <- as.numeric(as.character(spp_list$Spp_ID[which (spp_list$sci_name_underscore == sci_name)]))# this fixes weird factor problem
  
  spp_data_train <- mfri_abun_full_borm %>%
       filter (species == spp_id, 
         year <= 2013) %>% 
    drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "bt_max"))
  
  spp_data_test <- mfri_abun_full_borm %>%
    filter (species == spp_id, 
            year > 2013) %>% 
    drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "bt_max"))
  
  # time this, highly variable 
  start_time <- Sys.time()
  
  gam_tw <- gam (kg_tot  ~ bormicon_region + s(surface_temp) + s(bottom_temp) + s(tow_depth_begin) + s(sst_max) + s(sst_min) + s(bt_max),
                 family = tw(link = "log"),
                 data = spp_data_train,
                 method = "REML")
  
  # gam_tw <- gam (log(kg_tot + 1) ~ s(rugosity) + s(surface_temp) + s(bottom_temp) + s(tow_depth_begin) + s(sst_max) + s(sst_min) + s(bt_max),
  #                family = tw(),
  #                data = spp_data_train,
  #                method = "REML")

  
  # gam_tw <- gam (kg_tot  ~ s(rugosity) + s(surface_temp) + s(bottom_temp) + s(tow_depth_begin) + s(sst_max) + s(sst_min) + s(bt_max),
  #                family = tw(link = "log"),
  #                data = spp_data_train,
  #                method = "REML")
  
  end_time <- Sys.time()
  
  save (gam_tw,  file = paste0("Models/", directory, "/", sci_name, ".Rdata"))
  
  predict_tw <- predict.gam (gam_tw, spp_data_test, type = "response")
  
  #MAE_tw <- mean(abs(predict_tw - log(spp_data_test$kg_tot + 1)))
  MAE_tw <- mean(abs(predict_tw - spp_data_test$kg_tot))
  
  
  gam_naive <- gam (kg_tot ~ bormicon_region +  s(tow_depth_begin),
                 family = tw(link = "log"),
                 data = spp_data_train,
                 method = "REML")
  
  # gam_naive <- gam (log(kg_tot + 1) ~ s(rugosity) +  s(tow_depth_begin),
  #                family = tw(),
  #                data = spp_data_train,
  #                method = "REML")
  
  # gam_naive <- gam (kg_tot ~ s(rugosity) +  s(tow_depth_begin),
  #                family = tw(link = "log"),
  #                data = spp_data_train,
  #                method = "REML")
  
  predict_naive <- predict.gam (gam_naive, spp_data_test, type = "response")
  
  #MAE_naive <- mean (abs (predict_naive - log (spp_data_test$kg_tot + 1)))
  MAE_naive <- mean (abs (predict_naive - spp_data_test$kg_tot))
  
  # E1 <- predict_naive - log (spp_data_test$kg_tot + 1)
  # E2 <- predict_tw - log(spp_data_test$kg_tot + 1)
  E1 <- predict_naive - spp_data_test$kg_tot
  E2 <- predict_tw - spp_data_test$kg_tot
  
  # need complete cases; remove NA
  E1_cc <- E1 [ which (!is.na(E1) & !is.na(E2))]
  E2_cc <- E2 [ which (!is.na(E1) & !is.na(E2))]
  
  dm_gam <- dm.test (E1_cc, E2_cc, h = 1, power = 1, alternative = "greater")
  
  MASE = MAE_tw / MAE_naive
  
  DM_GAM_p = dm_gam$p.value
  
  stats_tw <- data.frame (
    species = sci_name,
    AIC = round(AIC(gam_tw), 2),
    dev_ex = round (summary (gam_tw)$dev.expl, 2),
    MAE = round (MAE_tw, 2),
    MASE_suit = ifelse (MASE < 1 & DM_GAM_p < 0.05, 1, 0),
    time = end_time - start_time
  )
  
}



# ==================
# Run functions on models ----
# ==================

# rugosity ----
dir.create ("Models/Rug_tw_LL")

# first did 2:9
rug_tw_ls <- expand.grid (sci_name = borm_spp$sci_name_underscore[17:20],
                           directory = "Rug_tw_LL") %>%
  as.list()

system.time(rug_tw_df3 <- pmap_dfr (rug_tw_ls , fit_tw_fun)); beep()
system.time(mk_rug <- fit_tw_fun (sci_name = "Microstomus_kitt", directory = "Rug_tw_LL"))
system.time(pp_rug <- fit_tw_fun (sci_name = "Pleuronectes_platessa", directory = "Rug_tw_LL"))

save (rug_tw_df, file = "Models/tw_rug_tmp_2-9.RData")
save (rug_tw_df2, file = "Models/tw_rug_tmp_11-16.RData")
rug_LL_20 <- rbind (mk_rug, rug_tw_df, pp_rug, rug_tw_df2, rug_tw_df3)
row.names (rug_LL_20) <- NULL
save (rug_LL_20, file = "Models/tw_rug_20spp.RData")

# rugosity lognormal
rug_tw_ls <- expand.grid (sci_name = borm_spp$sci_name_underscore[1:20],
                          directory = "Rug_tw") %>%
  as.list()

system.time(rug_tw_lognorm_df <- pmap_dfr (rug_tw_ls , fit_tw_fun)); beep()
row.names(rug_tw_lognorm_df) <- NULL
rug_tw_lognorm_df2 <- rug_tw_lognorm_df
save (rug_tw_lognorm_df, file = "Models/tw_rug_lognorm_20spp.RData")

# full bormicon----
dir.create ("Models/Borm_14_tw")
test_spp <- c("Gadus_morhua", "Melanogrammus_aeglefinus", "Anarhichas_lupus", "Lophius_piscatorius", "Merlangius_merlangus")

load ("Models/spp_Smooth_latlon.RData")

# filter out species that break for bormicon region
borm_spp <- filter (spp_Smooth_latlon, 
                    ! sci_name_underscore %in% c("Myoxocephalus_scorpius", "Amblyraja_hyperborea", "Rajella_fyllae,", "Lumpenus_lampretaeformis", "Ammodytes_marinus", "Mallotus_villosus", "Micromesistius_poutassou", "Argentina_silus", "Scomber_scombrus", "Clupea_harengus"))

borm_remain <- borm_spp %>%
  filter (!sci_name_underscore %in% c("Gadus_morhua", "Melanogrammus_aeglefinus", "Anarhichas_lupus", "Lophius_piscatorius", "Merlangius_merlangus", "Sebastes_mentella", "Cyclopterus_lumpus", "Microstomus_kitt", "Hippoglossoides_platessoides","Limanda_limanda", "Pollachius_virens", "Reinhardtius_hippoglossoides"))

lognorm_ls <- expand.grid (sci_name = borm_remain$sci_name_underscore[1:6],
                                directory = "Borm_14_tw") %>%
  as.list()

system.time(lognorm_df <- pmap_dfr (lognorm_ls, fit_tw_fun)); beep()

lognorm_ls2 <- expand.grid (sci_name = borm_remain$sci_name_underscore[7:9],
                           directory = "Borm_14_tw") %>%
  as.list()

system.time(lognorm_df2 <- pmap_dfr (lognorm_ls2, fit_tw_fun)); beep()

lognorm_ls3 <- expand.grid (sci_name = borm_remain$sci_name_underscore[10:12],
                           directory = "Borm_14_tw") %>%
  as.list()

system.time(lognorm_df3 <- pmap_dfr (lognorm_ls3, fit_tw_fun)); beep()

# temporary save
load ("Models/Tweedie_test_spp.RData")
tw_test_df_suit <- tw_test_df %>%
  mutate(MASE_suit = ifelse (MASE < 1 & DM_GAM_p < 0.05, 1, 0), 
         # fix weird time situation
        time = ifelse (time < 10, time * 60, time)) %>%
  dplyr::select (species, AIC, dev_ex, MAE, MASE_suit, time)

lognorm_df_tmp <- rbind (tw_test_df_suit, lognorm_df, lognorm_df2, lognorm_df3)
row.names (lognorm_df_tmp) <- NULL
save (lognorm_df_tmp, file = "Models/Borm_14_tw_lognorm_performance_24spp.RData")

# compare with log link----
dir.create ("Models/Borm_14_tw_loglink")

fit_tw_fun ("Gadus_morhua", directory = "Borm_14_tw_loglink")

loglink_test_ls <- list (sci_name = borm_spp$sci_name_underscore[1:10], 
                                directory = "Borm_14_tw_loglink") %>%
  as.list()

system.time(loglink_df <- pmap_dfr (loglink_test_ls, fit_tw_fun)); beep() #3920.02, 19338.59  for 10


save(loglink_df, file = "Models/tweedie_LL_10spp.RData")

# redo 11-20
loglink_test_ls2 <- list (sci_name = borm_spp$sci_name_underscore[11:20], 
                         directory = "Borm_14_tw_loglink") %>%
  as.list()

system.time(loglink_df2 <- pmap_dfr (loglink_test_ls2, fit_tw_fun)); beep() #3920.02, 19338.59  for 10, 1651.73  for 11-20
row.names(loglink_df2) <- NULL
row.names(loglink_df) <- NULL

loglink_df20 <- rbind (loglink_df, loglink_df2)

save(loglink_df20, file = "Models/tweedie_LL_20spp.RData")


loglink_test_ls <- list (sci_name = borm_remain$sci_name_underscore[1:6], 
                         directory = "Borm_14_tw_loglink") %>%
  as.list()

system.time(loglink_df2 <- pmap_dfr (loglink_test_ls, fit_tw_fun)); beep()

save (loglink_df, file = "Models/tw_LL_6spp_MASEvalues.RData")

ll_df_suit <- loglink_df %>%
  mutate(MASE_suit = ifelse (MASE < 1 & DM_GAM_p < 0.05, 1, 0), 
         # fix weird time situation
         time = ifelse (time < 10, time * 60, time)) %>%
  dplyr::select (species, AIC, dev_ex, MAE, MASE_suit, time)

loglink_df_tmp <- rbind (ll_df_suit, loglink_df2)
row.names (loglink_df_tmp) <- NULL
save (loglink_df_tmp, file ="Models/tw_LL_11spp.RData")

# continue with ones I can compare with lognormal
lognorm_spp <- list.files ("Models/Borm_14_tw")

ll_files <- list.files("Models/Borm_14_tw_loglink/")
ll_todo <-lognorm_spp_files[which (!lognorm_spp_files %in% ll_files)]

scan_fun <- function(x){
  sc <- scan(what = "", text = x, sep = ".")[1]
  return (sc)
}

ll_todo_spp <- map_chr (ll_todo, scan_fun)

loglink_remain_ls <- list (sci_name = ll_todo_spp, 
                         directory = "Borm_14_tw_loglink") %>%
  as.list()

system.time(loglink_df3 <- pmap_dfr (loglink_remain_ls, fit_tw_fun)); beep()
row.names (loglink_df3) <- NULL

load ("Models/tw_LL_11spp.RData")

ll_24 <- rbind (loglink_df_tmp, loglink_df3)
save (ll_24, file = "Models/tw_LL_24spp.RData")

# compare performance 
gam_2step <- read_csv ("../Models/GAM_performance_Borm_14_alltemp_NOFAKEZEROS.csv") %>%
  filter (species %in% c("Gadus_morhua", "Melanogrammus_aeglefinus", "Anarhichas_lupus", "Lophius_piscatorius", "Merlangius_merlangus")) %>%
  dplyr::select (species, PA_dev, PA_AIC, LB_dev, LB_AIC, MAE_gam, MASE_GAM, DM_GAM_p) %>%
  mutate (MASE_suit = ifelse (MASE_GAM < 1 & DM_GAM_p < 0.05, 1, 0), .keep = "unused") %>%
  mutate (model = "Two-step")




# CONVERT TO DF AND SAVE
system.time(test_tw_ls <- lapply (test_spp, fit_tw_fun, directory = "Borm_14_tw")); beep()
test_tw_df <- as.data.frame (test_tw_ls, ncol = 7) # this doesn't work
test_tw_df_mat <- as.data.frame (test_tw_df, ncol = 7)
test_docall <- do.call (rbind, test_tw_ls) # works, but time is messed up

time_Test <- c(test_tw_ls[[1]][7], test_tw_ls[[2]][7],test_tw_ls[[3]][7],test_tw_ls[[4]][7],test_tw_ls[[5]][7])
test_rbind <- rbind (test_tw_ls[[1]], test_tw_ls[[5]][7])

test_docall2 <- test_docall[, 1:4]
test_docall2$Time <- time_Test
test_docall2$Units <- c("sec", "sec", "sec", "min", "min")

test_docall$units <- c("sec", "sec", "sec", "min", "min")
docall2 <- do.call (rbind, test_tw_ls2)
docall2$units <- c("min", "min", "hour", "sec", "min", "sec", "sec")



tw_test_df <- rbind (test_docall, docall2)
rownames(tw_test_df) <- NULL
save (tw_test_df, file = "Models/tweedie_test_spp.RData")

# try another round of spp
test_spp2 <- c( "Sebastes_mentella", "Cyclopterus_lumpus", "Microstomus_kitt", "Hippoglossoides_platessoides","Limanda_limanda", "Pollachius_virens", "Reinhardtius_hippoglossoides")
system.time(test_tw_ls2 <- lapply (test_spp2, fit_tw_fun, directory = "Borm_14_tw")); beep()

# maybe should be using pmap_dfr??

# try a predict, going back and forth with step2 ----
stack_names <- c("bormicon_region", "surface_temp", "bottom_temp", "tow_depth_begin", "sst_max", "sst_min", "bt_max")
CM <- "gfdl"
scenario = 585
month_index <- 553

stack_1 <- stack (borm_r, sst_r, bt_r, depth_r, sst_max_r, sst_min_r, bt_max_r)
names (stack_1) <- stack_names

system.time(predict_tw_24 <- raster::predict (clip_1, b_tw, type = "response")) #0.29 s

plot (predict_tw_24) # looks okay!!


#
system.time(
  b_tw <- gam (log (kg_tot + 1) ~ bormicon_region + s(surface_temp) + s(bottom_temp) + s(tow_depth_begin) + s(sst_max) + s(sst_min) + s(bt_max),
               family = tw(),
               data = filter (mfri_abun_full_borm, species == 24),
               method = "REML")
)# took 3.5 hours with data = filter and log(kg_tot +1)


abun_14 <- mfri_abun_full_borm %>%
  filter (species == 14)

system.time(
  b_tw <- gam (log (kg_tot + 1) ~ bormicon_region + s(surface_temp) + s(bottom_temp) + s(tow_depth_begin) + s(sst_max) + s(sst_min) + s(bt_max),
               family = tw(),
               data = abun_14,
               method = "REML")
); beep()
# really fast with more common spp, much slower with 24, 14


abun_cod <- mfri_abun_full_borm %>%
  filter (species == 1) %>% 
  drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "bt_max"))

system.time(
  b_tw_cod <- gam (log (kg_tot + 1) ~ bormicon_region + s(surface_temp) + s(bottom_temp) + s(tow_depth_begin) + s(sst_max) + s(sst_min) + s(bt_max),
                   family = tw(),
                   data = abun_cod,
                   method = "REML")
)
# this was way better, 77 s. 

system.time(
  b_tw_cod_loglink <- gam (kg_tot ~ bormicon_region + s(surface_temp) + s(bottom_temp) + s(tow_depth_begin) + s(sst_max) + s(sst_min) + s(bt_max),
                           family = tw(link = "log"),
                           data = abun_cod,
                           method = "REML")
); beep() # this was faster, 40s, but worse dev expl and AIC


#####################################
# Try negbin

fit_nb_fun <- function (sci_name, directory) {
  
  spp_id <- as.numeric(as.character(spp_list$Spp_ID[which (spp_list$sci_name_underscore == sci_name)]))# this fixes weird factor problem
  
  spp_data_train <- mfri_abun_full_borm %>%
    filter (species == spp_id, 
            year <= 2013) %>% 
    drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "bt_max"))
  
  spp_data_test <- mfri_abun_full_borm %>%
    filter (species == spp_id, 
            year > 2013) %>% 
    drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "bt_max"))
  
  # time this, highly variable 
  start_time <- Sys.time()
  
  gam_nb <- gam (kg_tot ~ bormicon_region + s(surface_temp) + s(bt_min) + s(tow_depth_begin) + s(sst_max) + s(sst_min) + s(bt_max),
                 family = nb(link = "log"),
                 data = spp_data_train,
                 method = "REML") #8:54
  
  end_time <- Sys.time()
  
  save (gam_nb,  file = paste0("Models/", directory, "/", sci_name, ".Rdata"))
  
  predict_nb <- predict.gam (gam_nb, spp_data_test, type = "response")
  
  MAE_nb <- mean(abs(predict_nb - spp_data_test$kg_tot))
  
  
  gam_naive <- update (gam_nb, formula = as.formula("kg_tot ~ bormicon_region +  s(tow_depth_begin)"))
  
  predict_naive <- predict.gam (gam_naive, spp_data_test, type = "response")

  MAE_naive <- mean (abs (predict_naive - spp_data_test$kg_tot))
  
  E1 <- predict_naive - spp_data_test$kg_tot 
  E2 <- predict_nb - spp_data_test$kg_tot 
  
  # need complete cases; remove NA
  E1_cc <- E1 [ which (!is.na(E1) & !is.na(E2))]
  E2_cc <- E2 [ which (!is.na(E1) & !is.na(E2))]
  
  dm_gam <- dm.test (E1_cc, E2_cc, h = 1, power = 1, alternative = "greater")
  
  MASE = MAE_nb / MAE_naive
  
  DM_GAM_p = dm_gam$p.value
  
  stats_tw <- data.frame (
    species = sci_name,
    AIC = round(AIC(gam_nb), 2),
    dev_ex = round (summary (gam_nb)$dev.expl, 2),
    MAE = round (MAE_nb, 2),
    MASE_suit = ifelse (MASE < 1 & DM_GAM_p < 0.05, 1, 0),
    time = end_time - start_time
  )
  
}

dir.create ("Models/Borm_14_nb")

tmp <- fit_nb_fun (sci_name = "Gadus_morhua", directory = "Borm_14_nb")

borm_nb_ls <- expand.grid (sci_name = borm_spp$sci_name_underscore[1:10],
                          directory = "Borm_14_nb") %>%
  as.list()

system.time(borm_nb_df <- pmap_dfr (borm_nb_ls , fit_nb_fun)); beep()

borm_nb_ls2 <- expand.grid (sci_name = borm_spp$sci_name_underscore[11:20],
                           directory = "Borm_14_nb") %>%
  as.list()

system.time(borm_nb_df2 <- pmap_dfr (borm_nb_ls2 , fit_nb_fun)); beep()

row.names(borm_nb_df) <- NULL
row.names(borm_nb_df2) <- NULL

borm_nb_20 <- rbind (borm_nb_df, borm_nb_df2)
save (borm_nb_20, file = "Models/nb_LL_20spp.Rdata")