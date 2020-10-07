# Function to run GAMs on full data for all species
# 9/16/2020

# JGM

library (mgcv)
library (tidyverse)


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

fit_gam_fun <- function (model_terms, directory, spp_names) {
  
  # empty data frame for saving variable importance info
  var_imp_all_spp <- data.frame()

  # empty data frame for saving model summary statistics
  model_stats <- data.frame()
  
  for (i in 1:length(spp_names)) {
    
    # match species sci name to ID code in spp_list
    spp_id <- as.numeric(as.character(spp_list$Spp_ID[which (spp_list$sci_name_underscore == spp_names[i])])) # this fixes weird factor problem
    
    print(paste(i, spp_names[i], spp_id, Sys.time()))  
   #  
    # set up predictor data for selected species ----

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


    # Fit GAM ----
    set.seed(10) # needed for gamma maybe?


    #fit on training data
   # log biomass model:
    formula_LB <- as.formula (paste0 ("kg_log ~", (paste (model_terms, collapse = " + "))))
    gam_LB <- gam (formula_LB,
                   family = "gaussian",
                   data = spp_B,
                   gamma = gamma_B)

    # Presence-absence model
    formula_PA <- as.formula (paste0 ("Presence ~", (paste (model_terms, collapse = " + "))))
    gam_PA <- gam (formula_PA,
                   family = "binomial",
                   data = spp_PA,
                   gamma = gamma_PA)


    # # already fit models, just load
    # load (file.path("Models", directory, paste0(spp_names[i], "_PA.Rdata"))) # drop _full for depth_temp and bormicon
    # load (file.path("Models", directory, paste0(spp_names[i], "_LB.Rdata")))
    

    stats_df <- data.frame (
      species = rep(spp_names[i], 2),
      GAM = c ("PA", "LB"),
      dev_exp = c (summary (gam_PA)$dev.expl,
                   summary (gam_LB)$dev.expl
      ),
      AIC = c (AIC (gam_PA),
               AIC (gam_LB)

      )
      )

    # save models in appropriate folder
    save (gam_PA, file = paste0("Models/", directory, "/", spp_names[i], "_PA.Rdata"))
    save (gam_LB, file = paste0("Models/", directory, "/", spp_names[i], "_LB.Rdata"))
    
    # Calculate variable importance ----
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
      df <- data.frame (species = rep(spp_names[i], 2),
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

    var_imp_all_spp <- rbind (var_imp_all_spp, var_imp)

    #rbind model stats to full data frame
    model_stats <- rbind (model_stats, stats_df)

  } # end species for loop
  
  
  write.csv (var_imp_all_spp, file = paste0 ("Models/var_imp_", directory, ".csv"), row.names = FALSE)
  write.csv (model_stats, file = paste0("Models/summary_table_", directory, ".csv"), row.names = FALSE)
  
} # end function 


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


# smooth lat/lon but with tensor spline, folliwng pat convo
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

dir.create ("Models/Borm_14_alltemp")
load ("Models/spp_Smooth_latlon.RData")
Borm_14_alltemp_terms <- c ("bormicon_region", "s(surface_temp)", "s(bottom_temp)", "s(tow_depth_begin)", "s(sst_dev)", "s(bt_dev)", "s(sst_max)",  "s(sst_min)", "s(bt_max)")

# broke for 45 myoxechpalus, in all circumstances
borm_spp <- filter (spp_Smooth_latlon, 
                    sci_name_underscore != "Myoxocephalus_scorpius")

fit_gam_fun(model_terms = Borm_14_alltemp_terms, 
            directory = "Borm_14_alltemp", 
            spp_names = borm_spp$sci_name_underscore)
