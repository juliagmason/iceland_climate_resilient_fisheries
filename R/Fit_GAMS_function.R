# Function to run GAMs on full data for all species
# 9/16/2020

# JGM

library (mgcv)
library (tidyverse)


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


# Function to run GAM, full data, all species. 

fit_gam_fun <- function (model_terms, directory, spp_names) {
  
  for (i in 1:length(spp_names)) {
    
    # match species sci name to ID code in spp_list
    spp_id <- as.numeric(as.character(spp_list$Spp_ID[which (spp_list$sci_name_underscore == spp_names[i])])) # this fixes weird factor problem
    
    print(paste(i, spp_names[i], spp_id, Sys.time()))  
    
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
    
    # fit on training data
    # log biomass model:
    formula_LB <- as.formula (paste0 ("kg_log ~", model_terms))
    gam_LB <- gam (formula_LB,
                   family = "gaussian", 
                   data = spp_B,
                   gamma = gamma_B)
    
    # Presence-absence model
    formula_PA <- as.formula (paste0 ("Presence ~", model_terms))
    gam_PA <- gam (formula_PA,
                   family = "binomial", 
                   data = spp_PA,
                   gamma = gamma_PA)
    
    # save models in appropriate folder
    save (gam_PA, file = paste0("Models/", directory, "/", spp_names[i], "_PA.Rdata"))
    save (gam_LB, file = paste0("Models/", directory, "/", spp_names[i], "_LB.Rdata"))
  } # end for loop
  
} # end function 


# fit on temp/depth model
# I already fit 25 spp on temp_depth with all temp variables (now excluding bt_min), but will overwrite. Directory would be Depth_Temp
td_model_terms <- "s(surface_temp) + s(bottom_temp) + s(tow_depth_begin) +  s(sst_max) + s(sst_min) + s(bt_max)"

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

fit_gam_fun(model_terms = td_model_terms, 
            directory = "Depth_Temp", 
            spp_names = td_spp$sci_name_underscore)

# crashed on pandalus
fit_gam_fun(model_terms = td_model_terms, 
            directory = "Depth_Temp", 
            spp_names = td_spp$sci_name_underscore[68:nrow(td_spp)])
