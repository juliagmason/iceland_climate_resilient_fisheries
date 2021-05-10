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

