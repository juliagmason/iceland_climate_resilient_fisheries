# Compare performance NB vs tweedie
NB_stats <- read_csv ("Models/GAM_performance_Rug_nb.csv")

TW_stats <- read_csv ("Models/GAM_performance_Rug_tw_LL.csv")

summary (TW_stats$AIC)
summary (NB_stats$AIC) # NB is lower, tw has negative??

summary (TW_stats$MAE)
summary (NB_stats$MAE) # NB is lower

summary (TW_stats$dev_ex) # TW is higher
summary (NB_stats$dev_ex)

# there must be a much easier way of taking difference of a bunch of columns....
NB_stats$species[which (!NB_stats$species %in% TW_stats$species)]

NB_stats <- NB_stats[-c(31,50),]


# take difference of AIC. NB minus TW. so if the difference is > 2, tweedie did better. if the difference is < -2, NB did better
AIC_diff <- NB_stats$AIC - TW_stats$AIC
length (which (AIC_diff > 2)) # 12 spp
length (which (AIC_diff < -2)) # 41 spp

# if answer is positive, TW is better. if negative, NB is better
MAE_diff <- NB_stats$MAE - TW_stats$MAE
length (which (MAE_diff > 0)) # 11
length (which (MAE_diff < 0)) # 16

t.test (NB_stats$MAE, TW_stats$MAE) # not significant

NB_stats$species[which (AIC_diff > 2)]
# [1] "Microstomus_kitt"             "Amblyraja_radiata"           
# [3] "Hippoglossoides_platessoides" "Trisopterus_esmarkii"        
# [5] "Anarhichas_lupus"             "Brosme_brosme"               
# [7] "Sebastes_viviparus"           "Molva_dypterygia"            
# [9] "Gaidropsarus_argentatus"      "Lycodes_gracilis"            
# [11] "Artediellus_atlanticus"       "Triglops_murrayi"

NB_stats$species[which (MAE_diff > 0)]
# [1] "Melanogrammus_aeglefinus"  "Gadus_morhua"             
# [3] "Trisopterus_esmarkii"      "Brosme_brosme"            
# [5] "Limanda_limanda"           "Hippoglossus_hippoglossus"
# [7] "Lophius_piscatorius"       "Eutrigla_gurnardus"       
# [9] "Anarhichas_denticulatus"   "Bathyraja_spinicauda"     
# [11] "Chimaera_monstrosa"

NB_stats$species[which (MAE_diff < 0)]
# [1] "Microstomus_kitt"             "Amblyraja_radiata"           
# [3] "Glyptocephalus_cynoglossus"   "Hippoglossoides_platessoides"
# [5] "Merlangius_merlangus"         "Pleuronectes_platessa"       
# [7] "Anarhichas_lupus"             "Pollachius_virens"           
# [9] "Sebastes_marinus"             "Reinhardtius_hippoglossoides"
# [11] "Sebastes_viviparus"           "Sebastes_mentella"           
# [13] "Squalus_acanthias"            "Lycodes_eudipleurostictus"   
# [15] "Lycodes_reticulatus"          "Coryphaenoides_rupestris"   

# join? stack
stats_compare <- rbind (NB_stats, TW_stats)
stats_compare$model <- c(rep ("NB", nrow(NB_stats)), rep ("TW", nrow (TW_stats)))

diff_fun  <- function (x, y) {
  return (y - x)
}                      
                        

stats_compare %>%
  group_by (model) %>%
  summarise_if(is.numeric, diff_fun)
