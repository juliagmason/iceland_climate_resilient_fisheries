# compare observations and predicitons between observed, tweedie, nb

library (mgcv)
library (tidyverse)
library (gridExtra)

compare_distrib_fun <-  function(sci_name){
  # neg binom
  load (paste0("Models/Rug_nb/", sci_name, ".Rdata"))
  
  # tweedie
  load (paste0("Models/Rug_tw_LL/", sci_name, ".Rdata"))
  
  spp_id <- as.numeric(as.character(borm_spp$Spp_ID[which (borm_spp$sci_name_underscore == sci_name[i])]))
  
  spp_data_test <- mfri_abun_full %>%
    filter (species == spp_id, 
            year > 2013) %>% 
    drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "bt_max"))
  
  predict_nb <- predict.gam (gam_nb, spp_data_test, type = "response")
  
  predict_tw <- predict.gam (gam_tw, spp_data_test, type = "response")
  
  predict_stack <- data.frame (
    values = c(spp_data_test$kg_tot, predict_nb, predict_tw), model = c(rep("Observed", nrow (spp_data_test)), rep ("Neg binom", length (predict_nb)), rep("Tweedie", length (predict_tw))))
  
  
  # gg_raw <- ggplot (predict_stack) + geom_density ( aes (x = values,col = model), alpha = 0.5, binwidth = 3) + theme_bw() + theme (legend.position = "top", legend.title = element_blank())
  # 
  # gg_log <- ggplot (predict_stack) + geom_density ( aes (x = log(values),col = model), alpha = 0.5, binwidth = 0.1) + theme_bw() + theme (legend.position = "none")
  
  gg_raw <- ggplot (predict_stack) + geom_histogram ( aes (x = values,fill = model), alpha = 0.5, binwidth = max (predict_stack$values)/100) + theme_bw() + theme (legend.position = "top", legend.title = element_blank())
  
  gg_log <- ggplot (predict_stack) + geom_histogram ( aes (x = log(values),fill = model), alpha = 0.5, binwidth = max (log(predict_stack$values))/10) + theme_bw() + theme (legend.position = "none")
  
  grid.arrange (gg_raw, gg_log, nrow = 1, top = sci_name)
  
}


compare_distrib_fun("Microstomus_kitt")

pdf (file = "Figures/Compare_NB_TW_predictions_density.pdf")
lapply (borm_spp$sci_name_underscore, compare_distrib_fun)
dev.off()

pdf (file = "Figures/Compare_NB_TW_predictions_histogram.pdf")
lapply (borm_spp$sci_name_underscore, compare_distrib_fun)
dev.off()





ggplot (predict_stack) + geom_density ( aes (x = log(values),col = model))

stack_xy <- cbind (spp_data_test$kg_tot, predict_nb, predict_tw)
colnames (stack_xy) <- c("Observed", "NegBinom", "Tweedie")

ggplot (aes (x = Observed, y = NegBinom), data = stack_xy) +
  geom_point()

resid_xy <- data.frame (
  Tw_diff = predict_tw - spp_data_test$kg_tot,
  Nb_diff = predict_nb - spp_data_test$kg_tot
)

plot (resid_xy$Tw_diff, resid_xy$Nb_diff)

ggplot (resid_xy) +
  geom_point (aes (x = Tw_diff, y = Nb_diff))


preds_diff <- predict_tw - predict_nb
hist(preds_diff, breaks = 100)
