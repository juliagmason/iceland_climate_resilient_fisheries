## Predict.gam outputs
# 5/7/2021
# JGM

library (mgcv)

# predict_gam() instead of predict.gam()??
# https://cran.r-project.org/web/packages/tidymv/vignettes/predict-gam.html
library (tidymv) # my gams are too big??

load ("Models/Borm_14_alltemp/Gadus_morhua_PA.Rdata")

summary (gam_PA)

# use vague middle values for 2070, mean dev from predictor database, what seems like reasonable depth
newdata <- data.frame (
  surface_temp = 8.5,
  bottom_temp = 3, 
  tow_depth_begin = 250,
  sst_dev = 0.5,
  bt_dev = 0.9, 
  sst_max = 10, 
  sst_min = 7,
  bt_max = 9,
  bormicon_region = c(101, 102, 103, 104, 105, 106, 107, 108, 109, 111, 113, 114)
)


plot_gm_predict_fun_PA <- function (sci_name) {
  
  load (paste0("Models/Borm_14_alltemp/", sci_name, "_PA.Rdata"))  
  
  # predict gam
  gm_predict <- predict.gam (gam_PA, newdata, se.fit = TRUE, type = "response")
  
  # reshape to data frame to plot
  gm_df <- data.frame (
    Bormicon = as.factor(c(101, 102, 103, 104, 105, 106, 107, 108, 109, 111, 113, 114)),
    fit = gm_predict[[1]],
    se = gm_predict[[2]]
  )
  
  
  ggplot (gm_df, aes (x = Bormicon, y = fit)) +
    geom_point() +
    geom_errorbar (aes(ymin = fit - 2*se, ymax = fit + 2*se)) +
    theme_bw() +
    ggtitle (sci_name) +
    theme (
      axis.text = element_text (size = 12),
      axis.title = element_text (size = 14),
      plot.title = element_text (size = 16)
    )
}


# suitable spp
load ("Models/spp_Borm_suit.RData")

pdf ("Figures/Borm14_PA_gam_predict_Bormicon.pdf", width = 11, height = 8.5)
lapply (borm_suit, plot_gm_predict_fun_PA)
dev.off()

# LB
plot_gm_predict_fun_LB <- function (sci_name) {
  
  load (paste0("Models/Borm_14_alltemp/", sci_name, "_LB.Rdata"))  
  
  # predict gam
  gm_predict <- predict.gam (gam_LB, newdata, se.fit = TRUE, type = "response")
  
  # reshape to data frame to plot
  gm_df <- data.frame (
    Bormicon = as.factor(c(101, 102, 103, 104, 105, 106, 107, 108, 109, 111, 113, 114)),
    fit = gm_predict[[1]],
    se = gm_predict[[2]]
  )
  
  
  ggplot (gm_df, aes (x = Bormicon, y = fit)) +
    geom_point() +
    geom_errorbar (aes(ymin = fit - 2*se, ymax = fit + 2*se)) +
    theme_bw() +
    ggtitle (sci_name) +
    theme (
      axis.text = element_text (size = 12),
      axis.title = element_text (size = 14),
      plot.title = element_text (size = 16)
    )
}

pdf ("Figures/Borm14_LB_gam_predict_Bormicon.pdf", width = 11, height = 8.5)
lapply (borm_suit, plot_gm_predict_fun_LB)
dev.off()  


# definitely weird things going on with fake zeros. 
