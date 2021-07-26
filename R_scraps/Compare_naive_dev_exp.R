# Compare naive and full deviance explained slightly more legitimately??

# https://r.789695.n4.nabble.com/variance-explained-by-each-term-in-a-GAM-td836513.html

## calculate proportions deviance explained...
# (deviance(b1)-deviance(b))/deviance(b0) ## prop explained by s(x2)
# (deviance(b2)-deviance(b))/deviance(b0) ## prop explained by s(x1)
library (mgcv)
library(tidyverse)


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




# Function -----

naive_prop_dev_exp_fun <- function (sci_name){
  
  load (paste0("Models/Rug_nb/", sci_name, ".Rdata"))
  
  # input data
  spp_id <- as.numeric(as.character(spp_list$Spp_ID[which (spp_list$sci_name_underscore == sci_name)]))# this fixes weird factor problem
  
  spp_data_train <- mfri_abun_full_borm %>%
    filter (species == spp_id, 
            year <= 2013) %>% 
    drop_na (c("tow_depth_begin", "surface_temp", "bottom_temp", "sst_max", "bt_max"))
  
  # smoother terms
  full_sp <- gam_nb$sp
  
  naive_sp <- full_sp[c("s(rugosity)", "s(tow_depth_begin)")]
  
  
  gam_naive <- update (gam_nb, formula = as.formula("kg_tot ~ s(rugosity) +  s(tow_depth_begin)"), 
                       sp = naive_sp)
  
  temp_sp <- full_sp[c("s(surface_temp)", "s(bottom_temp)", "s(sst_max)", "s(sst_min)", "s(bt_max)")]
  
  gam_temp <- update (gam_nb, formula = as.formula("kg_tot ~  s(surface_temp) + s(bottom_temp) + 
    s(sst_max) + s(sst_min) + s(bt_max)"),
                      sp = temp_sp)
  
  gam_0 <- update (gam_nb, formula = as.formula ("kg_tot ~ 1"))
  
  dev_df <- data.frame (
    species = sci_name,
    exp_full = summary (gam_nb)$dev.expl,
    exp_naive = summary (gam_naive)$dev.expl,
    exp_temp = summary (gam_temp)$dev.expl,
    dev_0 = summary (gam_0)$dev.expl,
    deviance_full = deviance(gam_nb),
    deviance_naive = deviance(gam_naive),
    deviance_temp = deviance(gam_temp),
    deviance_0 = deviance(gam_0)) %>%
    mutate(
      prop_temp = (deviance_naive - deviance_full)/deviance_0,
      prop_static = (deviance_temp - deviance_full)/deviance_0
    )
    
}


load ("Models/spp_Smooth_latlon.RData")

# filter out species that break for bormicon region
borm_spp <- filter (spp_Smooth_latlon, 
                    ! sci_name_underscore %in% c("Myoxocephalus_scorpius", "Amblyraja_hyperborea", "Rajella_fyllae,", "Lumpenus_lampretaeformis", "Ammodytes_marinus", "Mallotus_villosus", "Micromesistius_poutassou", "Argentina_silus", "Scomber_scombrus", "Clupea_harengus"))
borm_55 <- borm_spp$sci_name_underscore

system.time(deviance_prop_df <- map_dfr (borm_55, naive_prop_dev_exp_fun)); beep()
save (deviance_prop_df, file = "Data/Rug_nb_naive_deviance_prop.Rdata")
tmp <- map_dfr("Microstomus_kitt", naive_prop_dev_exp_fun)
tmp <- naive_prop_dev_exp_fun("Microstomus_kitt")






set.seed(0)
n<-400
x1 <- runif(n, 0, 1)
## to see problem with not fixing smoothing parameters
## remove the `##' from the next line, and the `sp'
## arguments from the `gam' calls generating b1 and b2.
x2 <- runif(n, 0, 1) ## *.1 + x1

x3 <- runif(n, 0, 1)
f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
f3 <- function(x) exp(2*x) + 3
f <- f1(x1) + f2(x2) + f3(x3)
e <- rnorm(n, 0, 2)
y <- f + e
## fit full and reduced models...
b <- gam(y~s(x1)+s(x2))
b1 <- gam(y~s(x1),sp=b$sp[1])
b2 <- gam(y~s(x2),sp=b$sp[2])
b0 <- gam(y~1)
## calculate proportions deviance explained...
(deviance(b1)-deviance(b))/deviance(b0) ## prop explained by s(x2)
(deviance(b2)-deviance(b))/deviance(b0) ## prop explained by s(x1)

p <- gam (y ~ s(x1) + s(x2) + s(x3))
p_drop1 <- gam(y~s(x2) + s(x3),sp=p$sp[2:3])
p_drop2 <- gam(y~ s(x1) + s(x3), sp = p$sp[-2])
p_drop3 <- gam(y~ s(x1) + s(x2), sp = p$sp[1:2])
p_drop12 <- gam(y ~ s(x3), spp = p$sp[3])
p0 <- gam(y~1)


summary(p)$dev.expl # .787
(deviance(p_drop1)-deviance(p))/deviance(p0) # 0.18
cont1 <- summary(p)$dev.expl - summary(p_drop1)$dev.expl # 0.186

cont2 <- summary(p)$dev.expl - summary(p_drop2)$dev.expl # 0.41
cont3 <- summary(p)$dev.expl - summary(p_drop3)$dev.expl #0.174
summary(p)$dev.expl #0.7
.23 + .44 + .003, 0.673

summary(p)$dev.expl - summary(p_drop12)$dev.expl # 0.61
cont1 + cont2 # 0.59. so p drop12 is also including the intercept maybe?
