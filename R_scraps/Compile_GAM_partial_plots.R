## Compile GAM partial plots
# 5/7/21
# JGM

library (mgcViz)
## https://mfasiolo.github.io/mgcViz/articles/mgcviz.html

load ("Models/spp_Borm_suit.RData")

# Plot all PA partials on a page. Can't figure out how to do an overall title
plot_gam_partial_PA <- function (sci_name, directory) {
  load (file.path("Models", directory, paste0(sci_name, "_PA.Rdata")))
  b <- getViz(gam_PA)
  
  print(plot(b, allTerms = T) + ggtitle(paste(sci_name, "Pres/Abs", sep = " ")), pages = 1)
  
}


pdf ("Figures/Borm_14_alltemp_PA_partials.pdf", width = 11, height = 8.5)
lapply (borm_suit, plot_gam_partial_PA)
dev.off()

pdf ("Figures/Rug3m_PA_partials.pdf", width = 11, height = 8.5)
lapply (rug_spp, plot_gam_partial_PA, directory = "rugos3m")
dev.off()


# Same with log biomass
plot_gam_partial_LB <- function (sci_name, directory) {
  load (file.path("Models", directory, paste0(sci_name, "_LB.Rdata")))
  b <- getViz(gam_LB)
  
  print(plot(b, allTerms = T) + ggtitle(paste(sci_name, "log Biomass", sep = " ")), pages = 1)
  
}

pdf ("Figures/Borm_14_alltemp_LB_partials.pdf", width = 11, height = 8.5)
lapply (borm_suit, plot_gam_partial_LB)
dev.off()

# Tw partial plots
plot_gam_partial_tw <- function (sci_name, directory) {
  load (file.path("Models", directory, paste0(sci_name, ".Rdata")))
  b <- getViz(gam_tw)
  
  print(plot(b, allTerms = T) + ggtitle(sci_name), pages = 1)
  
}

# MB partial plots
plot_gam_partial_nb <- function (sci_name, directory) {
  load (file.path("Models", directory, paste0(sci_name, ".Rdata")))
  b <- getViz(gam_nb)
  
  print(plot(b, allTerms = T) + ggtitle(sci_name), pages = 1)
  
}

ll_files <- list.files("Models/Borm_14_tw_loglink/")

scan_fun <- function(x){
  sc <- scan(what = "", text = x, sep = ".")[1]
  return (sc)
}

ll_spp <- map_chr (ll_files, scan_fun)

# WEIRD THING SOME HAVE RUGOSITY
pdf ("Figures/Borm_14_tw_LL_partials.pdf", width = 11, height = 8.5)
lapply (ll_spp, plot_gam_partial_tw, directory = "Borm_14_tw_loglink")
dev.off()

# lognormal
# same set of spp

pdf ("Figures/Borm_14_tw_lognorm_partials.pdf", width = 11, height = 8.5)
lapply (ll_spp, plot_gam_partial_tw, directory = "Borm_14_tw")
dev.off()

# rugosity LL
rug_LL_files <- list.files ("Models/Rug_tw_LL")
rug_spp <- map_chr (rug_LL_files, scan_fun)

pdf ("Figures/Rug_tw_LL_partials.pdf", width = 11, height = 8.5)
lapply (rug_spp, plot_gam_partial_tw, directory = "Rug_tw_LL")
dev.off()

pdf ("Figures/Rug_tw_partials.pdf", width = 11, height = 8.5)
lapply (rug_spp, plot_gam_partial_tw, directory = "Rug_tw")
dev.off()



# negbin
pdf ("Figures/Rug3_btmin_partials.pdf", width = 11, height = 8.5)
lapply (borm_spp$sci_name_underscore, plot_gam_partial_nb, directory = "Rug_nb_btmin")
dev.off()

pdf ("Figures/Rug3_nb_partials.pdf", width = 11, height = 8.5)
lapply (borm_spp$sci_name_underscore, plot_gam_partial_nb, directory = "Rug_nb")
dev.off()

pdf ("Figures/Borm_nb_partials.pdf", width = 11, height = 8.5)
lapply (borm_spp$sci_name_underscore, plot_gam_partial_nb, directory = "Borm_14_nb")
dev.off()

pdf ("Figures/Borm_nb_btmin_partials.pdf", width = 11, height = 8.5)
lapply (borm_spp$sci_name_underscore, plot_gam_partial_nb, directory = "Borm_14_nb_btmin")
dev.off()

pdf ("Figures/Rug6_nb_partials.pdf", width = 11, height = 8.5)
lapply (borm_spp$sci_name_underscore, plot_gam_partial_nb, directory = "Rug6_nb")
dev.off()

pdf ("Figures/Rug1_nb_partials.pdf", width = 11, height = 8.5)
lapply (borm_spp$sci_name_underscore, plot_gam_partial_nb, directory = "Rug1_nb")
dev.off()

pdf ("Figures/Rug3_nb_partials_RMoutliers.pdf", width = 11, height = 8.5)
lapply (c("Gadus_morhua", "Melanogrammus_aeglefinus", "Sebastes_marinus", "Chimaera_monstrosa", "Rhinonemus_cimbrius", "Myctophidae", "Cottunculus_microps"), plot_gam_partial_nb, directory = "Rug_nb_RMoutliers")
dev.off()




#################################################
print (gam.check(gam_PA), pages = 1)
vis.gam(gam_PA)

library (mgcViz)
# https://mfasiolo.github.io/mgcViz/articles/mgcviz.html

b <- getViz(gam_PA)
l <- getViz (gam_LB)

o <- plot( sm(b, 1) )

o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()


print(plot(b, allTerms = T) + ggtitle("MA"), pages = 1) # Calls print.plotGam()
print(plot(l, allTerms = T), text("Three Diagrams in One Page"), pages = 1)
mtext("Three Diagrams in One Page", sid,  cex=1, line=-0.5)

check(b,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))
check(b, main = "MA") + main ("MA")
