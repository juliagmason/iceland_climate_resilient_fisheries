## Compile GAM partial plots
# 5/7/21
# JGM

library (mgcViz)
## https://mfasiolo.github.io/mgcViz/articles/mgcviz.html

load ("Models/spp_Borm_suit.RData")

# Plot all PA partials on a page. Can't figure out how to do an overall title
plot_gam_partial_PA <- function (sci_name) {
  load (paste0("Models/Borm_14_alltemp/", sci_name, "_PA.Rdata"))
  b <- getViz(gam_PA)
  
  print(plot(b, allTerms = T) + ggtitle(paste(sci_name, "Pres/Abs", sep = " ")), pages = 1)
  
}


pdf ("Figures/Borm_14_alltemp_PA_partials.pdf", width = 11, height = 8.5)
lapply (borm_suit, plot_gam_partial_PA)
dev.off()


# Same with log biomass
plot_gam_partial_LB <- function (sci_name) {
  load (paste0("Models/Borm_14_alltemp/", sci_name, "_LB.Rdata"))
  b <- getViz(gam_LB)
  
  print(plot(b, allTerms = T) + ggtitle(paste(sci_name, "log Biomass", sep = " ")), pages = 1)
  
}

pdf ("Figures/Borm_14_alltemp_LB_partials.pdf", width = 11, height = 8.5)
lapply (borm_suit, plot_gam_partial_LB)
dev.off()

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
