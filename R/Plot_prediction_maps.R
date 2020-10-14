# Plot prediction maps
# 9/22/2020
#JGM

library (raster)
library (viridis)
library (maps)
library (mapdata)
library (gridExtra)



# Function to plot historical, 245, and 585 in a panel
plot_pred_maps_fun <- function (sci_name, model_name) {
  
  # load bricks, which have a layer for each month and year
  br_hist <- brick (paste0("Models/Prediction_bricks/", sci_name, "_", model_name, "_2000_2018.grd"))
  br_245 <- brick (paste0("Models/Prediction_bricks/",  sci_name, "_", model_name,"_245_2061_2080.grd"))
  br_585 <- brick (paste0("Models/Prediction_bricks/", sci_name, "_", model_name, "_585_2061_2080.grd"))
  
  # calculate a mean, so just one layer
  mn_hist <- calc (br_hist, mean)
  mn_245 <- calc (br_245, mean)
  mn_585 <- calc (br_585, mean)
  
  overall_vals <- stack (mn_hist, mn_245, mn_585)
  
  spp_breaks <- quantile (getValues(overall_vals), probs = c(seq (0, .9, 0.1), 0.95, 0.99, 1), na.rm = TRUE)
  
  par (mfrow = c (1, 3))
  
  plot (mn_hist, 
        breaks = spp_breaks, col = viridis(length(spp_breaks) -1),
        xlim = c (-32, -3), ylim = c (60, 69),
        legend = FALSE,
        main = paste0(sci_name, " historical"))
  
  map('worldHires',add=TRUE, col='grey90', fill=TRUE)
  
  plot (mn_245, 
        breaks = spp_breaks, col = viridis(length(spp_breaks) -1),
        xlim = c (-32, -3), ylim = c (60, 69),
        legend = FALSE,
        main = paste0(sci_name, " 245"))
  map('worldHires',add=TRUE, col='grey90', fill=TRUE)
  
  plot (mn_585, 
        breaks = spp_breaks, col = viridis(length(spp_breaks) -1),
        xlim = c (-32, -3), ylim = c (60, 69),
        main = paste0(sci_name, " 585"))
  map('worldHires',add=TRUE, col='grey90', fill=TRUE)
}

plot_pred_maps_fun (sci_name = "Gadus_morhua", 
                    model_name = "Smooth_latlon")

plot_pred_maps_fun (sci_name = "Lophius_piscatorius", 
                    model_name = "Smooth_latlon")



plot_pred_maps_fun (sci_name = "Melanogrammus_aeglefinus", 
                    model_name = "Smooth_latlon")
# test maps ----

# haddock for smast seminar
png (file = "Figures/Haddock_thermpred_map.png", width = 6, height = 4, units = "in", res = 300)
plot (mn_hist, 
             breaks = spp_breaks, col = viridis(length(spp_breaks) -1),
             xlim = c (-32, -3), ylim = c (60, 69),
             legend = FALSE,
            main = "Suitable thermal habitat")
 maps::map('worldHires',add=TRUE, col='grey90', fill=TRUE)
 dev.off()
 
 # haddock vs herring, smast seminar
had_hist_br <- brick ("Models/Prediction_bricks/Melanogrammus_aeglefinus_Smooth_latlon_2000_2018.grd")
had_245_br <- brick ("Models/Prediction_bricks/Melanogrammus_aeglefinus_Smooth_latlon_245_2061_2080.grd")
had_585_br <- brick ("Models/Prediction_bricks/Melanogrammus_aeglefinus_Smooth_latlon_585_2061_2080.grd")
 
had_hist_mn <- calc (had_hist_br, mean)
had_245_mn <- calc (had_245_br, mean)
had_585_mn <- calc (had_585_br, mean)

had_vals <- stack (had_hist_mn, had_245_mn, had_585_mn) 
had_breaks <- quantile (getValues(had_vals), probs = c(seq (0, .9, 0.1), 0.95, 0.99, 1), na.rm = TRUE)
 
 her_hist_br <- brick ("Models/Prediction_bricks/Clupea_harengus_Smooth_latlon_2000_2018.grd")
 her_245_br <- brick ("Models/Prediction_bricks/Clupea_harengus_Smooth_latlon_245_2061_2080.grd")
 her_585_br <- brick ("Models/Prediction_bricks/Clupea_harengus_Smooth_latlon_585_2061_2080.grd")
 
 her_hist_mn <- calc (her_hist_br, mean)
 her_245_mn <- calc (her_245_br, mean)
 her_585_mn <- calc (her_585_br, mean)
 
 her_vals <- stack (her_hist_mn, her_245_mn, her_585_mn) 
 her_breaks <- quantile (getValues(her_vals), probs = c(seq (0, .9, 0.1), 0.95, 0.99, 1), na.rm = TRUE)
 
 
 png ("Figures/haddock_herring_SMAST_maps.png", width = 16, height = 9, units = "in", res = 300)
 par (mfrow = c (2, 3))
 plot (had_hist_mn, 
       breaks = had_breaks, col = viridis(length(had_breaks) -1),
       xlim = c (-32, -3), ylim = c (60, 69),
       cex.main = 2,
       legend = FALSE,
       main = "Haddock, historical")
 
 map('worldHires',add=TRUE, col='grey90', fill=TRUE)
 
 plot (had_245_mn, 
       breaks = had_breaks, col = viridis(length(had_breaks) -1),
       xlim = c (-32, -3), ylim = c (60, 69),
       legend = FALSE,
       cex.main = 2,
       main = "Haddock, SSP 245")
 map('worldHires',add=TRUE, col='grey90', fill=TRUE)
 
 plot (had_585_mn, 
       breaks = had_breaks, col = viridis(length(had_breaks) -1),
       xlim = c (-32, -3), ylim = c (60, 69),
       cex.main = 2,
       main = "Haddock, SSP 585")
 map('worldHires',add=TRUE, col='grey90', fill=TRUE)
 
 plot (her_hist_mn, 
       breaks = her_breaks, col = viridis(length(her_breaks) -1),
       xlim = c (-32, -3), ylim = c (60, 69),
       legend = FALSE,
       cex.main = 2,
       main = "Herring, historical")
 map('worldHires',add=TRUE, col='grey90', fill=TRUE)
 
 plot (her_245_mn, 
       breaks = her_breaks, col = viridis(length(her_breaks) -1),
       xlim = c (-32, -3), ylim = c (60, 69),
       legend = FALSE,
       cex.main = 2,
       main = "Herring, SSP 245")
 map('worldHires',add=TRUE, col='grey90', fill=TRUE)
 
 plot (her_585_mn, 
       breaks = her_breaks, col = viridis(length(her_breaks) -1),
       xlim = c (-32, -3), ylim = c (60, 69),
       cex.main = 2,
       main = "Herring, SSP 585")
 map('worldHires',add=TRUE, col='grey90', fill=TRUE)
 dev.off()
 
 
 
# for temp_depth, m. merlangus was biggest increase, brosme was biggest decrease

m_mer_hist_br <- brick ("Models/Prediction_bricks/Sebastes_marinus_Depth_Temp_2000_2018.grd")
m_mer_245_br <- brick ("Models/Prediction_bricks/Sebastes_marinus_Depth_Temp_245_2061_2080.grd")
m_mer_585_br <- brick ("Models/Prediction_bricks/Sebastes_marinus_Depth_Temp_585_2061_2080.grd")

m_mer_hist_mn <- calc (m_mer_hist_br, mean)
m_mer_245_mn <- calc (m_mer_245_br, mean)
m_mer_585_mn <- calc (m_mer_585_br, mean)

mer_breaks <- quantile (getValues(m_mer_585_mn), probs = c(seq (0, .9, 0.1), 0.95, 0.99, 1), na.rm = TRUE)

bros_hist_br <- brick ("Models/Prediction_bricks/Brosme_brosme_Depth_Temp_2000_2018.grd")
bros_245_br <- brick ("Models/Prediction_bricks/Brosme_brosme_Depth_Temp_245_2061_2080.grd")
bros_585_br <- brick ("Models/Prediction_bricks/Brosme_brosme_Depth_Temp_585_2061_2080.grd")

bros_hist_mn <- calc (bros_hist_br, mean)
bros_245_mn <- calc (bros_245_br, mean)
bros_585_mn <- calc (bros_585_br, mean)

bros_breaks <- quantile (getValues(bros_hist_mn), probs = c(seq (0, .9, 0.1), 0.95, 0.99, 1), na.rm = TRUE)


png ("Figures/Merlangus_Brosme_maps.png", width = 16, height = 9, units = "in", res = 300)
par (mfrow = c (2, 3))
plot (m_mer_hist_mn, 
      breaks = mer_breaks, col = viridis(length(mer_breaks) -1),
      xlim = c (-32, -3), ylim = c (60, 69),
      legend = FALSE,
      main = "M. merlangus historical")

map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (m_mer_245_mn, 
      breaks = mer_breaks, col = viridis(length(mer_breaks) -1),
      xlim = c (-32, -3), ylim = c (60, 69),
      legend = FALSE,
      main = "M. merlangus 245")
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (m_mer_585_mn, 
      breaks = mer_breaks, col = viridis(length(mer_breaks) -1),
      xlim = c (-32, -3), ylim = c (60, 69),
      main = "M. merlangus 585")
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (bros_hist_mn, 
      breaks = bros_breaks, col = viridis(length(bros_breaks) -1),
      xlim = c (-32, -3), ylim = c (60, 69),
      legend = FALSE,
      main = "B. brosme historical")
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (bros_245_mn, 
      breaks = bros_breaks, col = viridis(length(bros_breaks) -1),
      xlim = c (-32, -3), ylim = c (60, 69),
      legend = FALSE,
      main = "B. brosme 245")
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (bros_585_mn, 
      breaks = bros_breaks, col = viridis(length(bros_breaks) -1),
      xlim = c (-32, -3), ylim = c (60, 69),
      main = "B. brosme 585")
map('worldHires',add=TRUE, col='grey90', fill=TRUE)
dev.off()


# for temp_depth, s. marinus and l. esmarkii are different directions

smar_hist_br <- brick ("Models/Prediction_bricks/Sebastes_marinus_Depth_Temp_2000_2018.grd")
smar_245_br <- brick ("Models/Prediction_bricks/Sebastes_marinus_Depth_Temp_245_2061_2080.grd")
smar_585_br <- brick ("Models/Prediction_bricks/Sebastes_marinus_Depth_Temp_585_2061_2080.grd")

smar_hist_mn <- calc (smar_hist_br, mean)
smar_245_mn <- calc (smar_245_br, mean)
smar_585_mn <- calc (smar_585_br, mean)

smar_breaks <- quantile (getValues(smar_hist_mn), probs = c(seq (0, .9, 0.1), 0.95, 0.99, 1), na.rm = TRUE)

les_hist_br <- brick ("Models/Prediction_bricks/Lycodes_esmarkii_Depth_Temp_2000_2018.grd")
les_245_br <- brick ("Models/Prediction_bricks/Lycodes_esmarkii_Depth_Temp_245_2061_2080.grd")
les_585_br <- brick ("Models/Prediction_bricks/Lycodes_esmarkii_Depth_Temp_585_2061_2080.grd")

les_hist_mn <- calc (les_hist_br, mean)
les_245_mn <- calc (les_245_br, mean)
les_585_mn <- calc (les_585_br, mean)

les_breaks <- quantile (getValues(les_hist_mn), probs = c(seq (0, .9, 0.1), 0.95, 0.99, 1), na.rm = TRUE)


png ("Figures/Smarinus_Lesmarkii_maps.png", width = 16, height = 9, units = "in", res = 300)
par (mfrow = c (2, 3))
plot (smar_hist_mn, 
      breaks = smar_breaks, col = viridis(length(smar_breaks) -1),
      xlim = c (-32, -3), ylim = c (60, 69),
      legend = FALSE,
      main = "S. marinus historical")

map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (smar_245_mn, 
      breaks = smar_breaks, col = viridis(length(smar_breaks) -1),
      xlim = c (-32, -3), ylim = c (60, 69),
      legend = FALSE,
      main = "S. marinus 245")
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (smar_585_mn, 
      breaks = smar_breaks, col = viridis(length(smar_breaks) -1),
      xlim = c (-32, -3), ylim = c (60, 69),
      main = "S. marinus 585")
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (les_hist_mn, 
      breaks = les_breaks, col = viridis(length(les_breaks) -1),
      xlim = c (-32, -3), ylim = c (60, 69),
      legend = FALSE,
      main = "L. esmarkii historical")
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (les_245_mn, 
      breaks = les_breaks, col = viridis(length(les_breaks) -1),
      xlim = c (-32, -3), ylim = c (60, 69),
      legend = FALSE,
      main = "L. esmarkii 245")
map('worldHires',add=TRUE, col='grey90', fill=TRUE)

plot (les_585_mn, 
      breaks = les_breaks, col = viridis(length(les_breaks) -1),
      xlim = c (-32, -3), ylim = c (60, 69),
      main = "L. esmarkii 585")
map('worldHires',add=TRUE, col='grey90', fill=TRUE)
dev.off()


###
# https://stackoverflow.com/questions/33227182/how-to-set-use-ggplot2-to-map-a-raster
library (rasterVis)
levelplot(bros_hist_mn, 
          margin=FALSE,                       
          colorkey=list(
            space='bottom',                   
            labels=list(at=-5:5, font=4),
            axis.line=list(col='black')       
          ),    
          par.settings=list(
            axis.line=list(col='transparent') 
          ),
          scales=list(draw=FALSE),            
          col.regions=viridis,                   
          at=seq(-5, 5, len=101)) +           
  layer(sp.polygons(iceland, lwd=3))
