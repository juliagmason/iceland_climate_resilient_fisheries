## Plot species abundance and kg from MFRI data
# 6/17/2020
# JGM

# Data exploration, plot time series for each species

library (tidyverse)
library(lubridate)
library (ggmap)
library (gridExtra)

comb_ds <- read_csv ("Data/MFRI_comb_survey.csv",
                     col_types = cols(
                       sample_id = col_factor(),
                       species = col_factor(),
                       stat_sq = col_factor())
)

# species names and Ids
spp_list <- read_csv ("Data/species_eng.csv")

# P/A table
mfri_pa <- read_csv ("Data/PA_MFRI.csv",
                     col_types = cols(
                       sample_id = col_factor()
                     ))


# dataset that just has sums for each species----

spp_tot_ds <- comb_ds %>%
  group_by (sample_id, species) %>%
  summarize (n_tot = sum(n_per_nautmile, na.rm = TRUE),
             kg_tot = sum(kg_per_nautmile, na.rm = TRUE),
             station = first(station),
             lat = first(lat),
             lon = first(lon),
             year = first(year),
             date = first(date),
             season = first(season)
  )

# list of species that have data
sampled_spp <- sort(unique (spp_tot_ds$species))


# table that designates years caught and if landed for each species----
# bringing in code from explore_mfri_survey species section

# abbreviated table to match which samples happened in what year/season
sample_yrs <- comb_samples %>%
  dplyr::select (sample_id, year, season)

# attach years to species p/a to determine how many times each species sampled per season/yr
spp_pa_yr <- mfri_pa %>%
  pivot_longer (-sample_id, 
                names_to = "species",
                values_to = "presence") %>%
  left_join (sample_yrs, by = "sample_id") %>%
  group_by (species, year, season) %>%
  summarize (n_p = sum(presence))

# indicate if present in a given year and if landed
spp_yrs_sampled <- spp_pa_yr %>%
  mutate (p = ifelse (n_p > 0, 1, 0)) %>%
  group_by (species, season) %>%
  summarize (n_yrs_sampled = sum(p)) %>%
  mutate (landed = ifelse (species %in% spp_list$sci_name_underscore, 1, 0)) 

# subset species with >= 10 years missing ----
maj_spp <- spp_yrs_sampled %>%
  filter ((season == "autumn" & n_yrs_sampled > 14) | (season == "spring" & n_yrs_sampled > 25) ) %>% # 64 spp
  arrange (desc (n_yrs_sampled)) 

maj_spp_list <- spp_list$Spp_ID[which (spp_list$sci_name_underscore %in% unique(maj_spp$species))]

# plot dotplot, histogram, bar graph for each species----
cod_ds <- spp_tot_ds %>%
  filter (species == 1)

mf_ds <- spp_tot_ds %>%
  filter (species == 14)



#grid.arrange ()

p1 <- ggplot (mf_ds) +
  geom_point (aes (x = n_tot, y = dense_rank(n_tot), col = season)) 

#p1_2 <- dotchart (mf_ds$n_tot, groups = factor(mf_ds$season), col = factor(mf_ds$season))

p2 <- ggplot (mf_ds) +
  geom_boxplot (aes (x = season, y = n_tot)) +
  theme (legend.position = 'none')

p3 <- ggplot (mf_ds) +
  geom_histogram(aes (x = n_tot, fill = season), bins = 50)+
  theme (legend.position = 'none')

p4 <- ggplot (mf_ds) +
  geom_point (aes (x = kg_tot, y = dense_rank(n_tot), col = season)) 

p5 <- ggplot (mf_ds) +
  geom_boxplot (aes (x = season, y = kg_tot))

p6 <- ggplot (mf_ds) +
  geom_histogram(aes (x = kg_tot, fill = season), bins = 50)+
  theme (legend.position = 'none')

grid.arrange (p3, p2, p1, p6, p5, p4, nrow = 2, 
              top = "Monkfish")

# function to plot basic distribution of abundance and biomass----
plot_spp_catch_fun <- function (spp){
  
  #filter catch data
  spp_ds <- spp_tot_ds %>%
    filter (species == spp)
  
  # plot histogram, boxplot, and dotchart for abundance and biomass
  p1 <- ggplot (spp_ds) +
    geom_histogram(aes (x = n_tot, fill = season), bins = 50)+
    theme (legend.position = 'none')
  
  
  p2 <- ggplot (spp_ds) +
    geom_boxplot (aes (x = season, y = n_tot)) +
    theme (legend.position = 'none')
  
  
  p3 <- ggplot (spp_ds) +
    geom_point (aes (x = n_tot, y = dense_rank(n_tot), col = season))
  
  p4 <- ggplot (spp_ds) +
    geom_histogram(aes (x = kg_tot, fill = season), bins = 50)+
    theme (legend.position = 'none')
  
  
  p5 <- ggplot (spp_ds) +
    geom_boxplot (aes (x = season, y = kg_tot))
  
  p6 <- ggplot (spp_ds) +
    geom_point (aes (x = kg_tot, y = dense_rank(n_tot), col = season))
  
  # arrange and title with sci name and common name
  grid.arrange (p1, p2, p3, p4, p5, p6, nrow = 2, 
                top = paste (spp_list$Scientific_name[which(spp_list$Spp_ID == spp)], ", ",
                             spp_list$Common_name[which(spp_list$Spp_ID == spp)], sep = "")
  )
}

pdf (file = "Figures/Spp_abundance_biomass.pdf")

lapply (sampled_spp, plot_spp_catch_fun)

dev.off()


# Function to plot ts of abundance and biomass----
all_yrs = data.frame (year = rep(1985:2020, each = 2),
                      season = rep (c("spring", "autumn"), length(1985:2020))
                      )
plot_spp_ts_fun <- function (spp) {
  spp_subset <- spp_tot_ds %>%
    ungroup() %>%
    filter (species == spp) %>%
    dplyr::select (n_tot, kg_tot, year, season) %>%
    full_join (all_yrs, by = c("year", "season")) %>%
    mutate (kg_tot = replace(kg_tot, is.na(kg_tot) & (season == "spring" |  (season == "autumn" & year %in% 1995:2019)), 0),
            n_tot = replace(n_tot, is.na(n_tot) & (season == "spring" |  (season == "autumn" & year %in% 1995:2019)), 0))
  
  spp_mns <- spp_subset %>%
    group_by (year, season) %>%
    summarize (mn_kg = mean (kg_tot),
               se_kg = var(kg_tot)/sqrt (n()),
               mn_n = mean(n_tot),
               se_n = var (n_tot)/sqrt(n())
               )
  
  
  p1 <- ggplot (spp_subset, aes (x = year, y= kg_tot, col = season)) +
    geom_point (alpha = 0.5) +
    geom_smooth (method = "lm") +
    ggtitle ("Biomass jitter")
  
  
  p2 <- ggplot (spp_mns, aes (x = year, y = mn_kg, col = season)) +
    geom_point() +
    geom_line() +
    #geom_pointrange (aes (ymin = mn_kg - se_kg, ymax = mn_kg + se_kg)) +
    geom_smooth (method = "lm") +
    ggtitle("Biomass annual mean")+
    theme(legend.position = "none")
  
  ggplot (spp_mns, aes (x = year, y = se_kg, col = season)) +
    geom_point() +
    geom_line() +
    #geom_pointrange (aes (ymin = mn_kg - se_kg, ymax = mn_kg + se_kg)) +
    geom_smooth (method = "lm") +
    ggtitle("Biomass annual mean")+
    theme(legend.position = "none")
  
  # p3 <- ggplot (spp_subset, aes (x = year, y= n_tot, col = season)) +
  #   geom_jitter (alpha = 0.5) +
  #   geom_smooth (method = "lm") +
  #   ggtitle ("Abundance jitter")
  
  p4 <- ggplot (spp_mns, aes (x = year, y = mn_n, col = season)) +
    geom_point() +
    geom_line() +
    geom_smooth (method = "lm") +
    ggtitle("Abundance annual mean")+
    theme(legend.position = "none")
  
  grid.arrange (p2, p1, p4, p3, nrow = 2, # nicer to have mean ts larger
                top = paste (spp_list$Scientific_name[which(spp_list$Spp_ID == spp)], ", ",
                             spp_list$Common_name[which(spp_list$Spp_ID == spp)], sep = "")
  )
  
  print (spp_list$Common_name[which(spp_list$Spp_ID == spp)])
  
}

pdf (file = "Figures/Spp_abundance_biomass_ts.pdf")

lapply (sampled_spp, plot_spp_ts_fun)

dev.off()

#Function to map species distribution----

# blocks of five years?
#https://stackoverflow.com/questions/54884478/how-to-group-by-variable-and-cut-time-into-10s-bins-starting-at-132400-exactly
yr_breaks <- seq(1985, 2020, 5) # this cuts out 1985, going to say that's ok for now
yr_breaks[1] <- 0 # would include 1985

#ggmap basemap
#isl_basemap <- get_stamenmap (bbox = c(-30, 62, -8, 70), maptype = "toner-lite")

# just do outline
library (maps)
library (mapdata)

#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
library (sf)
library (rnaturalearth)
library (rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")


plot_spp_distr_fun <- function (spp) {
  
spp_distr_blocks <- spp_tot_ds %>%
  ungroup() %>%
  filter (species == spp) %>%
  full_join (all_yrs, by = c("year", "season")) %>%
  mutate (kg_tot = replace(kg_tot, is.na(kg_tot) & (season == "spring" |  (season == "autumn" & year %in% 1995:2019)), 0),
          year_bn = cut(year, breaks = yr_breaks)) %>%
  filter (!is.na(year_bn)) #just cut out 1985 for now

plt <- ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-30, -8), ylim = c (62,70), expand = FALSE ) +
  geom_point (aes (x = lon, y = lat, col = season, size = kg_tot),alpha = 0.7, data = spp_distr_blocks) +
  facet_wrap (~ <- ) +
  ggtitle (paste (spp_list$Scientific_name[which(spp_list$Spp_ID == spp)], ", ",
                  spp_list$Common_name[which(spp_list$Spp_ID == spp)], sep = "")
           ) +
  theme_bw() +
  theme (axis.text = element_blank())

print(plt)

#print (spp_list$Common_name[which(spp_list$Spp_ID == spp)])
  
}

pdf (file = "Figures/Spp_distribution.pdf")

lapply (sampled_spp, plot_spp_distr_fun)

dev.off()

# species distribution, try heatmap instead----
  
  ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-30, -8), ylim = c (62,70), expand = FALSE ) +
  stat_summary_2d(data = cod_length_bins, aes(x = lon, y = lat, z = n_log), fun = mean, binwidth = c (.5, .5)) +
  scale_fill_viridis_c(name = "Mean log abundance") +
  
  
  plot_spp_heatmap_fun <- function (spp) {
    
    spp_distr_blocks <- spp_tot_ds %>%
      ungroup() %>%
      filter (species == spp) %>%
      full_join (all_yrs, by = c("year", "season")) %>%
      mutate (kg_tot = replace(kg_tot, is.na(kg_tot) & (season == "spring" |  (season == "autumn" & year %in% 1995:2019)), 0),
              year_bn = cut(year, breaks = yr_breaks)) %>%
      filter (!is.na(year_bn)) #just cut out 1985 for now
    
    plt <- ggplot(data = world) +
      geom_sf() +
      coord_sf (xlim = c(-30, -8), ylim = c (62,70), expand = FALSE ) +
      stat_summary_2d(data = spp_distr_blocks, aes(x = lon, y = lat, z = log(kg_tot)), fun = mean, binwidth = c (.5, .5)) +
      scale_fill_viridis_c(name = "Mean log biomass") +
      facet_wrap (~year_bn) +
      ggtitle (paste (spp_list$Scientific_name[which(spp_list$Spp_ID == spp)], ", ",
                      spp_list$Common_name[which(spp_list$Spp_ID == spp)], sep = "")
      ) +
      theme_bw() +
      theme (axis.text = element_blank())
    
    print(plt)
    
    #print (spp_list$Common_name[which(spp_list$Spp_ID == spp)])
    
  }
               


# just plot number of samples each year for eacn species----

# plot check n samples over time
n_samples <- comb_samples %>%
  group_by (season, year) %>%
  tally() %>%
  ggplot (aes (x = year, y = n, col = season)) +
  geom_point() +
  geom_line()
# not sure this is helpful

spp_n_samples <- spp_tot_ds %>%
  group_by (species, season, year) %>%
  tally()

plot_spp_samples_ts <- function(spp){
  spp_samples <- filter (spp_n_samples, species == spp)
  
  print(
    ggplot (spp_samples, aes (x = year, y = n, col = season)) +
      geom_point() +
      geom_line() +
      geom_smooth (method = "lm") +
      ggtitle(paste("Number of samples present, ", spp_list$Scientific_name[which(spp_list$Spp_ID == spp)], ", ",
                                                        spp_list$Common_name[which(spp_list$Spp_ID == spp)], sep = "")) 
  )
}

# abundance and distribution for major caught spp ----

# make bigger breaks, 1985-1995 and then five years
yr_breaks <- c(1984, seq(1995, 2020, 5))

plot_maj_spp_ts_distrb_fun <- function (spp){
  


spp_subset <- spp_tot_ds %>%
  ungroup() %>%
  filter (species == spp) %>%
  dplyr::select (n_tot, kg_tot, year, season) %>%
  full_join (all_yrs, by = c("year", "season")) %>%
  mutate (kg_tot = replace(kg_tot, is.na(kg_tot) & (season == "spring" |  (season == "autumn" & year %in% 1995:2019)), 0),
          n_tot = replace(n_tot, is.na(n_tot) & (season == "spring" |  (season == "autumn" & year %in% 1995:2019)), 0))

spp_mns <- spp_subset %>%
  group_by (year, season) %>%
  summarize (mn_kg = mean (kg_tot),
             se_kg = var(kg_tot)/sqrt (n()),
             mn_n = mean(n_tot),
             se_n = var (n_tot)/sqrt(n())
  )


spp_distr_blocks <- spp_tot_ds %>%
  ungroup() %>%
  filter (species == spp) %>%
  full_join (all_yrs, by = c("year", "season")) %>%
  mutate (kg_tot = replace(kg_tot, is.na(kg_tot) & (season == "spring" |  (season == "autumn" & year %in% 1995:2019)), 0),
          year_bn = cut(year, breaks = yr_breaks)) %>%
  filter (!is.na(year_bn)) #just cut out 1985 for now


p2 <- ggplot (spp_mns, aes (x = year, y = mn_kg, col = season)) +
  geom_point() +
  geom_line() +
  #geom_pointrange (aes (ymin = mn_kg - se_kg, ymax = mn_kg + se_kg)) +
  geom_smooth (method = "lm") +
  ggtitle("Biomass annual mean")+
  theme(legend.position = "none")

p4 <- ggplot (spp_mns, aes (x = year, y = mn_n, col = season)) +
  geom_point() +
  geom_line() +
  geom_smooth (method = "lm") +
  ggtitle("Abundance annual mean")+
  theme(legend.position = "none")

plt <- ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-30, -8), ylim = c (62,70), expand = FALSE ) +
  geom_point (aes (x = lon, y = lat, col = season, size = kg_tot),alpha = 0.7, data = spp_distr_blocks) +
  facet_wrap (~year_bn, ncol = 6) +
  #ggtitle (paste (spp_list$Scientific_name[which(spp_list$Spp_ID == spp)], ", ",
                  #spp_list$Common_name[which(spp_list$Spp_ID == spp)], sep = "")
  #) +
  theme_bw() +
  theme (axis.text = element_blank(),
         legend.position = "bottom", 
         legend.box = "horizontal")

# match species to spp_yrs_sampled
spp_names <- spp_list %>%
  filter (Spp_ID == spp)

spp_yrs <- spp_yrs_sampled %>%
  filter (species == spp_names$sci_name_underscore)

grid.arrange (p2, p4, plt, nrow = 2, # nicer to have mean ts larger
              #widths = c (1,2),
              layout_matrix = cbind (c (1,3), c (2, 3)),
              top = paste (
                # Scientific name
                spp_list$Scientific_name[which(spp_list$Spp_ID == spp)], ", ",
                # Common name
                spp_list$Common_name[which(spp_list$Spp_ID == spp)], 
                # N years sampled in spring
                ", S:", spp_yrs$n_yrs_sampled[2],
                # N yeras sampled in autumn
                ", A:", spp_yrs$n_yrs_sampled[1],
                ifelse (spp_yrs$landed == 1, 
                        ", L",
                        ""),
                sep = "")
              )

}

#print (spp_list$Common_name[which(spp_list$Spp_ID == spp)])



pdf (file = "Figures/Maj_spp_ts_dist.pdf")

lapply (maj_spp_list, plot_maj_spp_ts_distrb_fun)

dev.off()

# plot difference in length distributions autumn and spring for major species ----
plot_season_length_dist_fun <- function (spp) {
  print (
  comb_ds %>%
    filter (species == spp) %>%
    ggplot (aes (x = length_cm, y = n_per_nautmile, fill = season)) +
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle (paste (spp_list$Scientific_name[which(spp_list$Spp_ID == spp)], ", ",
                   spp_list$Common_name[which(spp_list$Spp_ID == spp)], sep = "")
    )
    )
}

pdf (file = "Figures/Maj_spp_length_season_dist.pdf")
lapply (maj_spp_list, plot_season_length_dist_fun)
dev.off()


#### plot neater biomass distribution histograms for selected spp----
library (grid)

scan_fun <- function(x){
  sc <- scan(what = "", text = x, sep = ".")[1]
  return (sc)
}

nb_files <- list.files ("Models/Borm_14_nb/")
nb_spp <- map_chr (nb_files, scan_fun)

load ("Data/abun_full_borm.RData")

plot_biomass_hist <- function (sci_name) {
  
  spp_id <- as.numeric(as.character(spp_list$Spp_ID[which (spp_list$sci_name_underscore == sci_name)]))# this fixes weird factor problem
  
  spp_subset <- filter (mfri_abun_full_borm, species == spp_id)
  
  n_zero <- length (which (spp_subset$kg_tot == 0))
  perc_zero = round(n_zero/nrow (spp_subset), 2)
  
  spp_presence <- filter (spp_subset, kg_tot > 0)
  
  histp <- ggplot(spp_presence) +
    geom_histogram (aes(x = kg_tot)) +
    theme_bw() +
    ggtitle ("Biomass, presence only")
  
  histlog <- ggplot(spp_presence) +
    geom_histogram (aes(x = log(kg_tot))) +
    theme_bw() +
    ggtitle ("log(Biomass), presence only")
  
  # https://stackoverflow.com/questions/36008659/edit-style-of-grid-arrange-title-bold-italic-etc-r
  # https://stackoverflow.com/questions/32280476/gridextra-2-0-0-change-title-size
  #textGrob("Title", gp=gpar(fontsize=15,font=8)))
  title1 = textGrob(paste0 (sci_name, ", ", "% zeros = ", perc_zero), gp=gpar(fontsize=15))
  
  grid.arrange (histp, histlog,  nrow = 1, # nicer to have mean ts larger
                #widths = c (1,2),
                top = title1
                
  )
  
}

plot_biomass_hist("Microstomus_kitt")

pdf (file = "Figures/Biomass_distrib_10spp.pdf")
lapply (sort(borm_spp$sci_name_underscore[1:10]), plot_biomass_hist)
dev.off()

pdf (file = "Figures/Biomass_distrib_borm_spp.pdf")
lapply (sort(borm_spp$sci_name_underscore), plot_biomass_hist)
dev.off()
