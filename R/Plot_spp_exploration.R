## Plot species abundance and kg from MFRI data
# 6/17/2020
# JGM

# Data exploration, plot time series for each species

library (tidyverse)
library(lubridate)
library (ggmap)

spr_ds <- read_csv ("Data/springsurvey.csv",
                    col_types = cols(
                      sample_id = col_factor(),
                      species = col_factor(),
                      stat_sq = col_factor() # staves off trailing ch parsing error
                      
                    )) %>%
  mutate (date = make_date (year, month))

aut_ds <- read_csv ("Data/autumnsurvey.csv",
                    col_types = cols(
                      sample_id = col_factor(),
                      species = col_factor(),
                      stat_sq = col_factor() # staves off trailing ch parsing error
                      
                    ))%>%
  mutate (date = make_date (year, month))

# combined dataset
comb_ds <- rbind (spr_ds, aut_ds) 
comb_ds$season = c(rep ("spring", nrow (spr_ds)), rep("autumn", nrow (aut_ds)))

# species names and Ids
spp_list <- read_csv ("Data/species_eng.csv")


# dataset that just has sums for each species

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

# plot dotplot, histogram, bar graph for each species----
cod_ds <- spp_tot_ds %>%
  filter (species == 1)

mf_ds <- spp_tot_ds %>%
  filter (species == 14)

library (gridExtra)

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
               mn_n = mean(n_tot))
  
  
  p1 <- ggplot (spp_subset, aes (x = year, y= kg_tot, col = season)) +
    geom_point (alpha = 0.5) +
    geom_smooth (method = "lm") +
    ggtitle ("Biomass jitter") 
  
  
  p2 <- ggplot (spp_mns, aes (x = year, y = mn_kg, col = season)) +
    geom_point() +
    geom_line() +
    geom_smooth (method = "lm") +
    ggtitle("Biomass annual mean")+
    theme(legend.position = "none")
  
  p3 <- ggplot (spp_subset, aes (x = year, y= n_tot, col = season)) +
    geom_jitter (alpha = 0.5) +
    geom_smooth (method = "lm") +
    ggtitle ("Abundance jitter")
  
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
  facet_wrap (~year_bn) +
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

# just plot number of samples each year for eacn species

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
