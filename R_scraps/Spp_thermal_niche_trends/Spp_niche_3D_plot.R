# plotly
library (plotly)
library (tidyverse)

spp_lm_sig <- readRDS ("Data/spp_niche_lm_sig.Rds")

#  df of species names
spp_list  <- read_csv ("Data/species_eng.csv",
                       col_types = cols(
                         Spp_ID = col_factor()
                       )) %>%
  # rename (species = sci_name_underscore) %>% #rename to match species column
  # Common names have several options separated by commas, only grab first option for more legible graphs
  # https://stackoverflow.com/questions/42565539/using-strsplit-and-subset-in-dplyr-and-mutate
  mutate(Common_name = sapply(str_split(Common_name, ","), function(x) x[1]))



#https://stackoverflow.com/questions/45052188/how-to-plot-3d-scatter-diagram-using-ggplot

#https://gist.github.com/aagarw30/800c4da26eebbe2331860872d31720c1

spp_annual_trends <- read_csv ("Data/spp_niche_annual_trends.csv")

slopes <- spp_annual_trends %>%
  group_by (species, season, var) %>%
  filter (n() > 1) %>%
  summarise (slope = coef(lm (wtmean ~ year))[2])

spp_tbl <- spp_list %>%
  #select (Scientific_name, Spp_ID, mean_TB) %>%
  mutate (species = as.double (Spp_ID))

spr_sl <- slopes %>%
  filter (season == "spring") %>%
  pivot_wider (names_from = var,
               values_from = slope) %>%
  left_join (spp_tbl, by = "species")

fig <- plot_ly (spr_sl, x = ~bottom.temp, y = ~lat, z = ~tow.depth.begin, hoverinfo = "text",
                text = spr_sl$Scientific_name,
                color = ~mean_TB)  

# fig %>% 
  # add_markers () 

#https://stackoverflow.com/questions/34580095/using-r-and-plot-ly-how-do-i-script-saving-my-output-as-a-webpage
library (htmlwidgets)

htmlwidgets::saveWidget(as_widget(fig), "index.html")


### reduce dimensionality??

# which species have significant depth trends but not bottom temperature?
spr_deeper <- spp_lm_sig %>%
  filter (season == "spring", var %in% c("depth", "bottom.temp")) %>%
  distinct (species, .keep_all = TRUE)

spr_deeper$species[which (duplicated (spr_deeper$species))]
# how uniform is ti
