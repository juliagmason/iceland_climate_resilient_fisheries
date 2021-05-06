## Additional species niche trends visualizations
# 3/14/2021

# JGM

# Can i make a lollipop chart or cleveland dot chart to show change in biomass-weighted temperature from the colder period (maybe 1985-1993) and warmer period (maybe 2005-2013). start with only spring data? since autumn only starts in 2001

library (tidyverse)

mfri_abun <- read_csv ("Data/MFRI_comb_survey.csv",
                       col_types = cols(
                         sample_id = col_factor(),
                         species = col_factor(),
                         stat_sq = col_factor()
                       )
)%>%
  filter (!(year == 2011 & season == "autumn"),
          !(year < 2000 & season == "autumn")) %>% # remove autumn 2011 [labor strike] and pre-2000 autumn [sites weren't standard yet]
  # tweaked this from in the other gam_fitting etc. scripts so I don't have to bring in mfri_pred
  select (-length_cm) %>% # this is the only remaining column that varies within sample_id
  group_by (sample_id, species) %>%
  mutate (n_tot = sum(n_per_nautmile),
          kg_tot = sum(kg_per_nautmile),
          n_log = log (n_tot),
          kg_log = log (kg_tot), 
          .keep = "unused") %>% 
  
  ungroup() %>%
  distinct ()  # now should have one row per species, per sample



#  df of species names
spp_list  <- read_csv ("Data/species_eng.csv",
                       col_types = cols(
                         Spp_ID = col_factor()
                       )) %>%
  # rename (species = sci_name_underscore) %>% #rename to match species column
  # Common names have several options separated by commas, only grab first option for more legible graphs
  # https://stackoverflow.com/questions/42565539/using-strsplit-and-subset-in-dplyr-and-mutate
  mutate(Common_name = sapply(str_split(Common_name, ","), function(x) x[1]))


# filter out uncommon spp and invertebrates
# species with n_spring > 35 or n_autumn > 24 is 31, that's a reasonable start
common_spp <- spp_list %>%
  filter (n_spring > 35 | n_autumn > 24)


spp_2period <- mfri_abun %>%
  # # remove species with only one observation
  # group_by (species) %>%
  # filter (n() > 1) %>%
  # ungroup() %>%
  filter (species %in% common_spp$Spp_ID) %>%
  select (species, kg_tot, year, season, bottom_temp, tow_depth_begin, lat, lon) %>% # removed surface_temp
  # rename columns to replace underscore with period. Summarise will append new column names with an underscore and I can't figure out how to change it there
  rename_all(~ gsub("_", ".", .)) %>%
  # mutate (period = case_when (year %in% c(1985:1993) ~ "Period1",
  #                    year %in% c(2005:2013) ~ "Period2")
  #         ) %>%
  mutate (period = case_when (year %in% c(1995:1999) ~ "Period1",
                              year %in% c(2010:2014) ~ "Period2")
  ) %>%
  group_by (species, period, season) %>%
  summarise (
    across (bottom.temp:lon,
            list (
              wtmean = ~ weighted.mean (., w = kg.tot, na.rm = TRUE),
              se = ~ sd (.x, na.rm = TRUE)/sqrt(length(.x))
            ) # end list
    )) 

# https://www.r-graph-gallery.com/303-lollipop-plot-with-2-values.html

# pivot longer for ggplot. have a column with variable names, and then mean_1, mean_2, se_1, se_2
# just looking at spring for now
spp_2per_gg <- spp_2period %>%
  filter (!is.na (period), season == "spring") %>%
  pivot_longer (cols = bottom.temp_wtmean:lon_se, 
                names_pattern = "(.*)_(.*)",
                names_to = c("var", "metric"),
                values_to = "value") %>%
  pivot_wider (names_from = metric, 
               values_from = value) %>%
  pivot_wider ( 
               names_from = period,
               values_from = c(wtmean, se)) %>%
  rename (Spp_ID = species) %>%
  left_join (spp_list, by = "Spp_ID") 


## also add overall average point
overall_means <- mfri_abun %>%
  filter (species %in% common_spp$Spp_ID, season == "spring") %>% 
  select (species, kg_tot, year, season, bottom_temp, tow_depth_begin, lat, lon) %>% # removed sst
  rename_all(~ gsub("_", ".", .)) %>%
  group_by (species) %>%
  summarise (
    across (bottom.temp:lon,
            list (
              wtmean = ~ weighted.mean (., w = kg.tot, na.rm = TRUE),
              se = ~ sd (.x, na.rm = TRUE)/sqrt(length(.x))
            ) # end list
    )) %>%
  pivot_longer (cols = bottom.temp_wtmean:lon_se, 
                        names_pattern = "(.*)_(.*)",
                        names_to = c("var", "metric"),
                        values_to = "value") %>%
  pivot_wider (names_from = metric, 
               values_from = value) %>%
  rename (Spp_ID = species)
  

spp_2per_gg <- spp_2per_gg %>%
  left_join (overall_means, by = c("Spp_ID", "var"))

## plot charts reordered by size of change and start variable, all variables

# https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

plot_lollipop_2 <- function (varname) {
  
  # more legible variable name for plot titles
  titlename <- simpleCap(gsub ("\\.", " ", varname))
  
  titlename <- ifelse (titlename == "Tow Depth Begin", "Depth", titlename)
  
  # order by value in period 1
  spp_2per_gg_per1 <- spp_2per_gg %>%
    filter (var == varname) %>% 
    filter (!is.na (wtmean_Period1), !is.na(wtmean_Period2)) %>%
    ungroup() %>%
    mutate (Common_name = fct_reorder (Common_name, wtmean_Period1))
  

  
  # png (paste0("Figures/Spp_niche_", varname, "_lollipop_per1.png"), width = 16, height = 9, res = 300, units = "in")
  # 
  # print (
    
       spp_2per_gg_per1 %>%
       ggplot () +
         geom_segment (aes (x = Common_name, xend = Common_name,
                            y = wtmean_Period1, yend = wtmean_Period2)) +
         geom_point (aes (x = Common_name, y = wtmean_Period1), color = "blue", size = 3, shape = 1) +
         geom_point (aes (x = Common_name, y = wtmean_Period2), color = "red", size = 3, shape = 1) +
         geom_point (aes (x = Common_name, y = wtmean), color = "black", size = 3, shape = 4) +
         coord_flip () +
         theme_bw() +
         theme (legend.position = "none",
                plot.title = element_text (size = 16),
                axis.text.x = element_text (size = 14),
                axis.title = element_text (size = 14),
                axis.text.y = element_text (size = 12)) +
         #scale_color_gradient2 (low = "blue", high = "red")
         labs (x = "", y = titlename) +
         ggtitle (paste0("Biomass-weighted mean ", titlename, ", 1995-1999 (blue) to 2010-2014 (red)"))
       
  #) 
  
       #dev.off()
       
       # order by magnitude of change

       # png (paste0("Figures/Spp_niche_", varname, "_lollipop_chg.png"), width = 16, height = 9, res = 300, units = "in")
       # 
       # print (
       #      
       # spp_2per_gg %>%
       #   filter (var == varname) %>% 
       #   filter (!is.na (wtmean_Period1), !is.na(wtmean_Period2)) %>%
       #   ungroup() %>%
       #   mutate (Common_name = fct_reorder2 (Common_name, wtmean_Period1, wtmean_Period2, .fun = function (x, y) {abs( y - x)}, .desc = FALSE)) %>% 
       #   
       #   ggplot () +
       #   geom_segment (aes (x = Common_name, xend = Common_name,
       #                      y = wtmean_Period1, yend = wtmean_Period2)) +
       #   geom_point (aes (x = Common_name, y = wtmean_Period1), color = "blue", size = 3, shape = 1) +
       #   geom_point (aes (x = Common_name, y = wtmean_Period2), color = "red", size = 3, shape = 1) +
       #   coord_flip () +
       #   theme_bw() +
       #   theme (legend.position = "none",
       #          plot.title = element_text (size = 16),
       #          axis.text.x = element_text (size = 14),
       #          axis.title.y = element_text (size = 14),
       #          axis.text.y = element_text (size = 12)) +
       #   #scale_color_gradient2 (low = "blue", high = "red")
       #   labs (x = "", y = titlename) +
       #   ggtitle (paste0("Biomass-weighted mean ", titlename, ", 1985-1993 (blue) to 2005-2013 (red)"))
       # 
       # )
       # 
       # dev.off()

}

plot_lollipop_2("bottom.temp")

lapply (unique (spp_2per_gg$var), plot_lollipop_2)

pdf ("Figures/Spp_niche_lollipop_common_spp.pdf", width = 11, height = 8.5)
lapply (unique (spp_2per_gg$var), plot_lollipop_2)
dev.off()





png ("Figures/Spp_niche_sst_lollipop_per1.png", width = 16, height = 9, res = 300, units = "in")
spp_2per_gg %>%
  filter (var == "surface.temp") %>% 
  filter (!is.na (wtmean_Period1), !is.na(wtmean_Period2)) %>%
  ungroup() %>%
  #arrange (wtmean_Period1) %>%
  mutate (Common_name = fct_reorder (Common_name, wtmean_Period1)) %>% 

  ggplot () +
  geom_segment (aes (x = Common_name, xend = Common_name,
                     y = wtmean_Period1, yend = wtmean_Period2)) +
  geom_point (aes (x = Common_name, y = wtmean_Period1), color = "blue", size = 3, shape = 1) +
  geom_point (aes (x = Common_name, y = wtmean_Period2), color = "red", size = 3, shape = 1) +
  coord_flip () +
  theme_bw() +
  theme (legend.position = "none",
         plot.title = element_text (size = 16),
         axis.text.x = element_text (size = 14),
         axis.title.y = element_text (size = 14),
         axis.text.y = element_text (size = 12)) +
  #scale_color_gradient2 (low = "blue", high = "red")
  labs (x = "", y = "Temperature, C") +
  ggtitle ("Biomass-weighted mean temperature, 1985-1993 (blue) to 2005-2013 (red)")
dev.off()

# instead, order by magnitude of change
png ("Figures/Spp_niche_sst_lollipop_chg.png", width = 16, height = 9, res = 300, units = "in")
spp_2per_gg %>%
  filter (var == "surface.temp") %>% 
  filter (!is.na (wtmean_Period1), !is.na(wtmean_Period2)) %>%
  ungroup() %>%
  #arrange (wtmean_Period1) %>%
  mutate (Common_name = fct_reorder2 (Common_name, wtmean_Period1, wtmean_Period2, .fun = function (x, y) {abs( y - x)}, .desc = FALSE)) %>% 
  
  ggplot () +
  geom_segment (aes (x = Common_name, xend = Common_name,
                     y = wtmean_Period1, yend = wtmean_Period2)) +
  geom_point (aes (x = Common_name, y = wtmean_Period1), color = "blue", size = 3, shape = 1) +
  geom_point (aes (x = Common_name, y = wtmean_Period2), color = "red", size = 3, shape = 1) +
  coord_flip () +
  theme_bw() +
  theme (legend.position = "none",
         plot.title = element_text (size = 16),
         axis.text.x = element_text (size = 14),
         axis.title.y = element_text (size = 14),
         axis.text.y = element_text (size = 12)) +
  #scale_color_gradient2 (low = "blue", high = "red")
  labs (x = "", y = "Temperature, C") +
  ggtitle ("Biomass-weighted mean temperature, 1985-1993 (blue) to 2005-2013 (red)")
dev.off()
