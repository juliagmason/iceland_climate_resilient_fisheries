# Plot habitat change boxplots
# Figure 3 in manuscript

library (tidyverse)

#  df of species names
spp_list  <- read_csv ("Data/species_eng.csv",
                       col_types = cols(
                         Spp_ID = col_factor()
                       )) %>%
  rename (species = sci_name_underscore) %>% #rename to match species column
  # Common names have several options separated by commas, only grab first option for more legible graphs
  # https://stackoverflow.com/questions/42565539/using-strsplit-and-subset-in-dplyr-and-mutate
  mutate(Common_name = sapply(str_split(Common_name, ","), function(x) x[1]))

# gam performance to indicate suitable species (not necessary if just plotting 47 suitable spp)
MASE <- read_csv ("Models/GAM_performance_Borm_14_alltemp.csv")

# ==================
# load and plot habitat change ----
# ==================

# load habitat change df, made in Step3_Calculate_habitat_change.R
load ("Data/hab_change_df.RData")

# Plot only 47 suitable species presented in publication
load ("Models/spp_Borm_suit.RData")


# species-by-species change ----

# Looking at various metrics. Percent change is the most common, but hard to interpret when negative and positive changes are very large. Makes positive changes look way more important than negative changes. 


# plot landed spp only for interviews
landed_spp <- spp_list %>%
  filter (!is.na (Landed_name),
          ! species %in% c("Somniosus_microcephalus", "Other_flatfish", "Capelin_roe")) %>%
  pull (species)
# make sure to include lumpfish
landed_spp <- c(landed_spp[1:29], "Cyclopterus_lumpus")

# bring in icelandic names for graphs for stakeholders
isl_spp <- read_csv ("Data/Raw_data/species.csv",
                     col_types = cols(
                       tegund = col_factor()
                     )) %>%
  rename (Spp_ID = tegund) %>%
  left_join (spp_list, by = "Spp_ID") %>%
  dplyr::select (species, heiti)




# make a ggplot unit that I can manipulate for different uses ----
# can manipulate boxplot aesthetics, col and fill values, legend position, title. Labs command gets overridden, so put in subsequent plotting code

# legend position: c(0.83, 0.15) works well for percent change, c(0.6, 0.9) for log10 fold change 

# re-label scenarios
scen_labs <- c("SSP 2-4.5", "SSP 5-8.5")
names (scen_labs) <- c(245, 585) 

hab_change_gg <- hab_change %>%
  filter (species %in% borm_suit) %>%
  # use isl_spp instead of spp_list for icelandic names. 
  #left_join (isl_spp, by = "species") %>%
  
  left_join (MASE, by = "species") %>%
  
  
  mutate (Model_suitable = ifelse ( MASE_GAM < 1 & DM_GAM_p < 0.05, 
                                    "1", 
                                    "0"),
          Quota = factor(Quota),
          
          # create column to manipulate fill based on quota and model
          # https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette, show_col(hue_pal()(2))
          fill_quota = case_when (
            Model_suitable == 0 ~ "Model unsuitable",
            Model_suitable == 1 & Quota == 1 ~ "Quota",
            Model_suitable == 1 & Quota == 0 ~ "Non-Quota"
          ), 
          
          # column for fill based on thermal preference
          Therm_pref = case_when (
            mean_TB > 0 ~ "Warm",
            between (mean_TB, -3, 0) ~ "Cool",
            mean_TB < 0 ~ "Cold"
          ),
          
          fill_therm = case_when(
            Model_suitable == 0 ~ "*Model unsuitable",
            Model_suitable == 1 & mean_TB > 0 ~ "Warm",
            Model_suitable == 1 & between (mean_TB, -3, 0) ~ "Cool",
            Model_suitable == 1 &  mean_TB < 0 ~ "Cold"
          ),
          # reorder by desired unit of change
          species = fct_reorder (species, log10_foldchange),
          # also reorder common_name for changing labels
          Common_name = fct_reorder (Common_name, log10_foldchange)
  ) %>% # end mutate
          
  
  # filter landed species, using MFRI data loaded in plot_region_sector_exposure. remember to include lumpfish
  #filter (Spp_ID %in% ldgs$species | Spp_ID == 48) %>% 
  # get rid of species that I didn't have enough data for model diagnostics
  #filter (!is.na (Model_suitable)) %>%
  #filter (Model_suitable == 1, species != "Leptoclinus_maculatus") %>%

  ggplot () +
  guides (color = FALSE) +
  geom_vline (xintercept = 0, lty = 2, col = "dark gray") +
  facet_wrap (~scenario, 
              labeller = labeller(scenario = scen_labs)) +
  theme_bw() +
  theme (
    axis.text.x = element_text (size = 14),
    axis.text.y = element_text (size = 12),
    axis.title = element_text (size = 14),
    plot.title = element_text (size = 16),
    strip.text = element_text (size = 14),
    legend.text = element_text (size = 14)
  ) 



fig3_habchange <- hab_change_gg +
  geom_boxplot (aes (y = Common_name,
                     x = log10_foldchange,
                     color = Therm_pref, 
                     #color = mean_Steno,
                     #fill = mean_Steno,
                     #fill = fill_therm,
                     width = 0.85
  )) +
  labs (fill = "",
        y = "",
        x = "Log (Thermal habitat change)") +
  #scale_color_binned (breaks = c (2, 4, 6), low = "orange", high = "blue" ) +
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  #scale_fill_manual (values = alpha (c("lightgray", "blue", "deepskyblue", "red2"), 0.5)) +
  #scale_fill_binned (breaks = c (2, 4, 6), low = "orange", high = "blue" ) +
  #theme (legend.position = c(0.6, 0.9)) +
  #theme (legend.position = c(0.65))
  #theme (legend.title = element_text (size = 18)) +
  #theme (axis.text.x = element_text (face = face_landed)) +
  #ggtitle ("Log x-fold change, 2000-2018 vs. 2061-2080") 
  theme (axis.text.y = element_text (size = 12),
         strip.text = element_text (size = 14),
         axis.text.x = element_text (size = 12),
         axis.title = element_text (size = 14))


ggsave ("Figures/Fig3_Hab_change_TB_boxplot_suitable.eps", fig3_habchange, width = 170, height = 170, units = "mm", dpi = 300)

ggsave ("Figures/Fig3_Hab_change_TB_boxplot_suitable.pdf", fig3_habchange, width = 170, height = 170, units = "mm", dpi = 300)


png (file = "Figures/Fig3_Hab_change_TB_boxplot_suitable.png", width = 16, height = 9, units = "in", res = 300)
print (fig3_habchange)
dev.off()


