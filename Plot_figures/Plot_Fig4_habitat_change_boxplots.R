# Plot habitat change boxplots
# Figure 4 in manuscript

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

# gam performance to indicate suitable species 
MASE <- read_csv("Models/GAM_performance_Rug_tw_LL.csv")

# ==================
# load and plot habitat change ----
# ==================

# load habitat change df, made in Step3_Calculate_habitat_change.R
load ("Data/hab_change_rug_tw_df.RData")

load ("Models/spp_bormicon.RData")

# species-by-species change ----

# Looking at various metrics. Percent change is the most common, but hard to interpret when negative and positive changes are very large. Makes positive changes look way more important than negative changes. 


# rugosity med, combine with molva and hippo crop
# load ("Data/hab_change_rug_df_MmHp_crop.RData")
# load ("Data/hab_change_rug_df.RData")
hab_change_rug <- hab_change_rug %>%
  # filter (!species %in% c("Molva_molva", "Hippoglossoides_platessoides")) %>%
  # rbind (hab_change_rug_MmHp_crop) %>%
  mutate(Common_name = case_when (
    Common_name == "Lycodes eudipleurostictus" ~ "Doubleline eelpout",
    Common_name == "Lycodes seminudus" ~ "Longear eelpout",
    Common_name == "Rabbitfish (rat fish)" ~ "Rabbitfish",
    TRUE ~ Common_name)
  )




# make a ggplot unit that I can manipulate for different uses ----
# can manipulate boxplot aesthetics, col and fill values, legend position, title. Labs command gets overridden, so put in subsequent plotting code

# legend position: c(0.83, 0.15) works well for percent change, c(0.6, 0.9) for log10 fold change 

# re-label scenarios
scen_labs <- c("SSP 2-4.5", "SSP 5-8.5")
names (scen_labs) <- c(245, 585) 

hab_change_gg <- hab_change_rug %>%
  filter (!species %in% c("Squalus_acanthias", "Icelus_bicornis")) %>%
  left_join (MASE, by = "species") %>%
  mutate (
          # column for fill based on thermal preference
          Therm_pref = case_when (
            mean_TB > 0 ~ "Warm",
            between (mean_TB, -3, 0) ~ "Cool",
            mean_TB < 0 ~ "Cold"
          ),
          
          # reorder by desired unit of change
          species = fct_reorder (species, log10_foldchange),
          # also reorder common_name for changing labels
          Common_name = fct_reorder (Common_name, log10_foldchange)
  ) %>% # end mutate
        

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



fig4_habchange <- hab_change_gg +
  geom_boxplot (aes (y = Common_name,
                     x = log10_foldchange,
                     color = Therm_pref, 
                     width = 0.85
  )) +
  labs (fill = "",
        y = "",
        x = "Log (Thermal habitat change)") +
  scale_color_manual (values = c("blue", "deepskyblue", "red2")) +
  theme (axis.text.y = element_text (size = 12),
         strip.text = element_text (size = 14),
         axis.text.x = element_text (size = 12),
         axis.title = element_text (size = 14))


ggsave ("Figures/Fig4_Hab_change_TB_boxplot_suitable.eps", fig4_habchange, width = 170, height = 170, units = "mm", dpi = 300)

ggsave ("Figures/Fig4_Hab_change_TB_boxplot_suitable.png", fig4_habchange, width = 170, height = 170, units = "mm", dpi = 300)
