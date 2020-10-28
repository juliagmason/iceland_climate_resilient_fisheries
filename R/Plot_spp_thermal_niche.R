# Plot Campana et al TB, steno, depth
# 10/27/2020
# JGM

library (tidyverse)

spp_list <- read_csv ("Data/species_eng.csv",
                      col_types = cols(
                        Spp_ID = col_factor()
                      ))

png (file = "Figures/Spp_TB_Steno_depth.png", width = 16, height = 9, units = "in", res = 300)
spp_list %>%
  filter (!is.na (mean_TB)) %>% 
  mutate(depth_fac = case_when (
    between (mean_depth, 0, 300) ~ "shallow",
    between (mean_depth, 300, 800) ~ "midwater",
    mean_depth > 801 ~ "deep"
    )
    ) %>% 
  ggplot (aes (x = mean_TB,
               y = mean_Steno,
               col = depth_fac)) +
  geom_point () +
  geom_text (aes(label = sci_name_underscore)) +
  theme_bw() +
  ggtitle ("Species temperature affinity indices")
dev.off()