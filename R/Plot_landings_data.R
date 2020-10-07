# Plot landings data
# 9/28/2020
# JGM

library (tidyverse)

# What are the most important landed species by volume and value for the different sectors over the last decade?

# this is all quota types, all species, with tonnes and value
lands_qu <- read.csv("Data/Landings_annual_spp_quota.csv")

spp_list <- read_csv ("Data/species_eng.csv")
# Per carothers 2017 and hab change plots, combine hook and line with small boats,  also going to combine trawlers with large ITQ

spp_by_qu <- lands_qu %>%
  filter (year >= 2010, 
          Quota.class != "Small boats with limited fishing days") %>%
  mutate (Quota.class = case_when (
    Quota.class == "Hook and line boats with catch quota" ~ "Small boats with catch quota", 
    Quota.class == "Trawlers" ~ "Vessels with catch quota",
    TRUE ~ Quota.class
    ),
    amount = ifelse (
      metric == "X1000.ISK",
      amount / 1000, amount
      ), 
    metric = ifelse(
      metric == "X1000.ISK", 
      "ISK (millions)", metric
      )
    ) %>%
      group_by (Species, Quota.class, metric) %>%
      summarize (mn_val = mean (amount, na.rm = TRUE), 
                 sd_val = sd(amount, na.rm = TRUE)
                 ) %>%
  rename (Landed_name = Species) %>%
  left_join (spp_list, by = "Landed_name")

# idea from reading Woods et al. 2015--which are the highest-value species? e.g. price per ton ----
png (file = "Figures/Landings_spp_value_per_tonne.png", width = 16, height = 9, units = "in", res = 300)
lands_qu %>%
  filter (year >= 2010) %>%
  group_by (Species, metric) %>%
  summarize (mn_val = mean (amount, na.rm = TRUE)) %>% 
  pivot_wider (
    names_from = metric, 
    values_from = mn_val
  ) %>% 
  mutate (mkt_val = (X1000.ISK/1000)/Tonnes) %>% 
  ggplot (aes (x = reorder (Species, mkt_val), y = mkt_val)) +
  geom_bar (stat = "identity") +
  theme_bw() +
  theme(
    axis.text = element_text (size = 12),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    axis.title = element_text (size = 14),
    plot.title = element_text (size = 16),
    strip.text = element_text (size = 14)
  ) +
  labs (y = "Mean annual value, million ISK/tonne", x = "") +
  
  ggtitle ("Species value per tonne")
dev.off()

# Split out quota sectors and plot major species by volume and value ----
spp_cf_order <- spp_by_qu %>%
  filter (Quota.class == "Coastal fisheries", 
          metric == "ISK (millions)") %>%
  arrange (desc (mn_val))

png (file = "Figures/Landings_Coastal_fisheries.png", width = 16, height = 9, units = "in", res = 300)
spp_by_qu %>%
  filter (Quota.class == "Coastal fisheries", !is.na (mn_val)) %>% 
  mutate (Scientific_name = factor(Scientific_name, levels = spp_cf_order$Scientific_name)) %>%
  ggplot (aes (x = Scientific_name, y = mn_val)) +
  geom_bar(stat = "identity") +
  geom_errorbar (aes (ymin = mn_val - sd_val, ymax = mn_val + sd_val)) +
  facet_wrap (~metric, scales = "free", dir = "v") +
  theme_bw() +
  theme(
    axis.text = element_text (size = 12),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    axis.title = element_text (size = 14),
    plot.title = element_text (size = 16),
    strip.text = element_text (size = 14)
    ) +
  labs (y = "Mean annual value", x = "") +
  
    ggtitle ("Coastal fisheries")
dev.off()

# reorder outside. need to find better way of doing this
spp_itq_order <- spp_by_qu %>%
  filter (Quota.class == "Vessels with catch quota", 
          metric == "ISK (millions)") %>%
  arrange (desc (mn_val))

png (file = "Figures/Landings_Large_ITQ.png", width = 16, height = 9, units = "in", res = 300)
spp_by_qu %>%
  filter (Quota.class == "Vessels with catch quota", !is.na (mn_val)) %>% 
  mutate (Scientific_name = factor(Scientific_name, levels = spp_itq_order$Scientific_name)) %>%
  ggplot (aes (x = Scientific_name, y = mn_val)) +
  geom_bar(stat = "identity") +
  geom_errorbar (aes (ymin = mn_val - sd_val, ymax = mn_val + sd_val)) +
  facet_wrap (~metric, scales = "free", dir = "v") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.text = element_text (size = 16),
    axis.title = element_text (size = 18),
    plot.title = element_text (size = 24),
    strip.text = element_text (size = 18)
  ) +
  labs (y = "Mean annual value", x = "") +
  
  ggtitle ("Large boat ITQ")
dev.off()

spp_smitq_order <- spp_by_qu %>%
  filter (Quota.class == "Small boats with catch quota", 
          metric == "ISK (millions)") %>%
  arrange (desc (mn_val))

png (file = "Figures/Landings_Small_ITQ.png", width = 16, height = 9, units = "in", res = 300)
spp_by_qu %>%
  filter (Quota.class == "Small boats with catch quota", !is.na (mn_val)) %>% 
  mutate (Scientific_name = factor(Scientific_name, levels = spp_smitq_order$Scientific_name)) %>%
  ggplot (aes (x = Scientific_name, y = mn_val)) +
  geom_bar(stat = "identity") +
  geom_errorbar (aes (ymin = mn_val - sd_val, ymax = mn_val + sd_val)) +
  facet_wrap (~metric, scales = "free", dir = "v") +
  theme_bw() +
  theme(
    axis.text = element_text (size = 12),
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
    axis.title = element_text (size = 14),
    plot.title = element_text (size = 16),
    strip.text = element_text (size = 14)
  ) +
  labs (y = "Mean annual value", x = "") +
  
  ggtitle ("Small boat ITQ")
dev.off()