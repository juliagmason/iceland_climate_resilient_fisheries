# Clean and tidy Statistics Iceland landings data
# 6/23/2020
# JGM

library (tidyverse)

# first downloaded just community quota ----
cq <- read.csv ("Data/Raw_data/SI_community_quota_landings.csv", sep = ";", skip = 2)

# This has a column for species, quota type and unit, and then a column for each month of each year: X1992.January

cq_long <- cq %>% pivot_longer (
  cols = starts_with("X"), 
  names_to = c ("year", "month"),
  names_prefix = "X",
  #names_sep = ".",
  #names_pattern = "(\\d+)([A-Za-z]+)", # this gives NAs for both
  #names_pattern = "(.+?\\.)(.)", # this gives 1992. J
  #names_pattern = "(.*).(.)(.*)", # this is three groups. (.*).(.) is 1992.Janua y
  names_pattern = "(.*)\\.(.*)", # 
  values_to = "tonnes") 

cq_long$tonnes[which(cq_long$tonnes == "..")] <- NA
cq_long$tonnes <- as.numeric(cq_long$tonnes)

cq_long %>%
  filter (Species == "Cod") %>%
  group_by(year) %>%
  summarize (mn_tonnes = mean (tonnes, na.rm = TRUE)) %>%
  ggplot (aes (x  = year, y = mn_tonnes)) +
  geom_point()+
  geom_line()

cq_long %>%
  group_by (Species, year) %>%
  summarize (tot_cat = sum(tonnes, na.rm = TRUE)) %>%
  arrange (desc (tot_cat)) %>%
  View()

write.csv(cq_long, file= "Data/comm_quota_landings_annual.csv", row.names = FALSE)

# small boat quota types, by species ----
sm_q <- read.csv ("Data/Raw_data/SI_small_quota_spp_catch_value.csv", skip = 2, na.strings = c(".."))

sm_q_long <- sm_q %>%
    pivot_longer (
      cols = contains("20"),
      names_to = c("metric", "year"),
      names_pattern = "(.*)\\.(.*)", # split on last period
      values_to = "amount"
    ) 

head (sm_q_long)
write.csv (sm_q_long, "Data/Landings_annual_spp_small_quota.csv", row.names = FALSE)


# all quota types, annual avg by species  ----
# this is the same as above, but with large boats as well. 
spp_qu <- read.csv ("Data/Raw_data/SI_landings_spp_quota_2003_2019.csv", skip = 2, na.strings = c(".."))

spp_qu_long <- spp_qu %>%
  pivot_longer (
    cols = contains("20"),
    names_to = c("metric", "year"),
    names_pattern = "(.*)\\.(.*)", # split on last period
    values_to = "amount"
  ) %>%
  select (-Region)

write.csv (spp_qu_long, "Data/Landings_annual_spp_quota.csv", row.names = FALSE)

# all quota types, annual averages  ----
#(limited number of rows can download at once. If I need monthly values, may need to do by individual sectors)

annual_landings <- read.csv ("Data/Raw_data/SI_landings_annual.csv", sep = ";", skip = 2, na.strings = c(".."))

# columns are "X1992.Years.total"
lands_long <- annual_landings %>%
  pivot_longer (
    cols = starts_with("X"), 
    names_to = "year",
    names_prefix = "X",
    #names_pattern = "(.*)\\.",
    values_to = "tonnes"
  ) %>%
  # https://stackoverflow.com/questions/42565539/using-strsplit-and-subset-in-dplyr-and-mutate
  mutate (year = as.numeric (sapply (stringr::str_split(year, "\\."), function(x) x[1]))) %>% # remove extra characters
dplyr::select (Species, Quota.type, year, tonnes) %>%
  # rename columns to be compatible with mfri data
  rename (species = Species, quota = Quota.type)

write.csv (lands_long, "Data/Landings_annual_all_sectors.csv", row.names = FALSE)

# catch volume and value by quota type----

quota_catch <- read.csv("Data/Raw_data/SI_landings_quota_type.csv", skip = 2, na.strings = c(".."))
quota_long <- quota_catch %>%
  pivot_longer (
    cols = contains("20"),
    names_to = c("metric", "year"),
    names_pattern = "(.*)\\.(.*)", # split on last period
    values_to = "amount"
  ) %>%
  mutate (metric = ifelse (grepl("X", metric), "1000 ISK", metric)) # get rid of "X1000

# don't know how to do proportions yet, should be easier than this 
val_tot <- quota_long %>%
  dplyr::filter(metric == "1000 ISK") %>%
  group_by (year) %>%
  summarize (total_ISK = sum(amount, na.rm = TRUE))

tonnes_tot <- quota_long %>%
  dplyr::filter(metric == "Tonnes") %>%
  group_by(year) %>%
  summarize (total_tonnes = sum(amount, na.rm = TRUE))

# https://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
quota_val <- quota_long %>%
  dplyr::filter(metric == "1000 ISK") %>%
  group_by (Quota.class, year) %>%
  summarize (year_tot = sum(amount)) %>% 
  left_join (val_tot, by = "year") %>%
  mutate (perc_val = round (year_tot / total_ISK *100, 2))

quota_val_mn <- quota_val %>%
  group_by (Quota.class) %>%
  summarize (mean_val = mean (perc_val))

quota_tonnes <- quota_long %>%
  dplyr::filter(metric == "Tonnes") %>%
  group_by (Quota.class, year) %>%
  summarize (year_tot = sum(amount)) %>% 
  left_join (tonnes_tot, by = "year") %>%
  mutate (perc_wt = round (year_tot / total_tonnes *100, 2))

quota_tonnes_mn <- quota_tonnes %>%
  group_by (Quota.class) %>%
  summarize (mean_wt = mean (perc_wt)) # doesn't add up to 100 though...

quota_tonnes_overall_mn <- quota_long %>%
  dplyr::filter(metric == "Tonnes") %>%
  group_by (Quota.class) %>%
  summarize (tot = sum(amount)) %>%
  mutate (perc_wt = round (tot /sum(tonnes_tot$total_tonnes) * 100, 2))
  
quota_val_overall_mn <- quota_long %>%
  dplyr::filter(metric == "1000 ISK") %>%
  group_by (Quota.class) %>%
  summarize (tot = sum(amount)) %>%
  mutate (perc_wt = round (tot /sum(val_tot$total_ISK) * 100, 2))
