# Clean and tidy Statistics Iceland landings data
# 6/23/2020
# JGM

library (tidyverse)

# first downloaded just community quota
cq <- read.csv ("../Data/Raw_data/SI_community_quota_landings.csv", sep = ";", skip = 2)

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
  group_by (Species) %>%
  summarize (tot_cat = sum(tonnes, na.rm = TRUE)) %>%
  arrange (desc (tot_cat)) %>%
  View()

# all quota types, annual averages (limited number of rows can download at once. If I need monthly values, may need to do by individual sectors)

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
  mutate (year = as.numeric (strsplit(year, "\\.")[[1]][1])) %>% # remove extra characters
dplyr::select (Species, Quota.type, year, tonnes) %>%
  # rename columns to be compatible with mfri data
  rename (species = Species, quota = Quota.type)

write.csv (lands_long, "Data/Landings_annual_all_sectors.csv", row.names = FALSE)
