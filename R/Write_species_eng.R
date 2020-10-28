## Clean species list and add other names for matching with landings data
# 6/17/2020
#JGM

### Questions about species identifications ###
# Are the "translations" between common names in spp_names_match (lines 84-99) accurate?
# Why do L. piscatorius and A. minor have multiple species ID codes? monkfish (14, 953), a. minor (13, 913)
# Where would be a better source to see what species are under the quota system? (line 160)


library (tidyverse)
library (stringr)

library (spatstat) # for weighted median and quantiles



# ==================
#### Clean and translate raw spp list ---
# ==================
# Species table has parentheses, icelandic names. Clean up so I can match species ID to common name and scientific name. 

spp_table <- read_csv ("Data/Raw_data/species.csv")
# This has four columns: tegund (species ID number), heiti (Icelandic name), enskt_heiti (English common name), yfir_flokkur (not sure--might be stock category MFRI uses based on data availability), visindaheiti (scientific name with taxonomic source/year)


# keep spp_ID as a number instead of a factor so I can sort

# The scientific names column ('visindaheiti') has notes on classification source and year. In some cases, there are sub-species names. I want to try to just take the first two words, or just one word if it's at the genus/family level. 

#https://stackoverflow.com/questions/44689640/using-str-extract-all-to-extract-only-first-two-words-in-r
# their command works for most scientific names, but not if there's only one word (Family etc.) need to do an if/else?

# If there's a space in the visindaheiti column, take the first two words. NOTE: in some cases this means I'm misidentifying subspecies, but not sure how to do it better because not all of the classifications are in parentheses. 
spp_table$Scientific_name <- ifelse (
  grepl (" ", spp_table$visindaheiti), 
         word (spp_table$visindaheiti, 1, 2, sep = " "),
  spp_table$visindaheiti
  )

# manually fix S. Mentella subspecies
spp_table$Scientific_name[which (spp_table$tegund == 11)] <- "Sebastes mentellaOceanus"

# translate column headers and arrange in order of species ID
spp_table_eng <- spp_table %>%
  dplyr::select (tegund, enskt_heiti, Scientific_name) %>%
  rename (Spp_ID = tegund,
          Common_name = enskt_heiti) %>%
  arrange (Spp_ID)

# some have common name but not species name

# ==================
# add landings data landed names to be able to join landings data ----
# ==================

# This is from Statistics Iceland, selected all species, aggregated by region, month. 1992-2018.
# https://px.hagstofa.is/pxen/pxweb/en/Atvinnuvegir/Atvinnuvegir__sjavarutvegur__aflatolur__fiskveidisvaedi/SJA09001.px
ldgs <- read_csv ("Data/Landings_annual_all_sectors.csv")

prelim_names_match <- data.frame (Landed_name =  sort (unique (ldgs$species)),
                                  Common_name = sort (unique (ldgs$species))) %>% # second decoy column for joining
  left_join (spp_table_eng, by = "Common_name")

# this now has 42? two types of redfish 
length (which (is.na (prelim_names_match$Spp_ID))) # 17 problem species where Statistics Iceland common name doesn't match MFRI common name

prelim_names_match[which(is.na (prelim_names_match$Spp_ID)),]


# Grenadier--many different spp of grenadier in survey; only. one in landings. Two types of mackerel in survey; none in landings. What about ocean redfish? fasciatus?? shrimp is deepwater prawn??

# check which species of grenadier is most common in survey data
# grens <- spp_list %>%
#   filter (grepl ("grenadier", Common_name))

# comb_ds %>%
#   filter (species %in% grens$Spp_ID) %>%
#   group_by (species) %>%
#   summarize (tot_cat = sum(kg_per_nautmile, na.rm = TRUE)) %>%
#   arrange (desc (tot_cat))
# only 10 and 62, and 10 is way more common--247 total vs 13.3

#### Notes from Pamela: ----
# I see – those landings are from Hagstofa (Statistics Iceland) which are not always the same as Fiskistofa (Fisheries Directorate) and they lump all redfish together. You can try querying on the Fiskistofa database if you want, or just let me know what you want and we can pass landings data to you (ours should be on par with Fiskistofa, Hagstofa is less accurate).

# I also checked with our redfish guy and he said the same as Magnús and that we don’t really track 11 with the surveys – the surveys don’t really cover its range. I also forgot to mention that the autumn survey is mainly for deeper water species, so we use it mainly to track Greenland halibut, redfish, blue ling, and greater silver smelt (and I’m probably forgetting some but you can check in the advice sheets on (https://www.hafogvatn.is/en/harvesting-advice), and most of the others are tracked using the spring survey. Many may have enough numbers to be tracked in both, but they sometimes have differing patterns. Many of the noncommercial species are never looked at.

#The vast majority of grenadier landings are Coryphaenoides rupestrus, with a little Macrourus berglax. These are probably also lumped by Hagstofa.

# All mackeral landings should be Scomber scombrus. T. trachurus is not caught around here as far as I know, but maybe incidentally in some pelagic surveys. 


# Notes from Valtysson: 
# Species 91 (Lycodes rossi) has been combined with another, species 59 (Lycodes reticulatus). Originally they were thought to be two species but are now considered the same.

# More on the redfish complex. The Statice data is not good for redfishes. It has 2 categories; redfish and ocean redfish. Ocean redfish is the oceanic stock of Sebastes mentella, it is fished with pelagic trawl. Redfish is both Sebastes norvegicus and demersal S. mentella. Both fished with bottom trawls in the same areas but at slightly different depths. They were not seperated in catches previously ansd a combined TAC was given, they are now seperated in catches and have seperate TAC. Statice just has not updated its system. The Marine Research Institute has seperated the previous catch into species, see https://www.hafogvatn.is/en/moya/extras/categories/radgjof/djupkarfi and https://www.hafogvatn.is/en/moya/extras/categories/radgjof/karfi. Then there is the third species, S. viviparous, in Statice it is among others in the category "Other demersal fishes"

# make a reference table to match landing common names to spp_table common names
spp_names_match <- prelim_names_match %>%
  mutate (Common_name =
            case_when (Landed_name == "Atlantic catfish" ~ "Atlantic wolffish",
                       Landed_name == "Blue ling" ~ "Blue ling, European ling",
                       Landed_name == "Grenadier" ~ "Roundnose grenadier",
                       Landed_name == "Mackerel" ~ "Atlantic mackerel",
                       Landed_name == "Monk" ~ "Monkfish",
                       Landed_name == "Norwegian spring-spawning herring" ~ "Herring/Norway-Icelandic",
                       Landed_name == "Ocean redfish" ~ "Deep sea redfish", # This should be spp_ID 11, from Valtysson. changed from ocean redfish based on fiskistofa website and that 61 is caught each year http://www.fiskistofa.is/english/quotas-and-catches/catches-in-individual-species/
                       Landed_name == "Shrimp" ~ "Deep water prawn",
                       Landed_name == "Silver smelt" ~ "Greater argentine",
                       Landed_name == "Spiny dogfish" ~ "Dogfish",
                       Landed_name == "Spotted wolffish" ~ "Spotted wolffish, leopardfish",
                       #https://dfo-mpo.gc.ca/species-especes/profiles-profils/spottedwolf-louptachete-eng.html
                       Landed_name == "Starry ray" ~ "Starry ray, thorny skate",
                       Landed_name == "Tusk" ~ "Tusk, torsk, cusk",
                       TRUE ~ Common_name
                       
            )
  ) %>% 
  filter (Spp_ID != 905 | is.na(Spp_ID)) %>% # get rid of problematic repeated redfish
  dplyr::select (Landed_name, Common_name) 


#write.csv (spp_names_match, "Data/Match_spp_names.csv", row.names = FALSE)

spp_table_ldgs <- spp_table_eng %>%
  left_join (spp_names_match, by = "Common_name") %>%
  # add underscore in sci name to match Presence/Absence table. fill in with common name if no scientific name
  mutate (sci_name_underscore = ifelse (
    is.na (Scientific_name), gsub (" ", "_", Common_name),
           gsub(" ", "_", Scientific_name)
  )
    ) %>% 
  filter (! Spp_ID %in% c(953, 613, 913)) # just take out repeated common name species

# fix repeated spp
# have identified 3 so far that are commonly caught: S. mentella (11, 61), monkfish (953, 14), seminudus (69, 613)--this one is a subspecies, a. minor (13, 913)

# no instances of 953 or 613, 913. 

#comb_ds %>% filter (species %in% c(69, 613)) %>% group_by (species) %>% summarize (tot = sum(kg_per_nautmile, na.rm = TRUE))

# ==================
# Other helper columns for categorizing species ----
# ==================

# ==================
# count how many years each species has samples for each season ----
# survey data
comb_ds <- read_csv ("Data/MFRI_comb_survey.csv",
                     col_types = cols(
                       sample_id = col_factor(), 
                       #species = col_factor(), # let it interpret species as a number so can join
                       stat_sq = col_factor())
                     )


spp_yrs_sampled <- comb_ds %>%
  group_by (species, season) %>%
  summarise (n_yrs = length (unique (year))) %>%
  pivot_wider (
    names_from = season,
    values_from = n_yrs
  ) %>%
  rename (Spp_ID = species,
          n_autumn = autumn,
          n_spring = spring) %>%
  replace_na (list(n_autumn = 0, n_spring = 0)) %>%
  mutate (Spp_ID = as.numeric (Spp_ID))


# ==================
# add column for whether in quota system or not ----

# http://www.fiskistofa.is/english/quotas-and-catches/catches-in-individual-species/ drop down menu
# Need to refine with better catch data from Valtysson
# Lumpfish (48) definitely should not be included
quota_spp <- c(1:19, 21:28, 30:36, 40:47, 49, 60:63, 77, 78, 86, 95, 130, 173, 191, 199, 210, 235, 279, 286, 689, 703, 949, 950, 951) 


# ==================
# add thermal bias and steno index, from Campana et al. 2020 ----
# Thermal bias is median catch-weighted temperature of species, all years - median bottom water temp of all stations. 
# Steno index: range of 5th adn 95th percentile catch-weighted temperature, all years

mfri_abun <- read_csv ("Data/MFRI_comb_survey.csv",
                       col_types = cols(
                         sample_id = col_factor(),
                         species = col_factor(),
                         stat_sq = col_factor()
                       )
)%>%
  filter (!(year == 2011 & season == "autumn"),
          !(year < 2000 & season == "autumn")) %>% # remove autumn 2011 [labor strike] and pre-2000 autumn [sites weren't standard yet]
  group_by (sample_id, species) %>%
  summarize (n_tot = sum(n_per_nautmile),
             kg_tot = sum(kg_per_nautmile)) %>%
  mutate (n_log = log(n_tot),
          kg_log = log(kg_tot))

# predictor variables
# I compiled these in Build_MFRI_predictor_df.R. 
mfri_pred <-  read_csv ("Data/MFRI_predictor_df.csv",
                        col_types = cols(
                          sample_id = col_factor(),
                          stat_sq = col_factor(),
                          bormicon_region = col_factor(),
                          sst_dev = col_number(),
                          bt_dev = col_number(),
                          sst_max = col_number(),
                          sst_min = col_number(),
                          bt_max = col_number(),
                          bt_min = col_number()
                        )
) %>%
  filter (!(year == 2011 & season == "autumn"),
          !(year < 2000 & season == "autumn")
  ) 

# overall median bottom temp
med_bw <- median (mfri_pred$bottom_temp, na.rm = TRUE)
med_seas <- mfri_pred %>%
  group_by (season) %>%
  summarize (med = median (bottom_temp, na.rm = TRUE))

# calculate thermal index for each species. Since temperature distributions are different in the two seasons and Campana only did autumn, separate by season and take a mean
spp_therm <- mfri_abun %>%
  left_join (mfri_pred, by = "sample_id") %>%
  filter (!is.na (bottom_temp)) %>%
  group_by (species, season) %>%
  summarise (Therm = weighted.median(bottom_temp, w = kg_tot, na.rm = TRUE),
             Steno = weighted.quantile (bottom_temp, w = kg_tot, probs = 0.95, na.rm = TRUE) - weighted.quantile (bottom_temp, w = kg_tot, probs = 0.05, na.rm = TRUE),
             Depth = weighted.median (tow_depth_begin, w = kg_tot, na.rm = TRUE)
  ) %>%
  left_join (med_seas, by = "season") %>%
  mutate (TB = Therm - med) %>%
  group_by(species) %>%
  summarise (mean_TB = mean (TB),
             mean_Steno = mean(Steno),
             mean_depth = mean(Depth)) %>%
  mutate (Spp_ID = as.numeric(as.character(species)), .keep = "unused") # rename to match other tables

# campana only did years 1996-2018, only autumn?
spp_therm_campana <- mfri_abun %>%
  left_join (mfri_pred, by = "sample_id") %>%
  filter (between (year, 1996, 2018), season == "autumn") %>% length (unique (station))
  group_by (species) %>%
  summarise (
    TB = (weighted.median(bottom_temp, w = kg_tot, na.rm = TRUE) - med_bw),
             Steno = weighted.quantile (bottom_temp, w = kg_tot, probs = 0.95, na.rm = TRUE) - weighted.quantile (bottom_temp, w = kg_tot, probs = 0.05, na.rm = TRUE),
             Depth = weighted.mean (tow_depth_begin, w = kg_tot, na.rm = TRUE)
  ) %>%
  arrange (as.numeric(species))

  
  
# ==================
# Compile and write csv ----
# ==================
  spp_table_cat <- spp_table_ldgs %>%
    left_join (spp_yrs_sampled, by = "Spp_ID") %>%
    arrange (Spp_ID) %>%
    mutate (Quota = ifelse (Spp_ID %in% quota_spp, 1, 0)) %>%
    left_join (spp_therm, by = "Spp_ID")
    

  
  
  write.csv (spp_table_cat, file = "Data/species_eng.csv", row.names = FALSE)
