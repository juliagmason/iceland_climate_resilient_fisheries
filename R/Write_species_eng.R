## Clean species list and add other names for matching with landings data
# 6/17/2020
#JGM


#### Notes from Pamela: ----
# I see – those landings are from Hagstofa (Statistics Iceland) which are not always the same as Fiskistofa (Fisheries Directorate) and they lump all redfish together. You can try querying on the Fiskistofa database if you want, or just let me know what you want and we can pass landings data to you (ours should be on par with Fiskistofa, Hagstofa is less accurate).

# I also checked with our redfish guy and he said the same as Magnús and that we don’t really track 11 with the surveys – the surveys don’t really cover its range. I also forgot to mention that the autumn survey is mainly for deeper water species, so we use it mainly to track Greenland halibut, redfish, blue ling, and greater silver smelt (and I’m probably forgetting some but you can check in the advice sheets on (https://www.hafogvatn.is/en/harvesting-advice), and most of the others are tracked using the spring survey. Many may have enough numbers to be tracked in both, but they sometimes have differing patterns. Many of the noncommercial species are never looked at.

#The vast majority of grenadier landings are Coryphaenoides rupestrus, with a little Macrourus berglax. These are probably also lumped by Hagstofa.

# All mackeral landings should be Scomber scombrus. T. trachurus is not caught around here as far as I know, but maybe incidentally in some pelagic surveys. 
                                                                                                                                                                                                                                                                                                                                                    
##### Clean and translate raw spp list
# Species table has parentheses, icelandic names. Clean up so I can match species ID to common name and scientific name. 
library (tidyverse)
library (stringr)

spp_table <- read_csv ("Data/Raw_data/species.csv")
# keep spp_ID as a number so I can arrange

# The scientific names column ('visindaheiti') has notes on classification source and year. In some cases, there are sub-species names. I want to try to just take the first two words, or just one word if it's at the genus/family level. 

#https://stackoverflow.com/questions/44689640/using-str-extract-all-to-extract-only-first-two-words-in-r
# their command works for most scientific names, but not if there's only one word (Family etc.) need to do an if/else?

# If there's a space in the visindaheiti column, take the first two words.
spp_table$Scientific_name <- ifelse (
  grepl (" ", spp_table$visindaheiti), 
         word (spp_table$visindaheiti, 1, 2, sep = " "),
  spp_table$visindaheiti
  )

# translate column headers and arrange in order of species ID
spp_table_eng <- spp_table %>%
  dplyr::select (tegund, enskt_heiti, Scientific_name) %>%
  rename (Spp_ID = tegund,
          Common_name = enskt_heiti) %>%
  arrange (Spp_ID)

# some have common name but not species name

# add landings data landed names to be able to join landings data ----
ldgs <- read_csv ("Data/Landings_annual_all_sectors.csv")

prelim_names_match <- data.frame (Landed_name =  sort (unique (ldgs$species)),
                                  Common_name = sort (unique (ldgs$species))) %>% # second decoy column for joining
  left_join (spp_table_eng, by = "Common_name")

# this now has 42? two types of redfish 
length (which (is.na (prelim_names_match$Spp_ID))) # 17 problem kids

prelim_names_match[which(is.na (prelim_names_match$Spp_ID)),]

# many different spp of grenadier in survey; one in landings, two types of mackerel. ocean redfish? fasciatus?? shrimp is deepwater prawn??
# grens <- spp_list %>%
#   filter (grepl ("grenadier", Common_name))

# comb_ds %>%
#   filter (species %in% grens$Spp_ID) %>%
#   group_by (species) %>%
#   summarize (tot_cat = sum(kg_per_nautmile, na.rm = TRUE)) %>%
#   arrange (desc (tot_cat))
# only 10 and 62, and 10 is way more--247 total vs 13.3

# make a reference table to match landing common names to spp_table common names
spp_names_match <- prelim_names_match %>%
  mutate (Common_name =
            case_when (Landed_name == "Atlantic catfish" ~ "Atlantic wolffish",
                       Landed_name == "Blue ling" ~ "Blue ling, European ling",
                       Landed_name == "Grenadier" ~ "Roundnose grenadier",
                       Landed_name == "Mackerel" ~ "Atlantic mackerel",
                       Landed_name == "Monk" ~ "Monkfish",
                       Landed_name == "Norwegian spring-spawning herring" ~ "Herring/Norway-Icelandic",
                       Landed_name == "Ocean redfish" ~ "Deep sea redfish", # not sure about this one
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
  mutate (sci_name_underscore = ifelse (
    is.na (Scientific_name), gsub (" ", "_", Common_name),
           gsub(" ", "_", Scientific_name)
  )
    ) %>% # add underscore to match PA table. fill in with common name if no scientific name
  filter (! Spp_ID %in% c(11, 953, 613, 913)) # just take out repeated common name species, incl. s. mentella subspecies

# fix repeated spp
# have identified 3 so far that are commonly caught: S. mentella (11, 61), monkfish (953, 14), seminudus (69, 613), a. minor (13, 913)

# no instances of 953 or 613, 913. 

#comb_ds %>% filter (species %in% c(69, 613)) %>% group_by (species) %>% summarize (tot = sum(kg_per_nautmile, na.rm = TRUE))

# add columns with number of years sampled and whether landed----

comb_ds <- read_csv ("Data/MFRI_comb_survey.csv",
                     col_types = cols(
                       sample_id = col_factor(), 
                       #species = col_factor(), # let it interp species as a number so can join
                       stat_sq = col_factor())
)


# count how many years each species has samples for each season
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
  replace_na (list(n_autumn = 0, n_spring = 0))



# join to spp_table by species
spp_table_nyrs <- spp_table_ldgs %>%
  left_join (spp_yrs_sampled, by = "Spp_ID") %>%
  arrange (Spp_ID)

write.csv (spp_table_nyrs, file = "Data/species_eng.csv", row.names = FALSE)
