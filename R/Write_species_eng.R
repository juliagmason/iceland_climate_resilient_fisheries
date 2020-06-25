## Clean species list and add other names for matching with landings data
# 6/17/2020
#JGM

# Species table has parentheses, icelandic names. Clean up so I can match species ID to common name and scientific name. 
library (tidyverse)
library (stringr)

spp_table <- read_csv ("Data/Raw_data/species.csv")

#https://stackoverflow.com/questions/44689640/using-str-extract-all-to-extract-only-first-two-words-in-r
# their command works for most scientific names, but not if there's only one word (Family etc.) need to do an if/else?

spp_table$Scientific_name <- ifelse (
  grepl (" ", spp_table$visindaheiti), 
         word (spp_table$visindaheiti, 1, 2, sep = " "),
  spp_table$visindaheiti
  )

spp_table_eng <- spp_table %>%
  dplyr::select (tegund, enskt_heiti, Scientific_name) %>%
  rename (Spp_ID = tegund,
          Common_name = enskt_heiti) %>%
  arrange (Spp_ID)

# some have common name but not species name

# add landings data landed names ----
ldgs <- read_csv ("../Data/Landings_annual_all_sectors.csv")

prelim_names_match <- data.frame (Landed_name =  sort (unique (ldgs$species)),
                                  Common_name = sort (unique (ldgs$species))) %>% # second decoy column for joining
  left_join (spp_list, by = "Common_name")

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
  mutate (sci_name_underscore = gsub(" ", "_", Scientific_name)) %>% # add underscore to match PA table
  filter (! Spp_ID %in% c(11, 953, 613))

# fix repeated spp
# have identified 3 so far that are commonly caught: S. mentella (11, 61), monkfish (953, 14), seminudus (69, 613)

# no instances of 953 or 613. 

comb_ds %>% filter (species %in% c(69, 613)) %>% group_by (species) %>% summarize (tot = sum(kg_per_nautmile, na.rm = TRUE))

write.csv (spp_table_ldgs, file = "Data/species_eng.csv", row.names = FALSE)
