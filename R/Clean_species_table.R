## Clean species list
# 6/17/2020
#JGM

# Species table has parentheses, icelandic names. Clean up so I can match species ID to common name and scientific name. 
library (tidyverse)
library (stringr)

spp_table <- read_csv ("Data/species.csv")

#https://stackoverflow.com/questions/44689640/using-str-extract-all-to-extract-only-first-two-words-in-r
# their command works for most scientific names, but not if there's only one word (Family etc.) need to do an if/else?

spp_table$Scientific_name <- ifelse (
  grepl (" ", spp_table$visindaheiti), 
         word (spp_table$visindaheiti, 1, 2, sep = " "),
  spp_table$visindaheiti
  )

spp_table_eng <- spp_table %>%
  select (tegund, enskt_heiti, Scientific_name) %>%
  rename (Spp_ID = tegund,
          Common_name = enskt_heiti)


write.csv (spp_table_eng, file = "Data/species_eng.csv", row.names = FALSE)
