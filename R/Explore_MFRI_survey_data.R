## Explore MFRI survey data
# 6/16/2020
#JGM

library (tidyverse)

# spring survey data---
#uploaded 6/16 

spr_ds <- read_csv ("Data/springsurvey.csv",
                    col_types = cols(
                      species = col_factor(),
                      stat_sq = col_factor() 
      
                    ))

str(spr_ds)
