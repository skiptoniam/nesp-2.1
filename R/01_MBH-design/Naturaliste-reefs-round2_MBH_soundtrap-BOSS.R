###
# Project: NESP 2.1
# Data:    GA 250m Bathymetry
# Task:    Create a sampling plan - chosen from previous sample design completed for regular BOSS
# author:  Claude Spencer
# date:    June 2023
##

# Clear memory
rm(list=ls()) 

library(tidyverse)

round1 <- read.csv("data/metadata/2023-06_Nat-reef/2023-06_Naturaliste-Reefs_BOSS_metadata.csv") %>%
  dplyr::select(x, y, Sample) %>%
  dplyr::filter(Sample %in% c("DC03", "DC07", "DC31", "DCC01", "DCC07", "DCC11")) %>%
  dplyr::rename(old.sample = Sample) %>%
  glimpse()

samp <- round1 %>%
  dplyr::mutate(sample = paste("ST", 
                               str_pad(row_number() + 6, 2,                     
                                       side = "left", pad = "0") , sep = "")) %>%
  glimpse()


## Write out sampling file 
write.csv(samp,"data/mbh-design/NATREEF_ROUND2_SOUNDTRAP-BOSS_MBH_wgs84.csv", row.names = F) # write out the each region


