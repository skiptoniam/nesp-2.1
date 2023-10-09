# Clear the environment of large items
rm(list = ls())

library(tidyverse)

dat <- list.files(path = "data/staging",
                  pattern = "_broad.habitat.csv",
                  full.names = T,
                  recursive = T) %>%
  purrr::map_dfr(~read_csv(., col_types = cols(.default = "c"))) %>%
  dplyr::mutate(planned.or.exploratory = if_else(campaignid %in% "2021-03_West-Coast_BOSS", 
                                                 "MBH", planned.or.exploratory)) %>%
  dplyr::select(-c(broad.kelps, broad.reef, starts_with("fov"), mean.relief, sd.relief, 
                   method, starts_with("successful"), commonwealth.zone, state.zone, status)) %>%                       
  dplyr::filter(planned.or.exploratory %in% "MBH") %>%                          # 2020-10_BOSS campaign not recorded MBH for - too difficult to work out and already have good sample coverage here
  dplyr::mutate(location = case_when(location %in% "NPZ6" ~ "Abrolhos",
                                     location %in% "NPZ9" ~ "Abrolhos",
                                     location %in% "South-west" ~ "South-west Corner",
                                     .default = location)) %>%
  dplyr::mutate_at(vars(starts_with("broad")), ~replace_na(., "0")) %>%
  glimpse()

unique(dat$campaignid)
unique(dat$location)

write.csv(dat, "data/tidy/NESP-2.1_UWA_broad-habitat.csv",
          row.names = F)
