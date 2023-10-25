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
                   method, starts_with("successful"), commonwealth.zone, state.zone, status, date, time, site)) %>%   
  dplyr::select(campaignid, sample, location, planned.or.exploratory,longitude, 
                latitude, depth, starts_with("broad"), 
                total.points.annotated) %>%
  dplyr::filter(planned.or.exploratory %in% "MBH") %>%                  
  dplyr::mutate(location = case_when(location %in% "NPZ6" ~ "Abrolhos",
                                     location %in% "NPZ9" ~ "Abrolhos",
                                     location %in% "South-west" ~ "South-west Corner",
                                     .default = location)) %>%
  dplyr::mutate_at(vars(starts_with("broad"), total.points.annotated), ~replace_na(., "0")) %>%
  dplyr::filter(broad.total.points.annotated > 0) %>%
  glimpse()

apollo <- read.csv("data/staging/2021-06-ApolloMP_stereoBRUVs._broad.habitat_points.csv") %>%
  ga.clean.names() %>%
  dplyr::mutate(campaignid = "2021-06-ApolloMP_stereoBRUVs",
                planned.or.exploratory = "MBH") %>%
  dplyr::select(campaignid, sample, longitude, latitude, location, starts_with("broad")) %>%
  dplyr::mutate(total.points.annotated = rowSums(.[,6:(ncol(.))], na.rm = TRUE )) %>%
  glimpse()

beagle <- read.csv("data/staging/Beagle_AMP_Stereo_BRUV_Habitat_NESP_Formatted.csv") %>%
  ga.clean.names() %>%
  dplyr::mutate(campaignid = "Beagle_AMP_Stereo_BRUV") %>%
  dplyr::rename(total.points.annotated = broad.total.points.annotated.minus.openwater) %>%
  dplyr::select(campaignid, sample, longitude, latitude, location, starts_with("broad"), 
                -c(broad.openwater, broad.total.points.annotated)) %>%
  dplyr::mutate_at(vars(starts_with("broad")), ~replace_na(., 0)) %>%
  glimpse()

freycinet <- read.csv("data/staging/Freycinet_202108_Habitat.point.score.csv") %>%
  ga.clean.names() %>%
  dplyr::mutate(campaignid = "Freycinet_202108",
                broad.consolidated = broad.consolidated.rock.turf.mat + 
                  broad.consolidated.rock.veneer,
                broad.invertebrate.complex = broad.invertebrate.complex.complex.1. +
                  broad.invertebrate.complex.complex.2. + broad.invertebrate.complex.complex.3. +
                  broad.invertebrate.complex.complex.4.,
                broad.macroalgae = broad.macroalgae.erect.fine.branching.red +
                  broad.macroalgae.large.canopy.forming.brown,
                broad.sponges = broad.sponges.crusts.encrusting +
                  broad.sponges.erect.forms.branching + broad.sponges.erect.forms.lamiar +
                  broad.sponges.erect.forms.palmate + broad.sponges.hollow.forms.cups.and.alikes +
                  broad.sponges.large.multi.form.10cm. + broad.sponges.massive.forms.simple) %>%
  dplyr::rename(broad.unconsolidated = broad.unconsolidated.pebble.gravel.biogenic.coquina.shellhash) %>%
  dplyr::select(campaignid, sample, longitude, latitude, location, 
                broad.consolidated, broad.invertebrate.complex, broad.macroalgae,
                broad.sponges, broad.unconsolidated) %>%
  dplyr::mutate(total.points.annotated = rowSums(.[,6:(ncol(.))], na.rm = TRUE )) %>%
  glimpse()

unique(dat$campaignid)
unique(dat$location)

write.csv(dat, "data/tidy/NESP-2.1_broad-habitat.csv",
          row.names = F)
