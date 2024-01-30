# Clear the environment of large items
rm(list = ls())

library(tidyverse)

dat <- list.files(path = "data/staging",
                  pattern = "_broad.habitat.csv",
                  full.names = T,
                  recursive = T) %>%
  purrr::map_dfr(~read_csv(., col_types = cols(.default = "c"))) %>%
  dplyr::mutate(ifelse(is.na(total.points.annotated), broad.total.points.annotated, total.points.annotated)) %>%
  dplyr::select(-c(broad.kelps, broad.reef, starts_with("fov"), mean.relief, sd.relief, 
                   method, starts_with("successful"), commonwealth.zone, state.zone, 
                   status, date, time, site, broad.total.points.annotated, total.points.annotated)) %>%   
  dplyr::select(campaignid, sample, location, planned.or.exploratory,longitude, 
                latitude, depth, starts_with("broad")) %>%
  dplyr::mutate(location = case_when(location %in% "NPZ6" ~ "Abrolhos",
                                     location %in% "NPZ9" ~ "Abrolhos",
                                     location %in% "South-west" ~ "South-west Corner",
                                     .default = location)) %>%
  dplyr::mutate_at(vars(starts_with("broad")), ~replace_na(., "0")) %>%
  dplyr::mutate_at(vars(starts_with("broad")), as.numeric) %>%
  dplyr::mutate(total.points.annotated = rowSums(.[,8:(ncol(.))],na.rm = TRUE )) %>%
  dplyr::mutate(across(everything(), as.character)) %>%
  dplyr::filter(total.points.annotated > 0) %>%
  glimpse()

unique(dat$campaignid)

apollo <- read.csv("data/staging/2021-06-ApolloMP_stereoBRUVs._broad.habitat_points.csv") %>%
  ga.clean.names() %>%
  dplyr::mutate(campaignid = "2021-06-ApolloMP_stereoBRUVs",
                planned.or.exploratory = "MBH") %>%
  dplyr::select(campaignid, sample, longitude, latitude, location,  planned.or.exploratory,starts_with("broad")) %>%
  dplyr::mutate(total.points.annotated = rowSums(.[,7:(ncol(.))], na.rm = TRUE )) %>%
  dplyr::mutate(across(everything(), as.character)) %>%
  glimpse()

beagle <- read.csv("data/staging/Beagle_AMP_Stereo_BRUV_Habitat_NESP_Formatted.csv") %>%
  ga.clean.names() %>%
  dplyr::mutate(campaignid = "Beagle_AMP_Stereo_BRUV") %>%
  dplyr::rename(total.points.annotated = broad.total.points.annotated.minus.openwater) %>%
  dplyr::mutate(broad.unconsolidated = broad.unconsolidated.sand + broad.unconsolidated.bivalves +
                  broad.unconsolidated.screwshells + broad.unconsolidated.biogenic) %>%
  dplyr::select(campaignid, sample, longitude, latitude, location, planned.or.exploratory, starts_with("broad"), 
                -c(broad.openwater, broad.total.points.annotated, broad.unconsolidated.sand, 
                   broad.unconsolidated.bivalves, broad.unconsolidated.screwshells, broad.unconsolidated.biogenic)) %>%
  dplyr::mutate_at(vars(starts_with("broad")), ~replace_na(., 0)) %>%
  dplyr::mutate(across(everything(), as.character)) %>%
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
  dplyr::select(campaignid, sample, longitude, latitude, location, planned.or.exploratory,
                broad.consolidated, broad.invertebrate.complex, broad.macroalgae,
                broad.sponges, broad.unconsolidated) %>%
  dplyr::mutate(total.points.annotated = rowSums(.[,7:(ncol(.))], na.rm = TRUE )) %>%
  dplyr::mutate(across(everything(), as.character)) %>%
  glimpse()

final.dat <- bind_rows(apollo, beagle, dat, freycinet) %>%
  dplyr::mutate_at(vars(starts_with("broad")), ~replace_na(., "0")) %>%
  dplyr::filter(planned.or.exploratory %in% "MBH") %>%
  glimpse()

subset(final.dat,select=c(broad.seagrasses,broad.seagrasses_amphibolis.sp.,broad.seagrasses_posidonia.sp.,broad.seagrasses_halophila.sp.,broad.seagrasses_rupia.sp.,broad.seagrasses_zostera.sp.))


final.dat2 <- final.dat %>% 
              dplyr::mutate(broad.sessile.inverts = rowSums(apply(.[c(7,8,10,11,15,17,18,20,21,27,28,29)],2,as.numeric),na.rm = TRUE))
 
                       
broad.seagrass <- rowSums(apply(subset(final.dat2,select=c(broad.seagrasses,broad.seagrasses_amphibolis.sp.,broad.seagrasses_posidonia.sp.,broad.seagrasses_halophila.sp.,broad.seagrasses_rupia.sp.,broad.seagrasses_zostera.sp.)),2,as.numeric))

final.dat2$broad.seagrass <- broad.seagrass

final.dat3 <- subset(final.dat2,select=c(1:6,9,12,14,31,32))

final.dat4 <- final.dat3 %>% dplyr::mutate_at(vars(starts_with("broad")),as.numeric)

final.dat5 <- final.dat4 %>% dplyr::mutate(total.points.annotated = rowSums(.[,7:(ncol(.))], na.rm = TRUE ))

nodata_sites <- which(final.dat5$total.points.annotated==0)

final.dat6 <- final.dat5[-as.numeric(nodata_sites),]

write.csv(final.dat6, "data/tidy/NESP-2.1_broad-habitat_skip_edits.csv",
          row.names = F)
