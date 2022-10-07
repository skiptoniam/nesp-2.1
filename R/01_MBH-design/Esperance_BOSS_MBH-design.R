###
# Project: NESP 2.1
# Data:    National sampling plan - Daw, Salisbury & Eastern-Recherche
# Task:    Convert shapefile sampling points to csv and add an abbreviated sample name
# author:  Claude Spencer
# date:    September 2022
##

rm(list = ls())

library(dplyr)
library(sf)
library(tidyverse)

# Load the data from QGIS
inv <- st_read("data/mbh-design/Investigator_MBH_wgs84.shp")                    # Investigator
daw <- st_read("data/mbh-design/Eastern-recherche_MBH_wgs84.shp")               # Daw
sal <- st_read("data/mbh-design/Salisbury_MBH_wgs84.shp")                       # Salisbury

# Convert sf object to dataframe and abbreviate drop code to sample
sf_to_df <- function(data, park) {                                              # SF object, park abbreviation for sample names
  temp <- data %>%
    dplyr::select(DropC, geometry) %>%
    dplyr::mutate(x = unlist(map(data$geometry, 1)),
                  y = unlist(map(data$geometry, 2))) %>%
    as.data.frame() %>%
    dplyr::select(-geometry) %>%
    dplyr::arrange(DropC) %>%
    dplyr::slice_head(n = 100) %>%                                              # Take the top 100 rows
    dplyr::mutate(sample = paste(park,"DC", 
                                 str_pad(row_number(), 3,                       # Pad with more 0s when samples > 99
                                         side = "left", pad = "0") , sep = "-"))
  assign(paste(park, "MBH", sep = "_"), data.frame(temp), envir = .GlobalEnv)
  
}

# Run the function and write data to csv
sf_to_df(inv, "INV")                                                            # Two rocks 
write.csv(INV_MBH, file = "data/mbh-design/Investigator_MBH_wgs84_thinned.csv", 
          row.names = F)

sf_to_df(daw, "DAW")                                                            # Jurien
write.csv(DAW_MBH, file = "data/mbh-design/Daw_MBH_wgs84_thinned.csv", 
          row.names = F)

sf_to_df(sal, "SAL")                                                            # Jurien
write.csv(SAL_MBH, file = "data/mbh-design/Salisbury_MBH_wgs84_thinned.csv", 
          row.names = F)



