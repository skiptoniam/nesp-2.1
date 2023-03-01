###
# Project: NESP 2.1
# Data:    National sampling plan
# Task:    Convert shapefile sampling points to csv and add an abbreviated sample name
# author:  Claude Spencer
# date:    September 2022
##

rm(list = ls())

library(dplyr)
library(sf)
library(tidyverse)

# Load the data from QGIS
tr <- st_read("data/mbh-design/Two-rocks_MBH_wgs84.shp")                        # Two rocks
jb <- st_read("data/mbh-design/Jurien_MBH_wgs84.shp")                           # Jurien

# Convert sf object to dataframe and abbreviate drop code to sample
sf_to_df <- function(data, park) {                                              # SF object, park abbreviation for sample names
  temp <- data %>%
    dplyr::select(DropC, geometry) %>%
    dplyr::mutate(x = unlist(map(data$geometry, 1)),
                  y = unlist(map(data$geometry, 2))) %>%
    as.data.frame() %>%
    dplyr::select(-geometry) %>%
    dplyr::arrange(DropC) %>%
    dplyr::mutate(sample = paste("TR", 
                                 str_pad(row_number(), 2,                       # Pad with more 0s when samples > 99
                                         side = "left", pad = "0") , sep = "-"))
  assign(paste(park, "MBH", sep = "_"), data.frame(temp), envir = .GlobalEnv)
  
}

# Run the function and write data to csv
sf_to_df(tr, "TR")                                                              # Two rocks                      
write.csv(TR_MBH, file = "data/mbh-design/Two-rocks_MBH_wgs84.csv", 
          row.names = F)

sf_to_df(jb, "JB")                                                              # Jurien
write.csv(JB_MBH, file = "data/mbh-design/Jurien_MBH_wgs84.csv", 
          row.names = F)



