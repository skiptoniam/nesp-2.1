###
# Project: NESP 2.1
# Data:    GA 250m Bathymetry
# Task:    Generate DEM derived metrics fo Naturaliste Reefs
# author:  Claude Spencer
# date:    February 2023
##

# Clear memory
rm(list=ls()) 

library(sf)
library(tidyverse)
library(terra)
library(dplyr)
library(stars)
library(starsExtra)

wgscrs <- "+proj=longlat +datum=WGS84"

##########################################Read in bathy raster##############################
windarea <- st_read("data/spatial/shapefiles/WA proposals_georeferenced.shp") %>%
  dplyr::filter(id == 1) %>%
  vect() %>%
  project(wgscrs)

depth <- rast("data/spatial/rasters/raw bathymetry/bath_250_good.tif") %>%
  mask(windarea) %>%
  clamp(upper = 0, values = F) %>%
  trim()
plot(depth)

# Get bathymetry derivatives to have a play with
# Roughness
rough <- terrain(depth, v = c("roughness"))

preds <- rast(list(depth, rough))
names(preds)[1] <- "depth"

# Write out the tifs
writeRaster(preds, paste0("data/spatial/rasters/naturaliste_",
                          names(preds), ".tif"), overwrite = T)
