###
# Project: NESP 2.1
# Data:    GA 250m Bathymetry
# Task:    Create an Augusta sampling plan biased off roughness
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

depth <- rast("data/spatial/rasters/raw bathymetry/multibeam_derivatives_depth.tif")
detrended <- rast("data/spatial/rasters/raw bathymetry/multibeam_derivatives_detrended.tif")
roughness <- rast("data/spatial/rasters/raw bathymetry/multibeam_derivatives_roughness.tif")

preds <- rast(list(depth, detrended, roughness))
names(preds) <- c("depth", "detrended", "roughness")

wgscrs <- "+proj=longlat +datum=WGS84"
sppcrs <- "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs"

cwatr <- vect("data/spatial/shapefiles/amb_coastal_waters_limit_polygon.shp") %>%
  terra::project(sppcrs)
aumpa_sf <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp") %>%
  dplyr::filter(ResName %in% "South-west Corner" & ZoneName %in% "National Park Zone")
aumpa <- vect(aumpa_sf)  %>%
  terra::project(sppcrs)
plot(aumpa)
preds <- mask(preds, cwatr, inverse = T)
plot(preds[[1]])
plot(aumpa, add = T)
preds <- mask(preds, aumpa) %>%
  trim()
preds[[1]] <- clamp(preds[[1]], lower = -80, value = F)
preds[[2]] <- mask(preds[[2]], preds[[1]])  
preds[[3]] <- mask(preds[[3]], preds[[1]]) 
preds <- trim(preds)
plot(preds)

# Write out the tifs
writeRaster(preds, paste0("data/spatial/rasters/swc_",
                              names(preds), ".tif"), overwrite = T)
