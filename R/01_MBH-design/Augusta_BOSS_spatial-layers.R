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

wgscrs <- "+proj=longlat +datum=WGS84"

##########################################Read in bathy raster##############################
e <- ext(114.3, 115.4, -34.8, -34.2)    
depth <- rast("data/spatial/rasters/raw bathymetry/bath_250_good.tif") %>%
  crop(e) %>%
  clamp(lower = -200 ,upper = 0, values = F) %>%
  trim()
plot(depth)

# Get bathymetry derivatives to have a play with
# Investigator Island
# Roughness
rough <- terrain(depth, v = c("roughness"))

# Stack em up
preds <- rast(list(depth, rough))
plot(preds[[2]])
summary(preds)

cwatr <- vect("data/spatial/shapefiles/amb_coastal_waters_limit_polygon.shp")
aumpa <- vect("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")
plot(aumpa)
preds <- mask(preds, cwatr, inverse = T)
plot(preds[[1]])
plot(aumpa, add = T)
preds <- mask(preds, aumpa)
plot(preds[[1]])
preds[[1]] <- clamp(preds[[1]], lower = -120, value = F)
preds[[2]] <- mask(preds[[2]], preds[[1]])                          
plot(preds)

names(preds)[1] <- "depth"

# Write out the tifs
writeRaster(preds, paste0("data/spatial/rasters/augusta_",
                              names(preds), ".tif"), overwrite = T)
