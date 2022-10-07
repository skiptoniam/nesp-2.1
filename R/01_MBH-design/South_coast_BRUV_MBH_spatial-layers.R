###
# Project: NESP 2.1
# Data:    GA 250m Bathymetry
# Task:    Create an Esperance sampling plan biased off roughness
# author:  Claude Spencer
# date:    September 2022
##

# Clear memory
rm(list=ls()) 

library(MBHdesign)
library(sf)
library(sp)
library(tidyverse)
library(terra)
library(dplyr)
library(stars)
library(starsExtra)

set.seed(22)

wgscrs <- "+proj=longlat +datum=WGS84"

##########################################Read in bathy raster##############################
cbaths <- list.files("data/spatial/rasters/raw bathymetry", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame))
depth <- rast(cbathy)
crs(depth) <- wgscrs
e <- ext(120, 125, -35, -33.5)                                                  # Reduce general area
depth <- crop(depth, e)
plot(depth)

# mask out the coastal waters
# cwatr <- vect("data/spatial/shapefiles/amb_coastal_waters_limit_polygon.shp")
# depth <- mask(depth, buffer(cwatr), inverse = T)
# plot(depth)

# Mask it just to the two areas
# Investigator Island
inv <- vect("data/mbh-design/Investigator_sample-polys.shp")
inv.depth <- mask(depth, buffer(inv, width = 1000))
inv.depth <- trim(inv.depth)
plot(inv.depth)

# Daw Island/Eastern Recherche
eas <- vect("data/mbh-design/Easter-recherche_sample-polys.shp")
eas.depth <- mask(depth, buffer(eas, width = 1000))
eas.depth <- trim(eas.depth)
plot(eas.depth)

# Get bathymetry derivatives to have a play with
# Investigator Island
# Roughness
rough.inv <- terrain(inv.depth, v = c("roughness", "slope"))
# Detrended bathymetry
zstar <- st_as_stars(inv.depth)
detre.inv <- detrend(zstar, parallel = 8)
detre.inv <- as(object = detre.inv, Class = "SpatRaster")
names(detre.inv) <- c("detrended", "lineartrend")
plot(detre.inv)

# Stack em up
preds.inv <- rast(list(inv.depth, rough.inv, detre.inv[[1]]))
plot(preds.inv)
summary(preds.inv)

cwatr <- vect("data/spatial/shapefiles/amb_coastal_waters_limit_polygon.shp")
preds.inv <- mask(preds.inv, cwatr, inverse = T)
preds.inv[[1]] <- clamp(preds.inv[[1]], lower = -70, value = F)
preds.inv[[2]] <- mask(preds.inv[[2]], preds.inv[[1]])                          # Why am i like this...
preds.inv[[3]] <- mask(preds.inv[[3]], preds.inv[[1]])
preds.inv[[4]] <- mask(preds.inv[[4]], preds.inv[[1]])
plot(preds.inv)

# Daw
# Roughness
rough.eas <- terrain(eas.depth, v = c("roughness", "slope"))
# Detrended bathymetry
zstar <- st_as_stars(eas.depth)
detre.eas <- detrend(zstar, parallel = 8)
detre.eas <- as(object = detre.eas, Class = "SpatRaster")
names(detre.eas) <- c("detrended", "lineartrend")
plot(detre.eas)

# Stack em up
preds.eas <- rast(list(eas.depth, rough.eas, detre.eas[[1]]))
plot(preds.eas)
summary(preds.eas)

preds.eas <- mask(preds.eas, cwatr, inverse = T)
preds.eas[[1]] <- clamp(preds.eas[[1]], lower = -70, value = F)
preds.eas[[2]] <- mask(preds.eas[[2]], preds.eas[[1]])                          # Why am i like this...
preds.eas[[3]] <- mask(preds.eas[[3]], preds.eas[[1]])
preds.eas[[4]] <- mask(preds.eas[[4]], preds.eas[[1]])
plot(preds.eas)

# Write out the tifs
writeRaster(preds.inv, paste0("data/spatial/rasters/investigator_",names(preds.inv), ".tif"), overwrite = T)
writeRaster(preds.eas, paste0("data/spatial/rasters/eastern_",names(preds.eas), ".tif"), overwrite = T)

