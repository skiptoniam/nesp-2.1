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

# Make cuts for Investigator
n <- 70
detrended_qs <- c(0, 0.1, 0.9, 1)
detrended_cuts   <- quantile(preds.inv$detrended, probs = detrended_qs)
cat_detrended  <- cut(preds$detrended, breaks = detrended_cuts, na.rm = TRUE)
plot(stack(preds$detrended, cat_detrended))
detrended_split <- data.frame(zones = unique(cat_detrended),
                          split = c(0.2, 0.4, 0.4))
detrended_split$zbruv <- detrended_split$split * n
rough_split

##########################################Convert depth into a data.frame and generate strata##############################
##convert to df
depth               <- as.data.frame(depth, xy = T)
colnames(depth)     <- c("Easting", "Northing", "Depth")
depth[is.na(depth)] <- -9999

design <- depth %>%
  dplyr::mutate(incl.probs = ifelse(Depth == -9999, 0, 1))
  
inclProb_raster <- rasterFromXYZ(xyz = design[,c("Easting","Northing","incl.probs")])
plot(inclProb_raster)
#inclProb25_raster <- aggregate( inclProb_raster, fact=5, fun= max)# only need to run in massive datasets
# plot(inclProb_raster)
#Always a good idea to write out this raster incase things crash and you can re-read this in negating rerunning the above bit of code
writeRaster(inclProb_raster,"data/spatial/rasters/raw bathymetry/south-coast_BRUV_inclprobs.tif", overwrite = T)
# #########################################Set up the sampling design##############################
n <- 25000*0.25

samp <- quasiSamp.raster(n = n, 
                         inclusion.probs = inclProb_raster, 
                         randStartType = 2, 
                         nSampsToConsider = n*100) # may need to up this value if not running at a national scale
plot(inclProb_raster)
points( samp[,c("x","y")], pch=20, cex=0.01, col = "red")

## assign sampling order
samp$DropC <- 1:nrow(samp)

## Write out sampling file 
write.csv(samp,"data/mbh-design/south-coast_BRUV_MBH.csv") # write out the each region

## now we have sampling design. worth checking in arcmap or similar that you have enough sampling density to be useful for each AMP that we intend to visit

# samp <- quasiSamp.raster(n = n, 
#                          inclusion.probs = !is.na(inclProb_raster), 
#                          randStartType = 2, 
#                          nSampsToConsider = n*500)
# points( samp[,c("x","y")], pch=20, cex=0.01, col = "red")
