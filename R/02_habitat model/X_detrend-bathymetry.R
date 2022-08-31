###
# Project: NESP 2.1 
# Data:    Bathymetry Data
# Task:    Detrend bathymetry
# Author:  Claude Spencer
# Date:    August 2022
## 

# This script formats bathymetry data and extracts bathymetry derivatives for modelling habitat and fish
# As this raw bathymetry data is often too large for GitHub, the raw files are hidden in the .gitnore
# You have to download data and create this folder directory yourself for the script to run!

# Clear your environment
rm(list = ls())

# Load libraries - some more to add here
library(sp)
library(terra)
library(sf)
library(stars)
library(starsExtra)

# Set your study name
name <- ""                                                                      # Change here

# Set CRS for bathymetry data
wgscrs <- "+proj=longlat +datum=WGS84 +south"                                   # Latlong projection 

# This next section uses coarse GA bathymetry, replace if you have better bathymetry data (ie. multibeam or LiDAR)
# Read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
cbaths      <- list.files("data/spatial/rasters/raw bathymetry",                # Bathymetry data too large for git stored here
                          "*tile", full.names = TRUE) 
cbathy      <- lapply(cbaths,                                                   # Loads all of the tiles
                      function(x){read.table(file = x, header = TRUE, sep = ",")})    
cbathy      <- do.call("rbind", lapply(cbathy, as.data.frame))                  # Turns the list into a data frame
cbathy      <- cbathy[cbathy$Z <= 0, ]                                          # Get rid of topography data above 0m
bath_r      <- terra::rast(cbathy)                                              # Convert to a raster
crs(bath_r) <- wgscrs                                                           # Set the CRS
plot(bath_r)                                                                    # Plot to check everything looks ok

# Crop the bathymetry to the general study area
# tbath_c <- crop(bath_r, terra::ext(c(114, 115.75,-32, -30)))                  # No need for cropping when you want all of aus...
# plot(tbath_c)

# Calculate detrended bathymetry
zstar <- st_as_stars(tbath_c)                                                   # Convert to a stars object
detre <- detrend(zstar, parallel = 8)                                           # Detrend bathymetry - This usually runs quite slow!
detre <- as(object = detre, Class = "SpatRaster")                               # Convert it to a raster
names(detre) <- c("detrended", "lineartrend")
preds <- rast(detre)                                                            # Make a rasterstack 
plot(preds)                                                                     # Check it 

# Save the output
preds <- wrap(preds)
saveRDS(preds, paste(paste0('data/spatial/rasters/', name), 'spatial_covariates.rds', sep = "_"))
