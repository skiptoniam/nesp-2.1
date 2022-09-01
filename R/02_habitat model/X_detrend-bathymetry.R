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
library(googledrive)

# Set your study name
name <- "nesp-2.1"                                                                      # Change here

# Set CRS for bathymetry data
wgscrs <- "+proj=longlat +datum=WGS84 +south"                                   # Latlong projection 

# This next section uses coarse GA bathymetry, replace if you have better bathymetry data (ie. multibeam or LiDAR)
# Read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
drive_download("ausbath_09_v4_prj_masked.tif", 
               path = "data/spatial/rasters/raw bathymetry/ausbath_09_v4_prj_masked.tif",
               overwrite = T)
bath <- rast("data/spatial/rasters/raw bathymetry/ausbath_09_v4_prj_masked.tif")
plot(bath)                                                                      # Plot to check everything looks ok
bath_df <- as.data.frame(bath, na.rm = T, xy = T)                               # Trying to remove NAs to see if it fixes anything                   
bath_r <- rast(bath_df)
crs(bath_r) <- crs(bath)
plot(bath_r)

# Calculate detrended bathymetry
zstar <- st_as_stars(bath_r)                                                    # Convert to a stars object
detre <- detrend(zstar, parallel = 8)                                           # Detrend bathymetry - wish it had a progress bar
detre <- as(object = detre, Class = "SpatRaster")                               # Convert it to a raster
names(detre) <- c("detrended", "lineartrend")
plot(detre[[1]])
summary(detre[[1]])
# Save the output
terra::writeRaster(detre[[1]], "data/spatial/rasters/raw bathymetry/nesp-2.1_detrended-bathy_250m.tif",
                   overwrite = T)
