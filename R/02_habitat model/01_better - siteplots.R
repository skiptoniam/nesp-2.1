###
# Project: ** Add here **
# Data:    Geoscience Australia 250m res bathy
# Task:    Generate exploratory site plots
# author:  Claude Spencer
# date:    ** Add here **
##

# CONTENTS
# 1. Exploratory bathymetry plots (p1)
# 2. National Reef Model plot (p2)
# 3. Location overview plot - includes parks zones and an aus inset (p3)
# 4. Site zoom plot - including sampling points (p4)
# 5. Key Ecological Features (p5)
# 6. Old sea level map (p6)

# Clear your environment
rm(list = ls())

# Load libraries
library(dplyr)
library(sf)
library(rgeos)
library(rnaturalearth)
library(ggplot2)
library(metR)
library(stringr)
library(patchwork)
library(terra)
library(ggnewscale)
library(GlobalArchive)
library(tidyverse)
library(viridis)


# Set CRS for transformations
wgscrs <- CRS("+proj=longlat +datum=WGS84")
gdacrs <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

# Load necessary spatial files
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  st_crs(gdacrs) %>%
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))
# Crop here to area

# Commonwealth parks
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
damp_mp <- aumpa[aumpa$ResName%in%"Dampier",]                                   # All commonwealth parks
damp_sanc <- damp_mp[damp_mp$ZoneName%in%"National Park Zone",]                 # Just National Park Zones

# State parks

# Terrestrial parks

# Key Ecological Features

# Coastal waters limit
cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit

# Bathymetry data
bathy      <- readRDS(paste(paste0('data/spatial/rasters/',                     # From 02_spatial_layers.R
                              name), 'ga_bathy.rds', sep = "_"))
bath_r      <- rasterFromXYZ(bathy)
crs(bath_r) <- wgscrs

# Generate hillshading
slope  <- terrain(bath_r, opt='slope', unit='degrees')
aspect <- terrain(bath_r, opt='aspect', unit='degrees')
hill   <- hillShade(slope, aspect, angle = 70, azimuth = 0)
hill  <- as.data.frame(hill, xy = T, na.rm = T)                                 # To a dataframe for plotting

# build basic plot elements
p1 <- ggplot() +
  geom_tile(data = hill,aes(x = x, y = y, fill = layer), alpha = 1) +
  scale_fill_gradient(low = "white", high = "black", guide = "none") +
  new_scale_fill() +
  geom_tile(data = bathy, aes(x = x, y = y, fill = Depth), alpha = 0.7) +
  scale_fill_viridis() +
  geom_contour(data = bathy, aes(x = x, y = y, z = Depth), breaks = c( -30, -70), 
               colour = "white", size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = damp_mp, aes(color = ZoneName), fill = NA, size = 0.4) +
  scale_color_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                "National Park Zone" = "#7bbc63",
                                "Multiple Use Zone" = "#b9e6fb")) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.4) +
  annotate(geom = "text", x = 117.1, y = -20.385, label = "30m",
           color = "white", size = 2) +
  coord_sf(xlim = c(116.8333, 117.5167), ylim = c(-20.56667, -20.3)) +
  labs(y = "Latitude", x = "Longitude")+
  theme_minimal()
png(filename = "plots/exploratory-site-plot.png", height = 4, width = 10,
    res = 300, units = "in")
p1
dev.off()

