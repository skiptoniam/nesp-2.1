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
# library(rgeos)
library(rnaturalearth)
library(ggplot2)
library(metR)
library(stringr)
library(patchwork)
library(raster)
library(terra)
library(ggnewscale)
library(GlobalArchive)
library(tidyverse)
library(viridis)
library(geosphere)

# Set your study name
name <- "Jurien-Two-Rocks"                                                      # Change here

# Set CRS for transformations
wgscrs <- "+proj=longlat +datum=WGS84"
gdacrs <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

# Load necessary spatial files
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))
st_crs(aus) <- gdacrs

# Crop here to area
# Commonwealth parks
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
mpa <- aumpa[aumpa$ResName %in% c("Two Rocks", "Jurien"),]                                # All commonwealth parks
mpa$ZoneName <- factor(mpa$ZoneName, levels = c("Multiple Use Zone", "Special Purpose Zone", 
                                                "National Park Zone"))
npz <- mpa[mpa$ZoneName%in%"National Park Zone",]                       # Just National Park Zones

trok <- mpa[mpa$ResName %in% "Two Rocks",]
ext(trok)
juri <- mpa[mpa$ResName %in% "Jurien", ]
ext(juri)

# State parks

# Terrestrial parks

# Key Ecological Features

# National Reef Predictions
npz_spat <- vect(mpa)
npz_spat <- buffer(npz_spat, width = 1000)
plot(npz_spat)

jacmap <- rast("data/spatial/rasters/ecosystem-types-19class-naland.tif")      # Jac's aus habitat predictions
jacmap <- terra::crop(jacmap, npz_spat, mask = T)

# Coastal waters limit
cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit

# Bathymetry data
bathy      <- readRDS(paste(paste0('data/spatial/rasters/',                     # From 02_spatial_layers.R
                              name), 'ga_bathy.rds', sep = "_"))
bath_r      <- terra::rast(bathy)
crs(bath_r) <- wgscrs

# Mask to both parks
bath_c <- terra::crop(bath_r, npz_spat, mask = T)

# Generate hillshading
slope  <- terrain(bath_c, v='slope', unit='degrees')
aspect <- terrain(bath_c, v='aspect', unit='degrees')
hill   <- shade(slope, aspect, angle = 70, direction = 0)
hill  <- as.data.frame(hill, xy = T, na.rm = T)                                 # To a dataframe for plotting

bath_df <- as.data.frame(bath_c, xy = T)
bath_tr <- bath_df[bath_df$y < -31.0,]
bath_jb <- bath_df[bath_df$y > -31.0,]

# Jurien Bay
# build basic plot elements
p1 <- ggplot() +
  geom_tile(data = hill,aes(x = x, y = y, fill = lyr1), alpha = 1) +
  scale_fill_gradient(low = "white", high = "black", guide = "none") +
  new_scale_fill() +
  geom_tile(data = bath_jb, aes(x = x, y = y, fill = Z), alpha = 0.7) +
  scale_fill_viridis() +
  geom_contour(data = bath_jb, aes(x = x, y = y, z = Z), breaks = c( -30, -70), 
               colour = "white", size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = mpa, aes(color = ZoneName), fill = NA, size = 0.4) +
  scale_color_manual(values = c("National Park Zone" = "#7bbc63",
                                "Special Purpose Zone" = "#6daff4")) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.4) +
  annotate(geom = "text", x = c(114.93, 114.83), y = c(-30.65, -30.65), label = c("30m", "70m"),
           color = "white", size = 1.4) +
  coord_sf(xlim = c(min(bath_jb$x), max(bath_jb$x)), ylim = c(min(bath_jb$y), max(bath_jb$y))) +
  labs(y = "Latitude", x = "Longitude", title = "Jurien", fill = "Depth")+
  theme_minimal()
png(filename = "plots/jurien-exploratory-site-plot.png", height = 6, width = 8,
    res = 300, units = "in")
p1
dev.off()

# Two Rocks
p2 <- ggplot() +
  geom_tile(data = hill,aes(x = x, y = y, fill = lyr1), alpha = 1) +
  scale_fill_gradient(low = "white", high = "black", guide = "none") +
  new_scale_fill() +
  geom_tile(data = bath_tr, aes(x = x, y = y, fill = Z), alpha = 0.7) +
  scale_fill_viridis() +
  geom_contour(data = bath_tr, aes(x = x, y = y, z = Z), breaks = c( -30, -70), 
               colour = "white", size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = mpa, aes(color = ZoneName), fill = NA, size = 0.4) +
  scale_color_manual(values = c("National Park Zone" = "#7bbc63",
                                "Multiple Use Zone" = "#b9e6fb")) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.4) +
  annotate(geom = "text", x = c(115.515, 115.305), y = c(-31.60, -31.60), label = c("30m", "70m"),
           color = "white", size = 1.4) +
  coord_sf(xlim = c(min(bath_tr$x), max(bath_tr$x)), ylim = c(min(bath_tr$y), max(bath_tr$y))) +
  labs(y = "Latitude", x = "Longitude", title = "Two Rocks", fill = "Depth")+
  theme_minimal()
png(filename = "plots/tworocks-exploratory-site-plot.png", height = 4, width = 8,
    res = 300, units = "in")
p2
dev.off()

# jac's map, eh
# sort out the classes
jclass <- read.csv("data/spatial/rasters/Ecosystem_categories-final-2021.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(classname = category.number) %>%
  dplyr::select(classname, exp.ecosystem.names) %>%
  glimpse()

jmap_df <- as.data.frame(jacmap, xy = TRUE, na.rm = TRUE) %>%
  dplyr::rename(classname = "ecosystem-types-19class-naland") %>%
  dplyr::left_join(jclass) %>%
  dplyr::mutate(exp.ecosystem.names = gsub("\\.", " ", exp.ecosystem.names)) %>%
  glimpse()

unique(jmap_df$exp.ecosystem.names)

# Set colours for Investigator Island
# jcls_cols <- scale_fill_manual(values = c(
#   "Shelf unvegetated soft sediments" = "cornsilk1",
#   "Shelf vegetated sediments" = "seagreen3",
#   "Mesophotic coral reefs" = "orange",
#   "Shallow coral reefs less than 30 m depth" = "coral2",
#   "Rariophotic shelf reefs" = "steelblue3",
#   "Artificial reefs pipelines and cables" = "saddlebrown",
#   "Mid slope reef" = "azure4"))

# waterr_cols <- scale_fill_manual(values = c("National Park" = "#c4cea6",
#                                             "Nature Reserve" = "#e4d0bb"),
#                                  guide = "none")

# assign mpa colours - full levels are saved at end of script for future ref
nmpa_cols <- scale_color_manual(values = c("National Park Zone" = "#7bbc63",
                                           "Special Purpose Zone" = "#6daff4",
                                           "Multiple Use Zone" = "#b9e6fb"))

# ab_nmp$ZoneName <- factor(ab_nmp$ZoneName, levels = c("Multiple Use Zone", "Special Purpose Zone",
#                                                       "Habitat Protection Zone","National Park Zone"))

# Build plot Jurien Bay
p3 <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_tile(data = jmap_df, aes(x, y, fill = exp.ecosystem.names)) +
  scale_fill_manual(values = c(
    "Shelf unvegetated soft sediments" = "cornsilk1",
    # "Shelf vegetated sediments" = "seagreen3",
    "Mesophotic coral reefs" = "orange",
    "Shallow coral reefs less than 30 m depth" = "coral2",
    "Rariophotic shelf reefs" = "steelblue3",
    # "Artificial reefs pipelines and cables" = "saddlebrown",
    "Mid slope reef" = "azure4")) +
  geom_contour(data = bath_jb, aes(x = x, y = y, z = Z),
               breaks = c(-30, -70, -200, - 700, - 7000), colour = "black", alpha = 1, size = 0.18) +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName)) +
  nmpa_cols+
  labs(color = "Australian Marine Parks") +
  new_scale_color() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  annotate(geom = "text", x = c(114.93, 114.83), y = c(-30.65, -30.65), label = c("30m", "70m"),
           color = "black", size = 1.4) +
  coord_sf(xlim = c(min(bath_jb$x), max(bath_jb$x)), ylim = c(min(bath_jb$y), max(bath_jb$y))) +
  labs(fill = "Habitat classification", x = NULL, y = NULL) +
  theme_minimal()

png(filename = "plots/jurien_jmonk_natmap.png", width = 8, height = 6,
    units = "in", res = 200)
p3
dev.off()

# Build plot Two Rocks
p4 <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_tile(data = jmap_df, aes(x, y, fill = exp.ecosystem.names)) +
  scale_fill_manual(values = c(
    "Shelf unvegetated soft sediments" = "cornsilk1",
    # "Shelf vegetated sediments" = "seagreen3",
    "Mesophotic coral reefs" = "orange",
    "Shallow coral reefs less than 30 m depth" = "coral2",
    # "Rariophotic shelf reefs" = "steelblue3",
    "Artificial reefs pipelines and cables" = "saddlebrown"#,
    # "Mid slope reef" = "azure4"
    )) +
  geom_contour(data = bath_tr, aes(x = x, y = y, z = Z),
               breaks = c(-30, -70, -200, - 700, - 7000), colour = "black", alpha = 1, size = 0.18) +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName)) +
  nmpa_cols+
  labs(color = "Australian Marine Parks") +
  new_scale_color() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  annotate(geom = "text", x = c(115.515, 115.305), y = c(-31.60, -31.60), label = c("30m", "70m"),
           color = "black", size = 1.4) +
  coord_sf(xlim = c(min(bath_tr$x), max(bath_tr$x)), ylim = c(min(bath_tr$y), max(bath_tr$y))) +
  labs(fill = "Habitat classification", x = NULL, y = NULL) +
  theme_minimal()

png(filename = "plots/tworocks_jmonk_natmap.png", width = 8, height = 4,
    units = "in", res = 200)
p4
dev.off()

# Bathymetry cross section
# Read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
cbaths      <- list.files("data/spatial/rasters/raw bathymetry",                # Bathymetry data too large for git stored here
                          "*tile", full.names = TRUE) 
cbathy      <- lapply(cbaths,                                                   # Loads all of the tiles
                      function(x){read.table(file = x, header = TRUE, sep = ",")})    
cbathy      <- do.call("rbind", lapply(cbathy, as.data.frame))                  # Turns the list into a data frame
bath_r      <- terra::rast(cbathy)                                              # Convert to a raster
crs(bath_r) <- wgscrs                                                           # Set the CRS
# bath_r <- crop(bath_r, ext(114.600000000177, 115.5, -30.7166666670415, -30.1741099998549))
plot(bath_r)

bath_all <- as.data.frame(bath_r, xy = T)

# Jurien Bay
bathx_jb <- bath_all %>%
  dplyr::filter(y %in% -30.69125, x > 114.7 & x < 115.2) %>%
  dplyr::mutate(distance.from.coast = (distHaversine(cbind(.$x, .$y),
                                                    c(115.1137, -30.69125)))/1000) %>% # Coastline distance
  dplyr::mutate(distance.from.coast = ifelse(Z <= 0, distance.from.coast*-1, distance.from.coast))

p5 <- ggplot() +
  geom_rect(aes(xmin = min(bathx_jb$distance.from.coast), xmax = 5, ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  geom_line(data = bathx_jb, aes(y = Z, x = distance.from.coast))+
  geom_ribbon(data = bathx_jb, aes(ymin = -Inf, ymax = Z, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(breaks = c(150, 0, -150, -300, -450, -600),expand = c(0,0), limits = c(-700, 240)) +
  labs(x = "Distance from coast (km)", y = "Elevation (m)") + 
  # annotate("text", x = 3, y = 200, label = "Boranup") +
  geom_segment(aes(x = -5.556, xend = -5.556, y =-33, yend = 0), color = "red") +
  geom_segment(aes(x = c(-33.26033124, -38.52530863), xend = c(-33.26033124, -38.52530863), 
                   y = c(-153, -198), yend =  c(0, 0)), color = "green")

png(filename = "plots/jurien-cross-section.png", width = 8, height = 4,
    units = "in", res = 200)
p5
dev.off()

# Two rocks
sf_use_s2(T)
points <- data.frame(x = c(114.9741, 115.7935), 
                     y = c(-31.6035, -31.6035), id = 1)

tran <- sfheaders::sf_linestring(obj = points,
                                 x = "x", 
                                 y = "y",
                                 linestring_id = "id")
st_crs(tran) <- wgscrs

tranv <- vect(tran)
dep <- rast(bath_all)

bathy <- terra::extract(dep, tranv, xy = T, ID = F)

bath_cross <- st_as_sf(x = bathy, coords = c("x", "y"), crs = wgscrs)

aus <- st_read("data/spatial/shapefiles/cstauscd_r.mif")
st_crs(aus) <- st_crs(aumpa)
aus <- st_transform(aus, wgscrs)
aus <- aus[aus$FEAT_CODE %in% "mainland", ]
aus <- st_union(aus)
plot(aus)
ausout <- st_cast(aus, "MULTILINESTRING")
plot(ausout)

bath_sf <- bath_cross %>%
  dplyr::mutate(land = lengths(st_intersects(bath_cross, aus)) > 0) %>%
  bind_cols(st_coordinates(.)) %>%
  glimpse()

bath_df1 <- as.data.frame(bath_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::rename(depth = "Z") %>%
  dplyr::mutate(distance.from.coast = as.numeric((distHaversine(cbind(.$X, .$Y),
                                                                c(115.6488,-31.60375)))/1000)) %>%
  dplyr::mutate(distance.from.coast = ifelse(land %in% "FALSE", distance.from.coast*-1, distance.from.coast)) %>%
  dplyr::filter(depth > -250) %>%
  glimpse()

p6 <- ggplot() +
  geom_rect(aes(xmin = min(bath_df1$distance.from.coast), xmax = 5, ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  geom_segment(aes(x = -5.556, xend = -5.556, y =-21, yend = 0), color = "red") +
  geom_line(data = bath_df1, aes(y = depth, x = distance.from.coast))+
  geom_ribbon(data = bath_df1, aes(ymin = -Inf, ymax = depth, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(breaks = c(150, 0, -150, -300, -450, -600),expand = c(0,0), limits = c(-700, 240)) +
  labs(x = "Distance from coast (km)", y = "Elevation (m)") 
  

png(filename = "plots/tworocks-cross-section.png", width = 8, height = 4,
    units = "in", res = 200)
p6
dev.off()


