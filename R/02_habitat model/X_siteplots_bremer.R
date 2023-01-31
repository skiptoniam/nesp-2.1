###
# Project: NESP 2.1
# Data:    250m bathy
# Task:    Exploratory maps for Bremer
# author:  Claude Spencer
# date:    January 2023
##

rm(list = ls())

# Load libraries
library(dplyr)
library(sf)
# library(rgeos)
library(rnaturalearth)
library(ggplot2)
# library(metR)
library(stringr)
library(patchwork)
library(terra)
library(ggnewscale)
library(GlobalArchive)
library(tidyverse)
library(viridis)
library(geosphere)
library(stars)

# Set CRS for transformations
wgscrs <- "+proj=longlat +datum=WGS84" 

# Get data and sort spatial boundaries
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif")   %>%
  dplyr::filter(!FEAT_CODE %in% c("sea"))                                                         
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
brem_mp <- aumpa[aumpa$ResName%in%c("Bremer"),]
brem_test <- aumpa %>%
  dplyr::filter(ResName %in% c('South-west Corner', "Bremer", "Eastern Recherche")) %>%
  dplyr::mutate(value = ifelse(ZoneName %in% "National Park Zone", 1, 0))
# npz <- brem_mp %>%
#   dplyr::filter(ZoneName %in% "National Park Zone")
npz.rast <- st_rasterize(brem_test %>% dplyr::select(value, geometry))
plot(npz.rast)
npz.rast <- rast(npz.rast) %>%
  disagg(fact = 10, method = "")
plot(npz.rast)

# write_stars(npz.rast, "data/spatial/rasters/bremer-marine-park-raster.tif")

e <- ext(119.3, 120.5, -35.4, -33.9)
brem_mp <- st_crop(brem_mp, e)
st_crs(aus) <- st_crs(aumpa)
unique(brem_mp$ZoneName)
brem_mp$ZoneName <- factor(brem_mp$ZoneName, levels = c("Special Purpose Zone (Mining Exclusion)", 
                                                      "National Park Zone"))
terrnp <- st_read("data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
terrnp <- st_crop(terrnp, xmin = 118, ymin = -36, xmax = 121, ymax = -32)       # Just the study area

waterr_cols <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                            "Nature Reserve" = "#e4d0bb"), name = "State Managed Areas")

cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit

# Turn back on this section to re-run exporting the bathy

# cbaths <- list.files("data/spatial/rasters/raw bathymetry", "*tile", full.names = TRUE)
# cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
# cbathy <- do.call("rbind", lapply(cbathy, as.data.frame))
# bathy <- rast(cbathy)
# crs(bathy) <- wgscrs
# plot(bathy)
# 
# # Crop to general project area of Investigator Island (inv)
# brem_spat <- vect(brem_mp)
# brem_bathy <- crop(bathy, buffer(brem_spat, width = 50000))
# # brem_bathy[brem_bathy > 0] <- NA                                              # Remove topography data
# # inv_bathy[inv_bathy < -150] <- NA                                             # Remove data past 150m
# plot(brem_bathy)
# plot(brem_spat, add = T)
# brem_bathdf <- as.data.frame(brem_bathy, na.rm = T, xy = T)                     # As a dataframe for plotting
# colnames(brem_bathdf)[3] <- "Depth"
# saveRDS(brem_bathdf, file = "data/spatial/rasters/Bremer_GA_250m_bathy.RDS")    # Save it out

brem_bathdf <- readRDS("data/spatial/rasters/Bremer_GA_250m_bathy.RDS") %>%
  dplyr::filter(Depth > -250 & Depth < 0)
brem_bathy <- rast(brem_bathdf)
crs(brem_bathy) <- wgscrs
plot(brem_bathy)

# Hillshading for exploratory plot
slope <- terrain(brem_bathy, v = 'slope', unit = 'degrees')
aspect <- terrain(brem_bathy, v = 'aspect', unit = 'degrees')
hill <- shade(slope, aspect, angle = 70, direction = 0)                         # Change here
brem_hill <- as.data.frame(hill, xy = T, na.rm = T)                             # To a dataframe for plotting

brem_bathdfall <- readRDS("data/spatial/rasters/Bremer_GA_250m_bathy.RDS") %>%
  dplyr::filter(Depth < 0)

# Bremer Marine Park
# build basic plot elements
p1 <- ggplot() +
  geom_tile(data = brem_hill,aes(x = x, y = y, fill = lyr1), alpha = 1) +
  scale_fill_gradient(low = "white", high = "black", guide = "none") +
  new_scale_fill() +
  geom_tile(data = brem_bathdfall, aes(x = x, y = y, fill = Depth), alpha = 0.7) +
  scale_fill_viridis() +
  new_scale_fill() +
  geom_contour(data = brem_bathdf, aes(x = x, y = y, z = Depth), breaks = c(0, -30, -70, - 200),
               colour = "white", size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  waterr_cols +
  new_scale_fill() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.5) +
  geom_sf(data = brem_mp, aes(color = ZoneName), fill = NA, size = 0.8) +
  scale_color_manual(values = c("Special Purpose Zone (Mining Exclusion)" = "#6daff4",
                                "National Park Zone" = "#7bbc63")) +
  # annotate(geom = "text", x = c(120.8, 120.8, 120.8), y = c(-33.95, -34.08, -34.24), 
  #          label = c("30m", "70m", "70m"), size = 2, color = "white") +
  annotate(geom = "segment", linetype = "dashed", x = 119.473300, xend = 119.75, y = -34.254223, yend = -34.64,
           alpha = 0.7, colour = "gray20") +
  coord_sf(xlim = c(119.35, 120.43), ylim = c(-34.8, -33.8075)) +
  labs(y = "Latitude", x = "Longitude", color = "Australian Marine Parks") +
  theme_minimal()
png(filename = "plots/bremer-exploratory-site-plot.png", height = 6, width = 8,
  res = 300, units = "in")
p1
dev.off()

# 2. Site zoom plot - including sampling points (p2)
# Reload the depth data without filtering out deep stuff
brem_bathdf <- readRDS("data/spatial/rasters/Bremer_GA_250m_bathy.RDS") %>%
  dplyr::filter(Depth < 0)

metadata <- read.csv("data/raw/2022-11_Esperance_stereo-BOSS.csv") %>%
  dplyr::filter(Site %in% "Bremer") %>%
  glimpse

no.samps <- metadata %>%
  dplyr::summarise(n = n()) %>%
  glimpse()

p2 <- ggplot() +
  geom_contour_filled(data = brem_bathdf, aes(x = x, y = y, z = Depth,
                                        fill = after_stat(level)),
                      breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), alpha = 4/5) +
  scale_fill_grey(start = 1, end = 0.5 , guide = "none") +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +  
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  labs(fill = "State Managed Areas") +
  waterr_cols +
  new_scale_fill() +
  geom_sf(data = brem_mp, aes(fill = ZoneName), colour = NA, size = 0.8, alpha = 4/5) +
  scale_fill_manual(values = c("Special Purpose Zone (Mining Exclusion)" = "#6daff4",
                                "National Park Zone" = "#7bbc63")) +
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks") +
  new_scale_fill() +
  geom_contour(data = brem_bathdf, aes(x = x, y = y, z = Depth), 
               breaks = c(0, -30, -70, -200, - 700, - 9000), colour = "white", alpha = 1, size = 0.2) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.4) +
  geom_point(data = metadata, aes(Longitude, Latitude),
             alpha = 3/5, shape = 10) +
  labs(colour = "Sample", x = NULL, y = NULL) +
  guides(fill = guide_legend(order = 2), col = guide_legend(order = 1)) +
  coord_sf(xlim = c(119.35, 120.43), ylim = c(-34.8, -33.8075)) +                            # Change here
  theme_minimal()

png(filename = "plots/bremer-sampling-locations.png", height = 6, width = 8,
    res = 300, units = "in")
p2
dev.off()

# 3. National Reef Model plot (p2)
nrm <- rast("data/spatial/rasters/ecosystem-types-19class-naland.tif")
nrm <- crop(nrm, e)

# Load the classes to match to the raster
nrm_class <- read.csv("data/spatial/rasters/Ecosystem_categories-final-2021.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(classname = category.number) %>%
  dplyr::select(classname, exp.ecosystem.names) %>%
  glimpse()

nrm_df <- as.data.frame(nrm, xy = TRUE, na.rm = TRUE) %>%                       # Join and convert to a dataframe
  dplyr::rename(classname = "ecosystem-types-19class-naland") %>%
  dplyr::left_join(nrm_class) %>%
  dplyr::mutate(exp.ecosystem.names = gsub("\\.", " ", exp.ecosystem.names)) %>%
  glimpse()

unique(nrm_df$exp.ecosystem.names)                                              # Manually set colours for plotting
nrm_fills <- scale_fill_manual(values = c(
  "Shelf unvegetated soft sediments" = "cornsilk1",
  "Mesophotic coral reefs" = "orange",
  "Shallow coral reefs less than 30 m depth" = "coral2",
  "Shelf vegetated sediments" = "seagreen3",
  "Shallow rocky reefs less than 30 m depth" = "darkgoldenrod1",
  # "Mesophotic rocky reefs" = "khaki4",
  "Mesophotic rocky reefs" = "plum4",
  "Rariophotic shelf reefs" = "steelblue3",
  "Upper slope unvegetated soft sediments" = "wheat1",
  "Mid slope sediments" = "#f7d29c",
  "Upper slope rocky reefs shelf break to 700 m depth" = "indianred3",
  # "Artificial reefs pipelines and cables" = "saddlebrown",
  # "Mid slope reef" = "azure4",
  # "Lower slope reef and sediments" = "burlywood3",
  "Lower slope reef and sediments" = "azure4",
  "Abyssal reef and sediments" = "bisque4",
  "Shelf incising and other canyons" = "darkslategrey"),
  name = "Habtiat classification")

# assign mpa colours - full levels are saved at end of script for future ref
nmpa_cols <- scale_color_manual(values = c("National Park Zone" = "#7bbc63",
                                           "Special Purpose Zone (Mining Exclusion)" = "#6daff4"),
                                name = "Australian Marine Parks")

p3 <- ggplot() +
  geom_tile(data = nrm_df, aes(x, y, fill = exp.ecosystem.names)) +
  nrm_fills +
  new_scale_fill() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  waterr_cols +
  new_scale_fill() +
  geom_contour(data = brem_bathdf, aes(x = x, y = y, z = Depth),
               breaks = c(-30, -70, -200, - 700, - 7000), colour = "black", alpha = 1, size = 0.18) +
  geom_sf(data = brem_mp, fill = NA, aes(colour = ZoneName), size = 0.4) +
  nmpa_cols +
  new_scale_colour() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.6) +
  new_scale_color() +
  coord_sf(xlim = c(119.35, 120.43), ylim = c(-34.8, -33.8075)) +                            # e <- ext(112, 115, -23, -21)
  labs(x = NULL, y = NULL) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  theme_minimal()

png(filename = "plots/bremer-national-reef-model.png", width = 8, height = 7,
    units = "in", res = 300)
p3
dev.off()

# Bathymetry cross section
# Make sf point
points <- data.frame(x = c(119.473300, 120), 
                      y = c(-34.254223, -35), id = 1)

tran <- sfheaders::sf_linestring(obj = points,
                         x = "x", 
                         y = "y",
                         linestring_id = "id")
st_crs(tran) <- wgscrs

brem_bathdf <- readRDS("data/spatial/rasters/Bremer_GA_250m_bathy.RDS")
tranv <- vect(tran)
dep <- rast(brem_bathdf)

test <- dep %>%
  clamp(upper = 0, values = FALSE)
plot(test)
plot(tranv, add = T)
cbathy <- terra::extract(dep, tranv, xy = T, ID = F)

bath_cross <- st_as_sf(x = cbathy, coords = c("x", "y"), crs = wgscrs)

aus <- st_read("data/spatial/shapefiles/cstauscd_r.mif")
st_crs(aus) <- st_crs(aumpa)
aus <- st_transform(aus, wgscrs)
aus <- aus[aus$FEAT_CODE %in% "mainland", ]
aus <- st_union(aus)
plot(aus)
ausout <- st_cast(aus, "MULTILINESTRING")
plot(ausout)

bath_sf <- bath_cross %>%
  dplyr::mutate(x = unlist(map(bath_cross$geometry, 1)),
                y = unlist(map(bath_cross$geometry, 2)),
                land = lengths(st_intersects(bath_cross, aus)) > 0) %>%
  dplyr::mutate(distance.from.coast = distHaversine(cbind(x , y), c(119.5012, -34.29625))) %>%
  glimpse()

bath_df1 <- as.data.frame(bath_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::rename(depth = "Depth") %>%
  dplyr::mutate(distance.from.coast = as.numeric(distance.from.coast/1000)) %>%
  dplyr::mutate(distance.from.coast = ifelse(land %in% "FALSE", distance.from.coast*-1, distance.from.coast)) %>%
  dplyr::filter(depth > -250) %>%
  glimpse()

paleo <- data.frame(depth = c(-118, -94, -63, -41),
                    label = c("20-30 Ka", "15-17 Ka", "12-13 Ka", "9-10 Ka"))

for (i in 1:nrow(paleo)) {
  temp <- bath_df1 %>%
    dplyr::filter(abs(bath_df1$depth - paleo$depth[i]) == min(abs(bath_df1$depth - paleo$depth[i]))) %>%
    dplyr::select(depth, distance.from.coast) %>%
    slice(1)
  
  if (i == 1) {
    dat <- temp
  } 
  else {
    dat <- bind_rows(dat, temp)
  }}

paleo$distance.from.coast <- dat$distance.from.coast

p4 <- ggplot() +
  geom_rect(aes(xmin = min(bath_df1$distance.from.coast), 
                xmax = max(bath_df1$distance.from.coast), ymin =-Inf, ymax = 0), 
            fill = "#12a5db", alpha = 0.5) +
  geom_line(data = bath_df1, aes(y = depth, x = distance.from.coast)) +
  geom_ribbon(data = bath_df1, aes(ymin = -Inf, ymax = depth, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "Distance from coast (km)", y = "Elevation (m)") +
  geom_segment(data = paleo, aes(x = distance.from.coast, xend = distance.from.coast + 4,
                                 y = depth, yend = depth), linetype = 2, alpha = 0.5) +
  geom_text(data = paleo, aes(x = distance.from.coast + 6, y = depth, label = label), size = 3) +
  annotate(geom = "text", x = c(-10, 0.9), y = c(10, 40), label = c("Point Hood", "Gordon Inlet"), size = 2.5) +
  annotate(geom = "segment", x = 0.9, xend = 0.9, y = 0.2, yend = 35, linetype = "dashed") 
png(filename = "plots/bremer-cross-section.png", width = 8, height = 4,
    units = "in", res = 200)
p4
dev.off()
