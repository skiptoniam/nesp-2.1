###
# Project: Parks Eastern Recherche
# Data:    250m bathy
# Task:    Overview maps
# author:  Claude Spencer
# date:    August 2022
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

# Set CRS for transformations
wgscrs <- "+proj=longlat +datum=WGS84" 

# Get data and sort spatial boundaries
aus    <- st_read("data/spatial/shapefiles/aus-shapefile-w-investigator.shp")   # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
esp_mp <- aumpa[aumpa$ResName%in%c("Eastern Recherche", "South-west Corner"),]
# esp_nmp <- esp_mp[esp_mp$ZoneName %in% "National Park Zone", ]
e <- ext(120, 124.3, -36, -33.5)
esp_mp <- st_crop(esp_mp, e)
esp_mp$ZoneName <- factor(esp_mp$ZoneName, levels = c("Multiple Use Zone", "Special Purpose Zone",
                                                      "National Park Zone"))
terrnp <- st_read("data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
terrnp <- st_crop(terrnp, xmin = 120, ymin = -36, xmax = 124.3, ymax = -33.5)   # Just the study area

cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit

# Super slow - turn off if not needed !!!
# cbaths <- list.files("data/spatial/rasters/large", "*tile", full.names = TRUE)
# cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
# cbathy <- do.call("rbind", lapply(cbathy, as.data.frame)) 
# bathy <- rast(cbathy)
# crs(bathy) <- wgscrs

# Crop to general project area of Investigator Island (inv)
# inv_spat <- vect(esp_mp[esp_mp$ResName %in% c("South-west Corner"),])           # Spatial frame to buffer with                                             # To a spatial dataframe to use buffer
# inv_bathy <- crop(bathy, buffer(inv_spat, width = 0.05)) check width!!                        # Crop to general study area
# inv_bathy[inv_bathy > 0] <- NA                                                  # Remove topography data
# inv_bathy[inv_bathy < -150] <- NA                                               # Remove data past 150m
# plot(inv_bathy)
# plot(esp_mp, add = T)
# inv_bathdf <- as.data.frame(inv_bathy, na.rm = T, xy = T)                       # As a dataframe for plotting
# colnames(inv_bathdf)[3] <- "Depth"
# saveRDS(inv_bathdf, file = "data/spatial/rasters/Investigator_GA_250m_bathy-to150m.RDS")    # Save it out

inv_bathdf <- readRDS("data/spatial/rasters/Investigator_GA_250m_bathy-to150m.RDS")
inv_bathy <- rast(inv_bathdf)
crs(inv_bathy) <- wgscrs

# Hillshading for exploratory plot
slope <- terrain(inv_bathy, v = 'slope', unit = 'degrees')
aspect <- terrain(inv_bathy, v = 'aspect', unit = 'degrees')
hill <- shade(slope, aspect, angle = 70, direction = 0)                         # Change here
inv_hill <- as.data.frame(hill, xy = T, na.rm = T)                              # To a dataframe for plotting

# Crop to general project area of Eastern Recherche (esp)
# esp_spat <- vect(esp_nmp[esp_nmp$ResName %in% c("Eastern Recherche"),])   # Spatial frame to buffer with                                            # To a spatial dataframe to use buffer
# esp_bathy <- crop(bathy, buffer(esp_spat, width = 0.05)) Check width!                       # Crop to general study area
# esp_bathy[esp_bathy > 0] <- NA                                                  # Remove topography data
# esp_bathy[esp_bathy < -150] <- NA                                               # Remove data beyond 150m
# plot(esp_bathy)
# plot(esp_mp, add = T)
# esp_bathdf <- as.data.frame(esp_bathy, na.rm = T, xy = T) 
# colnames(esp_bathdf)[3] <- "Depth"
# saveRDS(esp_bathdf, file = "data/spatial/rasters/Daw_GA_250m_bathy-to150m.RDS") # Save it out

esp_bathdf <- readRDS("data/spatial/rasters/Daw_GA_250m_bathy-to150m.RDS")
esp_bathy <- rast(esp_bathdf)
crs(esp_bathy) <- wgscrs

# Hillshading for exploratory plot
slope <- terrain(esp_bathy, v = 'slope', unit = 'degrees')
aspect <- terrain(esp_bathy, v = 'aspect', unit = 'degrees')
hill <- shade(slope, aspect, angle = 70, direction = 0)                       # Change here
esp_hill <- as.data.frame(hill, xy = T, na.rm = T)                              # To a dataframe for plotting

waterr_cols <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                            "Nature Reserve" = "#e4d0bb"), name = "State Managed Areas")

# Investigator Island
# build basic plot elements
p1 <- ggplot() +
  geom_tile(data = inv_hill,aes(x = x, y = y, fill = lyr1), alpha = 1) +
  scale_fill_gradient(low = "white", high = "black", guide = "none") +
  new_scale_fill() +
  geom_tile(data = inv_bathdf, aes(x = x, y = y, fill = Depth), alpha = 0.7) +
  scale_fill_viridis() +
  new_scale_fill() +
  geom_contour(data = inv_bathdf, aes(x = x, y = y, z = Depth), breaks = c(0, -30, -70, - 200),
               colour = "white", size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  waterr_cols +
  new_scale_fill() +
  geom_sf(data = esp_mp, aes(color = ZoneName), fill = NA, size = 0.4) +
  scale_color_manual(values = c("Special Purpose Zone" = "#6daff4",
                                "National Park Zone" = "#7bbc63",
                                "Multiple Use Zone" = "#b9e6fb")) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.2) +
  # coord_sf(xlim = c(120.8, 121.2), ylim = c(-34.3, -33.84875)) +
  annotate(geom = "text", x = c(120.8, 120.8, 120.8), y = c(-33.95, -34.08, -34.24), 
           label = c("30m", "70m", "70m"), size = 2, color = "white") +
  coord_sf(xlim = c(120.625, 121.5), ylim = c(-34.58875, -33.8)) +
  labs(y = "Latitude", x = "Longitude", color = "Australian Marine Parks") +
  theme_minimal()
png(filename = "plots/investigator-exploratory-site-plot.png", height = 6, width = 8,
  res = 300, units = "in")
p1
dev.off()

# Eastern Recherche / Daw Island
# build basic plot elements
p2 <- ggplot() +
  geom_tile(data = esp_hill,aes(x = x, y = y, fill = lyr1), alpha = 1) +
  scale_fill_gradient(low = "white", high = "black", guide = "none") +
  new_scale_fill() +
  geom_tile(data = esp_bathdf, aes(x = x, y = y, fill = Depth), alpha = 0.7) +
  scale_fill_viridis() +
  new_scale_fill() +
  geom_contour(data = esp_bathdf, aes(x = x, y = y, z = Depth), breaks = c(0, -30, - 70, -200),
               colour = "white", size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  waterr_cols +
  new_scale_fill() +
  geom_sf(data = esp_mp, aes(color = ZoneName), fill = NA, size = 0.4) +
  scale_color_manual(values = c("Special Purpose Zone" = "#6daff4",
                                "National Park Zone" = "#7bbc63",
                                "Multiple Use Zone" = "#b9e6fb")) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.2) +
  annotate(geom = "text", x = c(124.1, 124.1, 124.1), y = c(-33.77, -33.95, -34.2), 
           label = c("30m", "70m", "70m"), size = 2, color = "white") +  
  geom_sf(data = tran, linetype = "dashed", alpha = 0.3) + # I run this bit later in the script - bit silly
  coord_sf(xlim = c(123.2837, 124.3487), ylim = c(-34.62375, -33.55125)) +
  labs(y = "Latitude", x = "Longitude", color = "Australian Marine Parks") +
  theme_minimal()
png(filename = "plots/eastern-recherche-exploratory-site-plot.png", height = 6, width = 8,
    res = 300, units = "in")
p2
dev.off()

# Bathymetry cross section
cbaths <- list.files("data/spatial/rasters/large", "*tile", full.names = TRUE)
cbath <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbath <- do.call("rbind", lapply(cbath, as.data.frame))

# Eastern Recherche / Daw Island
# Make sf point
points <- data.frame(x = c(123.95, 124.4), 
                      y = c(-33.5, -34.4), id = 1)

tran <- sfheaders::sf_linestring(obj = points,
                         x = "x", 
                         y = "y",
                         linestring_id = "id")
st_crs(tran) <- wgscrs

tranv <- vect(tran)
dep <- rast(cbath)

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
  dplyr::mutate("distance.from.coast" = st_distance(bath_cross, ausout),
                x = unlist(map(bath_cross$geometry, 1)),
                y = unlist(map(bath_cross$geometry, 2)),
                land = lengths(st_intersects(bath_cross, aus)) > 0) %>%
  glimpse()

bath_df1 <- as.data.frame(bath_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::rename(depth = "Z") %>%
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

p3 <- ggplot() +
  geom_rect(aes(xmin = min(bath_df1$distance.from.coast), xmax = max(bath_df1$distance.from.coast), ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  geom_line(data = bath_df1, aes(y = depth, x = distance.from.coast)) +
  geom_ribbon(data = bath_df1, aes(ymin = -Inf, ymax = depth, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "Distance from coast (km)", y = "Elevation (m)") +
  # geom_segment(data = paleo, aes(x = distance.from.coast, xend = distance.from.coast + 20, 
  #                                y = depth, yend = depth), linetype = 2, alpha = 0.5) +
  # geom_text(data = paleo, aes(x = distance.from.coast + 26, y = depth, label = label), size = 3) +
  annotate(geom = "text", x = c(-20, - 35), y = 10, label = c("Whaleback Island", "Daw Island"), size = 2.5)
png(filename = "plots/eastern-recherche-cross-section.png", width = 8, height = 4,
    units = "in", res = 200)
p3
dev.off()


jacmap <- rast("data/spatial/rasters/ecosystem-types-19class-naland.tif")      # Jac's aus habitat predictions
jacmap <- crop(jacmap, e)
terrnp <- st_read("data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
# reduce terrestrial parks
terrnp <- st_crop(terrnp, xmin = 120, ymin = -36, xmax = 124.3, ymax = -33.5)   # Just the study area
cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters line
inv_bathdf <- readRDS("data/spatial/rasters/Investigator_GA_250m_bathy-to150m.RDS") # Investigator Island bathy 
esp_bathdf <- readRDS("data/spatial/rasters/Daw_GA_250m_bathy-to150m.RDS")      # Daw Island bathy

# Deleted all the other habitat plots, bring back in later

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
jcls_cols <- scale_fill_manual(values = c(
  "Shelf unvegetated soft sediments" = "cornsilk1",
  "Shelf vegetated sediments" = "seagreen3",
  "Shallow rocky reefs less than 30 m depth" = "darkgoldenrod1",
  "Mesophotic rocky reefs" = "khaki4",
  "Rariophotic shelf reefs" = "steelblue3",
  "Upper slope unvegetated soft sediments" = "wheat1",
  "Mid slope sediments" = "#f7d29c",
  "Upper slope rocky reefs shelf break to 700 m depth" = "indianred3",
  "Artificial reefs pipelines and cables" = "saddlebrown",
  "Mid slope reef" = "azure4"))

waterr_cols <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                            "Nature Reserve" = "#e4d0bb"),
                                 guide = "none")

# assign mpa colours - full levels are saved at end of script for future ref
nmpa_cols <- scale_color_manual(values = c("National Park Zone" = "#7bbc63",
                                           "Special Purpose Zone" = "#6daff4",
                                           "Multiple Use Zone" = "#b9e6fb"))

# ab_nmp$ZoneName <- factor(ab_nmp$ZoneName, levels = c("Multiple Use Zone", "Special Purpose Zone",
#                                                       "Habitat Protection Zone","National Park Zone"))

# Build plot Investigator Island
p7 <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  waterr_cols +
  new_scale_fill() +
  geom_tile(data = jmap_df, aes(x, y, fill = exp.ecosystem.names)) +
  jcls_cols +
  geom_contour(data = inv_bathdf, aes(x = x, y = y, z = Depth),
               breaks = c(-30, -70, -200, - 700, - 7000), colour = "black", alpha = 1, size = 0.18) +
  geom_sf(data = esp_mp, fill = NA, aes(colour = ZoneName)) +
  nmpa_cols+
  labs(color = "Australian Marine Parks") +
  new_scale_color() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  annotate(geom = "text", x = c(120.8, 120.8, 120.8), y = c(-33.95, -34.08, -34.24), 
           label = c("30m", "70m", "70m"), size = 2, color = "black") +
  coord_sf(xlim = c(120.625, 121.5), ylim = c(-34.58875, -33.8)) +
  labs(fill = "Habitat classification", x = NULL, y = NULL) +
  theme_minimal()

png(filename = "plots/investigator_jmonk_natmap.png", width = 8, height = 6,
    units = "in", res = 200)
p7
dev.off()

# Set colours for Eastern Recherche
jcls_cols <- scale_fill_manual(values = c(
  "Shelf unvegetated soft sediments" = "cornsilk1",
  "Shelf vegetated sediments" = "seagreen3",
  "Shallow rocky reefs less than 30 m depth" = "darkgoldenrod1",
  "Mesophotic rocky reefs" = "khaki4",
  "Rariophotic shelf reefs" = "steelblue3",
  "Upper slope unvegetated soft sediments" = "wheat1",
  "Mid slope sediments" = "#f7d29c",
  "Upper slope rocky reefs shelf break to 700 m depth" = "indianred3",
  "Artificial reefs pipelines and cables" = "saddlebrown",
  "Mid slope reef" = "azure4",
  "Lower slope reef and sediments" = "darkgoldenrod4"))


# Build plot Eastern Recherche
p8 <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  waterr_cols +
  new_scale_fill() +
  geom_tile(data = jmap_df, aes(x, y, fill = exp.ecosystem.names)) +
  jcls_cols +
  geom_contour(data = esp_bathdf, aes(x = x, y = y, z = Depth),
               breaks = c(-30, -70, -200, - 700, - 7000), colour = "black", alpha = 1, size = 0.18) +
  geom_sf(data = esp_mp, fill = NA, aes(colour = ZoneName)) +
  nmpa_cols+
  labs(color = "Australian Marine Parks") +
  new_scale_color() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  annotate(geom = "text", x = c(120.8, 120.8, 120.8), y = c(-33.95, -34.08, -34.24), 
           label = c("30m", "70m", "70m"), size = 2, color = "black") +
  coord_sf(xlim = c(123.2837, 124.3487), ylim = c(-34.62375, -33.55125)) +
  labs(fill = "Habitat classification", x = NULL, y = NULL) +
  theme_minimal()

png(filename = "plots/eastern-recherche_jmonk_natmap.png", width = 8, height = 6,
    units = "in", res = 200)
p8
dev.off()

