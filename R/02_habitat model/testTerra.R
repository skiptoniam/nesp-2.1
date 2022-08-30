# Clear your environment
rm(list = ls())

# Load libraries - some more to add here
library(sp)
library(raster)
library(terra)
library(sf)
library(stars)
library(starsExtra)
library(dplyr)

# Set your study name

preds.terra <- readRDS("data/spatial/rasters/Abrolhos_spatial_covariates.rds")
preds.terra <- rast(preds.terra)

preds.raster <- readRDS("data/spatial/rasters/test-raster-package.rds")

# Load in tidy data from the formatting scripts
boss <- read.csv("data/tidy/2021-05_Abrolhos_BOSS_random-points_broad.habitat.csv") %>% # Need to type filename manually
  dplyr::mutate(method = "BOSS") %>%                                            # Change here
  dplyr::filter(location %in% "NPZ6") %>%                                       # Only Shallow Bank
  glimpse()

bruv <- read.csv("data/tidy/2021-05_Abrolhos_BRUVs_random-points_broad.habitat.csv") %>% # Need to type filename manually
  dplyr::mutate(method = "BRUV") %>%                                            # Change here
  dplyr::filter(location %in% "NPZ6") %>%                                       # Only Shallow Bank
  glimpse()

hab  <- bind_rows(boss, bruv)  

# Align crs and check samples over bathy and extract terrain data
allhab_sp <- vect(hab, geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84 +south")           # Convert the habitat data to a SpatialPoints dataframe
# allhab_t <- spTransform(allhab_sp, CRS = sppcrs)
habt_df   <- as.data.frame(allhab_sp)                                           # Convert the habitat data back to a regular dataframe
habi_df   <- cbind(habt_df, terra::extract(preds.terra, allhab_sp))                  # Extract the bathymetry derivatives and join on as a dataframe


# FOr raster
allhab_sp2 <- SpatialPointsDataFrame(coords = hab[3:2], data = hab,
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +south"))
habi_df2   <- cbind(habt_df, raster::extract(preds.raster, allhab_sp))                  # Extract the bathymetry derivatives and join on as a dataframe

