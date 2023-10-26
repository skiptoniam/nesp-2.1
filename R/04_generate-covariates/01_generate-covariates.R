rm(list = ls())

library(terra)
library(sf)
library(tidyverse)

aus <- st_read("data/spatial/shapefiles/cstauscd_r.mif",
               crs = 4283) %>%
  st_transform(4326) %>%
  dplyr::filter(!FEAT_CODE %in% "sea") %>%
  terra::vect()

# plot(aus, max.plot = 1)

bathy <- terra::rast("data/spatial/rasters/raw bathymetry/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
  terra::crop(ext(110, 155, -45, -23.4394)) %>%
  terra::mask(aus, inverse = T) %>%
  terra::clamp(lower = -250, values = F) %>%
  terra::trim()
plot(bathy)


