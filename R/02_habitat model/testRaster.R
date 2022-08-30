# Clear your environment
rm(list = ls())

# Load libraries - some more to add here
library(sp)
library(raster)
library(sf)
library(stars)
library(starsExtra)

# Set your study name
name <- "Abrolhos"                                                              # Change here

# Set CRS for bathymetry data
wgscrs <- CRS("+proj=longlat +datum=WGS84 +south")                                   # Latlong projection 

# This next section uses coarse GA bathymetry, replace if you have better bathymetry data (ie. multibeam or LiDAR)
# Read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
cbaths      <- list.files("data/spatial/rasters/raw bathymetry",                # Bathymetry data too large for git stored here
                          "*tile", full.names = TRUE) 
cbathy      <- lapply(cbaths,                                                   # Loads all of the tiles
                      function(x){read.table(file = x, header = TRUE, sep = ",")})    
cbathy      <- do.call("rbind", lapply(cbathy, as.data.frame))                  # Turns the list into a data frame
cbathy      <- cbathy[cbathy$Z <= 0 & cbathy$X < 117, ]                         # Get rid of topography data above 0m, general crop to speed life up
bath_r      <- rasterFromXYZ(cbathy)                                            # Convert to a raster
crs(bath_r) <- wgscrs                                                           # Set the CRS
plot(bath_r)                                                                    # Plot to check everything looks ok

# Crop the bathymetry to the general study area
# tbath <- projectRaster(bath_r, crs = sppcrs)
# tbath_c <- crop(tbath, extent(c(105000, 165000, 6880000, 7000000)))
tbath_c <- crop(bath_r, raster::extent(c(112.978992634, 113.622204887,-28.146712369, -27.081850499)))
plot(tbath_c)
fbath_df <- as.data.frame(tbath_c, xy = TRUE)                                   # Convert this to a dataframe
# saveRDS(fbath_df, paste(paste0('data/spatial/rasters/',                         # Save it for use in the next scripts
#                                name), 'ga_bathy.rds', sep = "_")) 

# Calculate raster terrain derivatives
preds.raster <- raster::terrain(tbath_c, neighbors = 8,
                              opt = c("slope", "aspect", "TPI", "TRI", "roughness"))           # Remove here as necessary
preds.raster <- raster::stack(tbath_c, preds.raster)                                             # Stack the derivatives with the bathymetry
plot(preds.raster)

saveRDS(preds.raster, "data/spatial/rasters/test-raster-package.rds")
