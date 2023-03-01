###
# Project: NESP 2.1
# Data:    GA 250m Bathymetry
# Task:    Create an SwC sampling plan biased off roughness
# author:  Claude Spencer
# date:    February 2023
##

# Clear memory
rm(list=ls()) 

library(MBHdesign)
library(sf)
library(sp)
library(tidyverse)
library(raster)
library(dplyr)
library(stars)
library(starsExtra)

set.seed(69) 

wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")

tifs  <- list.files("data/spatial/rasters/", "swc*", full.names = TRUE)         # From the mac-swc repo
preds <- stack(tifs)
plot(preds)
names(preds) <- c("depth", "detrended","roughness")
crs(preds)

# Make cuts
n <- 48

hist(preds$roughness)
roughness_qs <- c(0, 0.85, 0.95, 1)
roughness_cuts   <- quantile(preds$roughness, probs = roughness_qs)
cat_roughness  <- cut(preds$roughness, breaks = roughness_cuts, na.rm = TRUE)
plot(stack(preds$roughness, cat_roughness))

roughness_split <- data.frame(zones = unique(cat_roughness),
                              split = c(0.1, 0.3, 0.6))
roughness_split$zbruv <- roughness_split$split * n
roughness_split

# create inclusion probability rasters for levels of each covariate
# choose carefully here - if you can make do with less rasters for selection, do that
# convert to data frame
icr_df    <- as.data.frame(cat_roughness, xy = TRUE, na.rm = T)
inp_rasts <- cat_roughness

# calculate inclusion probabilities for each variable
roughness_lvl_sum  <- table(icr_df$layer)
roughness_p_strata <- roughness_lvl_sum / sum(roughness_lvl_sum)
roughness_inclp    <- roughness_split$split / roughness_p_strata

# translate this onto inclusion probability rasters for each layer - leaving this in long format for easy interp
for(lev in 1:length(roughness_inclp)){
  inp_rasts$layer[inp_rasts$layer == lev] <- roughness_inclp[lev]
}
# 
# 
# # scale the layers so there is equal influence of each
for(rasti in 1:nlayers(inp_rasts)){
  inp_rasts[[rasti]] <- inp_rasts[[rasti]] / sum(rasti[], na.rm = TRUE)
}
# 
plot(inp_rasts)
inp_overall <- inp_rasts
plot(inp_overall)
cellStats(inp_overall, 'sum') # can't remember why we check this?
inp_overall[] <- inp_overall[] / sum(inp_overall[], na.rm = TRUE)
cellStats(inp_overall, 'sum') # to make it equal 1?

# not sure why but multiply inclusion p values so the sum equals the number of samples?

inp_overall[] <- inp_overall[] * n
plot(inp_overall)
cellStats(inp_overall, "sum")
inp_overall[is.na(inp_overall)] <- 0.00000000000000000000000000000000001        # Make the NAs very small prob

plot(inp_overall)

# select sites
samp <- quasiSamp.raster(n = n,
                         inclusion.probs = inp_overall,
                         randStartType = 2,
                         nSampsToConsider = 20000)

plot(inp_overall)
points( samp[,c("x","y")], pch=20, cex=1, col = "red")

## assign sampling order
samp$DropC <- 1:nrow(samp)
samp <- samp %>%
  dplyr::mutate(sample = paste("SWC-BV", 
                               str_pad(row_number(), 2,                     
                                       side = "left", pad = "0") , sep = "-"))

## Write out sampling file 
write.csv(samp,"data/mbh-design/SwC_BRUV_MBH_utm50.csv", row.names = F) # Need to manually move some points to >250m separation

# Move points manually in QGIS then run the next part
samp_m <- st_read("data/mbh-design/SwC_BRUV_MBH_utm50.shp")                      
samp_t <-  st_transform(samp_m, crs = wgscrs) %>%
  dplyr::mutate(lon = st_coordinates(.)[,1],
                lat = st_coordinates(.)[,2]) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry, x, y)) %>%
  glimpse()

write.csv(samp_t,"data/mbh-design/SwC_BRUV_MBH_wgs84.csv", row.names = F)
