###
# Project: NESP 2.1
# Data:    GA 250m Bathymetry
# Task:    Create an Esperance sampling plan biased off roughness
# author:  Claude Spencer
# date:    September 2022
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

set.seed(22)

wgscrs <- CRS("+proj=longlat +datum=WGS84")

tifs.inv  <- list.files("data/spatial/rasters/", "investigator*", full.names = TRUE)
preds.inv <- stack(tifs.inv)
extent(preds.inv)
preds.inv <- crop(preds.inv, extent(120.855, 121.18, -34.15, -33.895))
plot(preds.inv)

# Make cuts for Investigator
n <- 72                                                                         # Number of samples

hist(preds.inv$investigator_roughness)
roughness_qs <- c(0, 0.7, 0.9, 1)
roughness_cuts   <- quantile(preds.inv$investigator_roughness, probs = roughness_qs)
cat_roughness  <- cut(preds.inv$investigator_roughness, breaks = roughness_cuts, na.rm = TRUE)
plot(stack(preds.inv$investigator_roughness, cat_roughness))
roughness_split <- data.frame(zones = unique(cat_roughness),
                              split = c(0.2, 0.4, 0.4))
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

# bruv >350m apart - adding a few extra sites, manually remove any that are too near

## select sites

#having issues choosing locations within these separate sites
samp <- quasiSamp.raster(n = n,
                       inclusion.probs = inp_overall,
                       randStartType = 2,
                       nSampsToConsider = 20000)

plot(inp_overall)
points( samp[,c("x","y")], pch = 20, cex=1, col = "red")


## assign sampling order
samp$DropC <- 1:nrow(samp)
samp <- samp %>%
  dplyr::mutate(sample = paste("INV-BV", 
                               str_pad(row_number(), 2,                     
                                       side = "left", pad = "0") , sep = "-"))

## Write out sampling file 
write.csv(samp,"data/mbh-design/Investigator_BRUVs_MBH_wgs84.csv") # write out the each region