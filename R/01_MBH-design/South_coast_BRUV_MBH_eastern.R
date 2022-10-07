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

tifs  <- list.files("data/spatial/rasters/", "eastern*", full.names = TRUE)
preds <- stack(tifs)
plot(preds)

# Make cuts for Daw
n <- 70

hist(preds$eastern_roughness)
roughness_qs <- c(0, 0.7, 0.9, 1)
roughness_cuts   <- quantile(preds$eastern_roughness, probs = roughness_qs)
cat_roughness  <- cut(preds$eastern_roughness, breaks = roughness_cuts, na.rm = TRUE)
plot(stack(preds$eastern_roughness, cat_roughness))
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
points( samp[,c("x","y")], pch=20, cex=1, col = "red")

## check spread of sites
# plot against inclusion probabilities
plot(inp_overall)
plot(tha_sites_sp, add = TRUE)

# get covariates
preds <- readRDS("output/covariate_rasters.rds")

site_covs <- cbind(tha_sites, extract(preds, tha_sites_sp))
site_c_w  <- melt(site_covs, id.vars = 1:4)
ggplot(site_c_w, aes(ID, value)) + 
  geom_point(alpha = 3/5) +
  geom_smooth() +
  facet_wrap(~ variable, scales = "free")

covdat <- readRDS("output/covariate_df.rds")
covd_w <- melt(covdat[3:6])
covd_w$source <- c("rasters")

sitedat <- data.frame("variable" = site_c_w$variable, "value" = site_c_w$value, "source" = c("sites"))
alldat  <- rbind(covd_w, sitedat)

ggplot(alldat, aes(variable, value, colour = source)) + 
  geom_violin() + 
  facet_wrap(~ variable, scales = "free")

hist(site_covs$site)
hist(site_covs$depth)
hist(site_covs$slope)


