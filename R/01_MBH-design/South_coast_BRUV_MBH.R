##National shelf habitat sampling design based on GA250 bathy with 70:30 effort in 0-120m and 120-250m respectively. 
##Rationale being that we have mud/silt starting at this point so need less sampling effort
##Prepared by jacquomo.monk@utas.edu.au
##1/09/2022

# Clear memory
rm(list=ls()) 


library(MBHdesign)
library(sp)
library(raster)
library(rgdal)
library(fields)
library(rasterVis)
library(tidyverse)
library(googledrive)
library(terra)
library(Rcpp)
library(SpaDES)
library(dplyr)

set.seed( 22)

##########################################Read in bathy raster##############################
# depth <- drive_download("shelf_depth.asc",                                      # Run once
#                         path = "data/spatial/rasters/raw bathymetry/shelf_depth.asc") 
inclProb_raster <- raster("data/spatial/rasters/raw bathymetry/Bremer-Israelite_inclProb_raster.tif") 

n <- 35000

samp <- quasiSamp.raster(n = n, 
                         inclusion.probs = inclProb_raster, 
                         randStartType = 2, 
                         nSampsToConsider = n*100) # may need to up this value if not running at a national scale
plot(inclProb_raster)
points( samp[,c("x","y")], pch=20, cex=0.01, col = "red")

## assign sampling order
samp$DropC <- 1:nrow(samp)

## Write out sampling file 
write.csv(samp,"data/mbh-design/BRUV_Bremer-Israelite.csv") # write out the each region

## now we have sampling design. worth checking in arcmap or similar that you have enough sampling density to be useful for each AMP that we intend to visit

# samp <- quasiSamp.raster(n = n, 
#                          inclusion.probs = !is.na(inclProb_raster), 
#                          randStartType = 2, 
#                          nSampsToConsider = n*500)
# points( samp[,c("x","y")], pch=20, cex=0.01, col = "red")
