###
# Project: NESP 2.1
# Data:    National sampling plan
# Task:    Load old sampling points, convert sample name and export to cplot
# author:  Claude Spencer
# date:    February 2023
##

rm(list = ls())

library(tidyverse)
library(rgdal)
library(gpx)
library(sf)
library(nngeo)
source('R/01_MBH-design/functions.R')

samps.boss <- read.csv("data/mbh-design/2021-03_West-Coast_BOSS_MBH-design.csv") %>% # "Site" matches to the 2021-03 campaign - Remove "SWC-NPZ-BOSS-"
  dplyr::select(Lon, Lat, Site) %>%
  dplyr::mutate(sample = paste("SWC-DC", 
                               str_pad(row_number(), 3,                     
                                       side = "left", pad = "0") , sep = "-"),
                parent.sample = str_replace_all(Site, "SWC-NPZ-BOSS-", "")) %>%
  glimpse()

write.csv(samps.boss, "data/mbh-design/2023-03_SwC_BOSS_wgs84.csv", row.names = F)

samps.bruv <- read.csv("data/mbh-design/2021-03_West-Coast_BOSS_MBH-design.csv") %>% # "Site" matches to the 2021-03 campaign - Remove "SWC-NPZ-BOSS-"
  dplyr::select(Lon, Lat, Site) %>%
  dplyr::mutate(sample = paste("SWC-BV", 
                               str_pad(row_number(), 3,                     
                                       side = "left", pad = "0") , sep = "-"),
                parent.sample = str_replace_all(Site, "SWC-NPZ-BOSS-", "")) %>%
  glimpse()

# Read in shapefile
bruv_sf1 <- st_as_sf(samps.bruv, coords = c("Lon", "Lat"))
bruv_sf2<- st_as_sf(samps.bruv, coords = c("Lon", "Lat"))
wgscrs <- CRS("+proj=longlat +datum=WGS84")
st_crs(bruv_sf1) <- wgscrs
st_crs(bruv_sf2) <- wgscrs

dists <- st_nn(bruv_sf1, bruv_sf2, k = 2, returnDist = T)
unlist(dists$nn)
unlist(dists$dist)

test <- dists %>%
  map_df(., unlist)

test <- st_join(bruv_sf1, bruv_sf2, join = st_nn, k = 2, maxdist = 500)

write.csv(samps.bruv, "data/mbh-design/2023-03_SwC_stereo-BRUVs_wgs84.csv", row.names = F)

samps.boss.aug <- read.csv("data/mbh-design/Augusta_BOSS_MBH_wgs84.csv") %>%
  dplyr::select(x, y, DropC, sample) %>%
  dplyr::rename(Lon = x, Lat = y) %>%
  glimpse()

# Export to txt file ready for processing into cplot .MRK file
# SwC BRUVs
cplot.bruv.swc <- data.frame("mark" = c("mark"),
                         "PXYCSLM" = c("PXYCSLM"),
                         DDtolatlon(samps.bruv[, 1:2]),
                         "symbol" = c("Black Star"),
                         "ptcode" = samps.bruv$sample,
                         c(0))
head(cplot.bruv.swc)
write.table(cplot.bruv.swc , "output/MBH-design/Augusta-SwC_2023-03/2023-03_SwC_stereo-BRUVs.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

min(cplot.bruv.swc$Lon)
max(cplot.bruv.swc$Lon)
min(cplot.bruv.swc$Lat)
max(cplot.bruv.swc$Lat)

# SwC BOSS
cplot.boss.swc <- data.frame("mark" = c("mark"),
                             "PXYCSLM" = c("PXYCSLM"),
                             DDtolatlon(samps.boss[, 1:2]),
                             "symbol" = c("Pink Star"),
                             "ptcode" = samps.boss$sample,
                             c(0))
head(cplot.boss.swc)
write.table(cplot.boss.swc , "output/MBH-design/Augusta-SwC_2023-03/2023-03_SwC_BOSS.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

min(cplot.boss.swc$Lon)
max(cplot.boss.swc$Lon)
min(cplot.boss.swc$Lat)
max(cplot.boss.swc$Lat)

# Augusta BOSS
cplot.boss.aug <- data.frame("mark" = c("mark"),
                             "PXYCSLM" = c("PXYCSLM"),
                             DDtolatlon(samps.boss.aug[, 1:2]),
                             "symbol" = c("Blue Star"),
                             "ptcode" = samps.boss.aug$sample,
                             c(0))
head(cplot.boss.aug)
write.table(cplot.boss.aug , "output/MBH-design/Augusta-SwC_2023-03/2023-03_Augusta_BOSS.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

min(cplot.boss.swc$Lon)
max(cplot.boss.swc$Lon)
min(cplot.boss.swc$Lat)
max(cplot.boss.swc$Lat)

# # couldn't figure out how to get this to work with the quotation marks so I just pasted manually
# "TMQ CPlot Chart Type 2   ",
# "Description: Ningaloo      ",
# "Bounds: 21.30.0000S,113.20.0000E,22.55.0000S,114.20.0000E      ",
# "Format: NM3      ",
# "Rev. Date: 110819/160554     ",
# "Scale: 1:00      ",
# "Mag. Variation: 0     ",
# "Cautions:       ",
# "  "  ,
# "<EOH>       "

# then run this to duplicate each file and convert the copy to .MRK file
txts <- list.files("output/MBH-design/Augusta-SwC_2023-03/", "*.txt", full.names = T)
for(filei in txts){
  file.copy(filei, paste(gsub(".txt", ".MRK", filei)))
}