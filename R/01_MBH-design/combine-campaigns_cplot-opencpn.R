###
# Project: NESP 2.1 - Esperance 2022-11
# Data:    BRUVS, BOSS
# Task:    Combine all campaigns, export for CPLOT and OpenCPN
# author:  Claude Spencer
# date:    November 2022
##

rm(list = ls())

library(tidyverse)
library(rgdal)
library(gpx)
library(sf)
source('R/01_MBH-design/functions.R')

daw.boss <- read.csv("data/mbh-design/Daw_MBH_wgs84_thinned.csv") %>%
  dplyr::select(x, y, DropC, sample)
daw.bruv <- read.csv("data/mbh-design/Eastern_BRUVs_MBH_wgs84.csv") %>%
  dplyr::select(x, y, DropC, sample)
inv.bruv <- read.csv("data/mbh-design/Investigator_BRUVs_MBH_wgs84.csv") %>%
  dplyr::select(x, y, DropC, sample)
inv.boss <- read.csv("data/mbh-design/Investigator_MBH_wgs84_thinned.csv") %>%
  dplyr::select(x, y, DropC, sample)
sal.boss <- read.csv("data/mbh-design/Salisbury_MBH_wgs84_thinned.csv") %>%
  dplyr::select(x, y, DropC, sample)
san.boss <- read.csv("data/mbh-design/Sandyhook_MBH_wgs84.csv") %>%
  dplyr::select(x, y, DropC, sample)
fig.boss <- read.csv("data/mbh-design/Figureofeight_MBH_wgs84.csv") %>%
  dplyr::select(x, y, DropC, sample)
sal.bruv <- read.csv("data/mbh-design/Salisbury_BRUVs_MBH_wgs84.csv") %>%
  dplyr::select(x, y, DropC, sample)

all.boss <- bind_rows(daw.boss, inv.boss, sal.boss,
                        san.boss, fig.boss) %>%
  glimpse()

all.bruv <- bind_rows(daw.bruv, inv.bruv, sal.bruv) %>%
  glimpse()

# Export to txt file ready for processing into cplot .MRK file
cplot.bruv <- data.frame("mark" = c("mark"),
                             "PXYCSLM" = c("PXYCSLM"),
                             DDtolatlon(all.bruv[, 1:2]),
                             "symbol" = c("Black Star"),
                             "ptcode" = all.bruv$sample,
                             c(0))
head(cplot.bruv)
write.table(cplot.bruv , "output/MBH-design/2022-11_Esperance_bruv_cplot.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

min(cplot.bruv$x)
max(cplot.bruv$x)
min(cplot.bruv$y)
max(cplot.bruv$y)

# Export to txt file ready for processing into cplot .MRK file
cplot.boss <- data.frame("mark" = c("mark"),
                         "PXYCSLM" = c("PXYCSLM"),
                         DDtolatlon(all.boss[, 1:2]),
                         "symbol" = c("Pink Star"),
                         "ptcode" = all.boss$sample,
                         c(0))
head(cplot.boss)
write.table(cplot.boss , "output/MBH-design/2022-11_Esperance_boss_cplot.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)


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

# then manually move this file to the cplot folder (avoids missing any files! erk)

# then run this to duplicate each file and convert the copy to .MRK file
txts <- list.files("output/MBH-design", "*.txt", full.names = T)
for(filei in txts){
  file.copy(filei, paste(gsub(".txt", ".MRK", filei)))
}

# Export the points to open CPN
# Load in the test way point to check the format
# testpoint <- readOGR("data/MBH-design/test waypoint.gpx")
# testdf <- as.data.frame(testpoint)

# BOSS to OpenCPN

cpn.boss <- all.boss %>%
  dplyr::rename("lon" = x,
                "lat" = y,
                "name" = sample) %>%
  dplyr::mutate(sym = "xmblue") %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  glimpse()

st_write(cpn.boss, dsn = "output/MBH-design/2022-11_Esperance_BOSS.gpx",
         dataset_options = "GPX_USE_EXTENSIONS=yes", layer = "waypoints",
         driver = "GPX", overwrite_layer = T)

# BRUV to OpenCPN
cpn.bruv <- all.bruv %>%
  dplyr::rename("lon" = x,
                "lat" = y,
                "name" = sample) %>%
  dplyr::mutate(sym = "xmred") %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  glimpse()

st_write(cpn.bruv, dsn = "output/MBH-design/2022-11_Esperance_BRUV.gpx",
         dataset_options = "GPX_USE_EXTENSIONS=yes", layer = "waypoints",
         driver = "GPX", overwrite_layer = T)
