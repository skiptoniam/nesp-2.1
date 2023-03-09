###
# Project: NESP 2.1
# Data:    National sampling plan
# Task:    Export MBH designs to cplot
# author:  Claude Spencer
# date:    February 2023
##

rm(list = ls())

library(tidyverse)
library(rgdal)
library(gpx)
library(sf)
library(nngeo)
library(GlobalArchive)
source('R/01_MBH-design/functions.R')

aug.boss <- read.csv("data/mbh-design/Augusta_BOSS_MBH_wgs84.csv") %>%
  glimpse() # Slightly different naming

swc.boss <- read.csv("data/mbh-design/SwC_BOSS_MBH_utm50.csv") %>%
  glimpse()

swc.bruv <- read.csv("data/mbh-design/SwC_BRUV_MBH_wgs84.csv") %>%
  # mutate(lon = as.character(lon),
  #        lat = as.character(lat)) %>%
  glimpse()

mma <- read_csv("data/mbh-design/SI-1301 Proposed Seabed Sample Locations Cape Leeuwin_Rev1.csv") %>%
  ga.clean.names() %>%
  dplyr::mutate(latitude = str_replace_all(.$latitude, "\xb0 ", ".")) %>%
  dplyr::mutate(latitude = str_replace_all(.$latitude, "' S", "")) %>%
  dplyr::mutate(longitude = str_replace_all(.$longitude, "\xb0 ", ".")) %>%
  dplyr::mutate(longitude = str_replace_all(.$longitude, "' E", "")) %>%
  separate(latitude, into = c("lat.hour", "lat.min", "lat.dec"), sep = "\\.") %>%
  dplyr::mutate(lat.dec = ifelse(str_length(.$lat.min) == 2,
                                 signif(as.numeric(lat.dec), digits = 5) %>% str_sub(start = 1, end = 5), 
                                 signif(as.numeric(lat.dec), digits = 6) %>% str_sub(start = 1, end = 6))) %>%
  separate(longitude, into = c("lon.hour", "lon.min", "lon.dec"), sep = "\\.") %>%
  dplyr::mutate(lon.dec = ifelse(str_length(.$lon.min) == 2,
                                 signif(as.numeric(lon.dec), digits = 4) %>% str_sub(start = 1, end = 4), 
                                 signif(as.numeric(lon.dec), digits = 5) %>% str_sub(start = 1, end = 5))) %>%
  dplyr::mutate(latitude = paste0(paste(lat.hour, lat.min, lat.dec, sep = "."), "S"),
                longitude = paste0(paste(lon.hour, lon.min, lon.dec, sep = "."), "E")) %>%
  glimpse()

mma.arc <- mma %>%
  dplyr::mutate(lon.dd = (as.numeric(paste(lon.min, lon.dec, sep = "."))/60),
                longitude = lon.dd + as.numeric(lon.hour),
                lat.dd = (as.numeric(paste(lat.min, lat.dec, sep = "."))/60),
                latitude = (lat.dd + as.numeric(lat.hour))*-1) %>%
  dplyr::select(longitude, latitude, sample.no, proposed.sampling.methods, dab.no) %>%
  glimpse()

write.csv(mma.arc, "data/mbh-design/MMA_BOSS_wgs84.csv", row.names = F)

# Export to txt file ready for processing into cplot .MRK file
# MMA samples
cplot.mma <- data.frame("mark" = c("mark"),
                             "PXYCSLM" = c("PXYCSLM"),
                             lon = mma$longitude,
                             lat = mma$latitude, 
                             "symbol" = c("Blue Star"),
                             "ptcode" = mma$sample.no,
                             c(0))
head(cplot.mma)
write.table(cplot.mma , "output/MBH-design/Augusta-SwC_2023-03/2023-03_MMA-backscatter.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

paste(min(cplot.mma$lat), min(cplot.mma$lon),
      max(cplot.mma$lat),max(cplot.mma$lon), sep = ",") # Add into 'Bounds' - not sure its needed or not 

# SwC BRUVs
cplot.bruv.swc <- data.frame("mark" = c("mark"),
                         "PXYCSLM" = c("PXYCSLM"),
                         DDtolatlon(swc.bruv[, 5:6]),
                         "symbol" = c("Blue Star"),
                         "ptcode" = swc.bruv$sample,
                         c(0))

head(cplot.bruv.swc)
write.table(cplot.bruv.swc , "output/MBH-design/Augusta-SwC_2023-03/2023-03_SwC_stereo-BRUVs.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

paste(min(cplot.bruv.swc$lat), min(cplot.bruv.swc$lon),
      max(cplot.bruv.swc$lat),max(cplot.bruv.swc$lon), sep = ",") # Add into 'Bounds' - not sure its needed or not 

# SwC BOSS
cplot.boss.swc <- data.frame("mark" = c("mark"),
                             "PXYCSLM" = c("PXYCSLM"),
                             DDtolatlon(swc.boss[, 5:6]),
                             "symbol" = c("Black Star"),
                             "ptcode" = swc.boss$sample,
                             c(0))
head(cplot.boss.swc)
write.table(cplot.boss.swc , "output/MBH-design/Augusta-SwC_2023-03/2023-03_SwC_BOSS.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

paste(min(cplot.boss.swc$lat), min(cplot.boss.swc$lon),
      max(cplot.boss.swc$lat),max(cplot.boss.swc$lon), sep = ",") # Add into 'Bounds' - not sure its needed or not 

# Augusta BOSS
cplot.boss.aug.commas <- data.frame("mark" = c("mark"),
                             "PXYCSLM" = c("PXYCSLM"),
                             lon = measurements::conv_unit(aug.boss$x, 
                                                           from = "dec_deg", 
                                                           to = "deg_dec_min"),
                             lat = measurements::conv_unit(aug.boss$y, 
                                                           from = "dec_deg", 
                                                           to = "deg_dec_min"),
                             # DDtolatlon(aug.boss[,1:2]),
                             "symbol" = c("Green Diamond"),
                             "ptcode" = aug.boss$sample,
                             c(0)) %>%
  separate(lon, into = c("lon.hour", "lon.dm"), sep = " ") %>%
  separate(lat, into = c("lat.hour", "lat.dm"), sep = " ") %>%
  dplyr::mutate(lon.dm = as.character(signif(as.numeric(lon.dm, digits = 6))),
                lat.dm = as.character(signif(as.numeric(lat.dm, digits = 6)))) %>%
  separate(lon.dm, into = c("lon.min", "lon.dec"), sep = "\\.") %>%
  separate(lat.dm, into = c("lat.min", "lat.dec"), sep = "\\.") %>%
  dplyr::mutate(lon.dec = as.character(signif(as.numeric(lon.dec, digits = 4))),
                lon.min = str_pad(.$lon.min, side = "left", pad = "0", width = 2)) %>%
  dplyr::mutate(lon.dec = str_trunc(.$lon.dec, width = 4,  side = "right", ellipsis = "")) %>%
  dplyr::mutate(lon.dec = str_pad(.$lon.dec, side = "right", pad = "0", width = 4),
                lon = paste0(paste(lon.hour, lon.min, lon.dec, sep = "."), ",E"),
                lat = sub('.', '',paste0(paste(lat.hour, lat.min, lat.dec, sep = "."), ",S"))) %>%
  dplyr::select(mark, PXYCSLM, lon, lat, symbol, ptcode, c.0.) %>%
  glimpse()

cplot.boss.aug <- data.frame("mark" = c("mark"),
                             "PXYCSLM" = c("PXYCSLM"),
                             lon = measurements::conv_unit(aug.boss$x, 
                                                           from = "dec_deg", 
                                                           to = "deg_dec_min"),
                             lat = measurements::conv_unit(aug.boss$y, 
                                                           from = "dec_deg", 
                                                           to = "deg_dec_min"),
                             # DDtolatlon(aug.boss[,1:2]),
                             "symbol" = c("Green Diamond"),
                             "ptcode" = aug.boss$sample,
                             c(0)) %>%
  separate(lon, into = c("lon.hour", "lon.dm"), sep = " ") %>%
  separate(lat, into = c("lat.hour", "lat.dm"), sep = " ") %>%
  dplyr::mutate(lon.dm = as.character(signif(as.numeric(lon.dm, digits = 6))),
                lat.dm = as.character(signif(as.numeric(lat.dm, digits = 6)))) %>%
  separate(lon.dm, into = c("lon.min", "lon.dec"), sep = "\\.") %>%
  separate(lat.dm, into = c("lat.min", "lat.dec"), sep = "\\.") %>%
  dplyr::mutate(lon.dec = as.character(signif(as.numeric(lon.dec, digits = 4))),
                lon.min = str_pad(.$lon.min, side = "left", pad = "0", width = 2)) %>%
  dplyr::mutate(lon.dec = str_trunc(.$lon.dec, width = 4,  side = "right", ellipsis = "")) %>%
  dplyr::mutate(lon.dec = str_pad(.$lon.dec, side = "right", pad = "0", width = 4),
                lon = paste0(paste(lon.hour, lon.min, lon.dec, sep = "."), "E"),
                lat = sub('.', '',paste0(paste(lat.hour, lat.min, lat.dec, sep = "."), "S"))) %>%
  dplyr::select(mark, PXYCSLM, lon, lat, symbol, ptcode, c.0.) %>%
  glimpse()

DDtolatlon(aug.boss[66,1:2])
DDtolatlon(aug.boss[81,1:2])

write.table(cplot.boss.aug , "output/MBH-design/Augusta-SwC_2023-03/2023-03_Augusta_BOSS_20230310.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)
write.table(cplot.boss.aug.commas , "output/MBH-design/Augusta-SwC_2023-03/2023-03_Augusta_BOSS_20230310-commas.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

paste(min(cplot.boss.aug$lat), min(cplot.boss.aug$lon),
      max(cplot.boss.aug$lat), max(cplot.boss.aug$lon), sep = ",") # Add into 'Bounds' - not sure its needed or not 

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
