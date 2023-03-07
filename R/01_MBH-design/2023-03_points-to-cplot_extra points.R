###
# Project: NESP 2.1
# Data:    National sampling plan
# Task:    Export MBH designs to cplot
# author:  Claude Spencer
# date:    February 2023
##

rm(list = ls())

library(tidyverse)
library(dplyr)
library(tidyr)
library(rgdal)
library(gpx)
library(sf)
library(nngeo)
library(GlobalArchive)
source('R/01_MBH-design/functions.R')

mma <- read.csv("data/mbh-design/13011_Cape_Leeuwin_Submerged_Cultural_Heritage_Landscapes_bg.csv") %>%
  ga.clean.names() %>%
  dplyr::mutate(latitude = str_replace_all(.$wgs84.latitude.dm., "\xb0 ", ".")) %>%
  dplyr::mutate(latitude = str_replace_all(.$latitude, "' S", "")) %>%
  dplyr::mutate(longitude = str_replace_all(.$wgs84.latitude.dm., "\xb0 ", ".")) %>%
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


names(mma)

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
cplot.boss.aug <- data.frame("mark" = c("mark"),
                             "PXYCSLM" = c("PXYCSLM"),
                             DDtolatlon(aug.boss[, 1:2]),
                             "symbol" = c("Black Star"),
                             "ptcode" = aug.boss$sample,
                             c(0))
head(cplot.boss.aug)
write.table(cplot.boss.aug , "output/MBH-design/Augusta-SwC_2023-03/2023-03_Augusta_BOSS.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

paste(min(cplot.boss.aug$y), min(cplot.boss.aug$x),
      max(cplot.boss.aug$y), max(cplot.boss.aug$x), sep = ",") # Add into 'Bounds' - not sure its needed or not 

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
