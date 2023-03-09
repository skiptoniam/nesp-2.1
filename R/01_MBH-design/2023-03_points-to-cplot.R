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

test <- swc.bruv %>%
  cbind(DDtolatlon(swc.bruv[, 5:6]))

measurements::conv_unit(114.9009, 
                        from = "dec_deg", 
                        to = "deg_dec_min")

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
                             lon = measurements::conv_unit(aug.boss$x, 
                                                           from = "dec_deg", 
                                                           to = "deg_dec_min"),
                             lat = measurements::conv_unit(aug.boss$y, 
                                                           from = "dec_deg", 
                                                           to = "deg_dec_min"),
                             "symbol" = c("Black Star"),
                             "ptcode" = aug.boss$sample,
                             c(0)) %>%
  dplyr::mutate(lon = str_replace_all(.$lon, " ", "."),
                lat = str_replace_all(.$lat, " ", ".")) %>%
  dplyr::mutate(lat = str_replace_all(.$lat, "-", "")) %>%
  dplyr::mutate(lon = paste0(lon, "E"),
                lat = paste0(lat, "S")) %>%
  glimpse()

measurements::conv_unit(115.0014, from = "dec_deg", to = "deg_dec_min")
measurements::conv_unit(test[81,2], from = "dec_deg", to = "deg_dec_min")

test <- cplot.boss.aug %>%
  cbind(aug.boss) %>%
  # dplyr::mutate(lon = conv_unit(lon, from = "deg_dec_min", to = "dec_deg"),
  #               lat = conv_unit(lat, from = "deg_dec_min", to = "dec_deg")) %>%
  dplyr::select(lon, x, lat, y, everything()) %>%
  glimpse()

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
