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
source('R/01_MBH-design/functions.R')

aug.boss <- read.csv("data/mbh-design/Augusta_BOSS_MBH_wgs84.csv") %>%
  glimpse() # Slightly different naming

swc.boss <- read.csv("data/mbh-design/SwC_BOSS_MBH_utm50.csv") %>%
  glimpse()

swc.bruv <- read.csv("data/mbh-design/SwC_BRUV_MBH_wgs84.csv") %>%
  glimpse()


# Export to txt file ready for processing into cplot .MRK file
# SwC BRUVs
cplot.bruv.swc <- data.frame("mark" = c("mark"),
                         "PXYCSLM" = c("PXYCSLM"),
                         DDtolatlon(swc.bruv[, 5:6]),
                         "symbol" = c("Black Star"),
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
                             "symbol" = c("Pink Star"),
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
                             "symbol" = c("Blue Star"),
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