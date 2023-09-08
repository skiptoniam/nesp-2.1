###
# Project: SW Habitat & Fish 
# Data:    FOV habitat data
# Task:    Format Esperance & Bremer habitat data
# Author:  Claude Spencer
# Date:    February 2023
## 

# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
library(GlobalArchive)

# To tidy data
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

# Study name ----
study <- "2022-11_Esperance" 

## Set your working directory ----
working.dir <- getwd() # this only works through github projects

## Save these directory names to use later----
data.dir <- paste(working.dir,"data",sep="/") 
raw.dir <- paste(data.dir,"raw",sep="/") 
em.export.dir <- raw.dir # Hehe
tm.export.dir <- raw.dir # Hehe

# Read in metadata----
read_files_csv <- function(flnm) {
  flnm %>%
    readr::read_csv(col_types = readr::cols(.default = "c")) %>%
    GlobalArchive::ga.clean.names() %>%
    dplyr::mutate(campaign.naming = str_replace_all(flnm, paste0(em.export.dir,"/"),"")) %>%
    tidyr::separate(campaign.naming,into = c("campaignid"), sep="/", extra = "drop", fill = "right") %>%
    dplyr::mutate(campaignid=str_replace_all(.$campaignid,c("_Metadata.csv"= "")))
}


metadata <- list.files(path = raw.dir, 
                       recursive = T,
                       pattern = "_Metadata.csv",
                       full.names = T)  %>% # read in the file
  purrr::map_dfr(~read_files_csv(.)) %>%
  dplyr::select(campaignid, sample, latitude, longitude, date, site, location, successful.count, depth) %>% # select only these columns to keep
  mutate(sample = as.character(sample)) %>% # in this example dataset, the samples are numerical
  glimpse() # preview

unique(metadata$campaignid)
names(metadata)

# read in the points annotations ----
read_tm_delim <- function(flnm) {
  read.delim(flnm,header = T,skip = 4,stringsAsFactors = FALSE, colClasses = "character") %>%
    dplyr::mutate(campaign.naming = str_replace_all(flnm, paste0(tm.export.dir,"/"),"")) %>%
    tidyr::separate(campaign.naming,into = c("campaignid"), sep="/", extra = "drop", fill = "right") %>%
    # dplyr::mutate(relief.file = ifelse(str_detect(campaignid, "Relief"), "Yes", "No")) %>%
    dplyr::mutate(direction = ifelse(str_detect(campaignid, "Backwards"), "Backwards", "Forwards")) %>%
    dplyr::mutate(campaignid = str_replace_all(.$campaignid,c("_Backwards_Dot Point Measurements.txt"= "",
                                                              "_Forwards_Dot Point Measurements.txt"= "",
                                                              "_Backwards_Relief_Dot Point Measurements.txt" = "",
                                                              "_Forwards_Relief_Dot Point Measurements.txt" = "",
                                                              "_Relief_Dot Point Measurements.txt" = "",
                                                              "_Dot Point Measurements.txt"= "")))
}

habitat <- list.files(path = tm.export.dir,
                      recursive = T,
                      pattern = "Dot Point Measurements.txt",
                      full.names = T) %>%
  purrr::map_dfr(~read_tm_delim(.)) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample = ifelse(str_detect(campaignid, "BRUV"), as.character(opcode), as.character(period))) %>%
  dplyr::mutate(sample = ifelse(is.na(sample), str_replace_all(filename, c(".jpg" = "",
                                                                           ".png" = "",
                                                                           "\\_" = "\\-")), sample)) %>%
  separate(catami_l2_l3, into = c("catami_l2", "catami_l3"), sep = " > ") %>%
  dplyr::select(campaignid, sample,image.row,image.col,
                catami_l2, catami_l3, catami_l4) %>%     # select only these columns to keep
  dplyr::mutate(sample = ifelse(sample %in% "DAW-DC-C04", "DAW-DC-CO4", 
                                ifelse(sample %in% "DAW-DC-C05", "DAW-DC-CO5", 
                                       ifelse(sample %in% "DAW-DC-C06", "DAW-DC-CO6", sample)))) %>%
  glimpse() # preview

unique(habitat$sample)
unique(habitat$campaignid)

no.annotations <- habitat %>%
  group_by(campaignid, sample) %>%
  dplyr::summarise(points.annotated=n()) # Looks fine

# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(habitat,metadata, 
                              by = c("campaignid","sample"))                  
missing.habitat <- anti_join(metadata,habitat, 
                             by = c("campaignid","sample"))
unique(missing.habitat$sample)
unique(missing.metadata$sample)

# CREATE broad classifications
con <- read.csv("data/raw/CATAMI-UWA_conversion.csv") %>%
  ga.clean.names() %>%
  separate(catami_l2_l3, into = c("catami_l2", "catami_l3"), sep = " > ") %>%
  dplyr::mutate(catami_l2 = str_trim(catami_l2)) %>%
  dplyr::mutate(catami_l3 = str_trim(catami_l3)) %>%
  dplyr::mutate(catami_l4 = str_trim(catami_l4)) %>%
  dplyr::select(-catami_l5) %>%
  distinct() %>%
  glimpse()

broad.points <- habitat %>%
  left_join(con) %>%
  dplyr::mutate(id = 1:nrow(.)) %>% # Key for tidyr::spread
  dplyr::select(-c(catami_l2, catami_l3, catami_l4, image.row, 
                   image.col)) %>%
  dplyr::filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water")) %>%
  dplyr::mutate(broad = paste("broad",broad,sep = ".")) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(campaignid, sample) %>%
  tidyr::spread(key = broad, value = count, fill = 0) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise_all(funs(sum)) %>%
  ungroup() %>%
  dplyr::select(-id) %>%
  dplyr::mutate(broad.total.points.annotated=rowSums(.[,3:(ncol(.))],na.rm = TRUE )) %>%
  ga.clean.names() %>%
  glimpse()

unique(broad.points$campaignid)

# Write final habitat data----
habitat.broad.points <- metadata %>%
  left_join(broad.points, by = c("campaignid","sample")) %>%
  dplyr::filter(!is.na(broad.total.points.annotated)) %>%
  glimpse()

unique(habitat.broad.points$campaignid)

write.csv(habitat.broad.points,file = paste0("data/tidy/",study,"_random-points_broad.habitat.csv"), 
          row.names=FALSE)
