###
# Project: NESP 2.1 - Esperance 2022-11
# Data:    BRUVS, BOSS
# Task:    Add metadata to meg labsheets from marks added to iPads using collector 
# author:  Brooke Gibbons
# date:    December 2022
##

library(tidyverse)
library(GlobalArchive)
library(lubridate)
library(googlesheets4)

# Find valid timezone
# Search for Perth, change to other location if the samples were not in WA.
grep("Perth", OlsonNames(), value=TRUE)

metadata.names <- c(system.number = NA_real_,
                    sample = NA_real_,
                    latitude = NA_real_,
                    longitude = NA_real_,
                    date = NA_real_,
                    time = NA_real_,
                    local.time = NA_real_,
                    site = NA_real_,
                    location = NA_real_,
                    status = NA_real_,
                    depth = NA_real_,
                    observer = NA_real_,
                    successful.count = NA_real_,
                    successful.length = NA_real_,
                    video.notes = NA_real_,
                    left.camera = NA_real_,
                    right.camera = NA_real_,
                    rear.camera = NA_real_)

# Check files in data folder 
dir("data/metadata/")

# Read in cameras 
bruv.cameras <- read.csv("data/metadata/BRUV_cameras_template_0.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(system.number = ï.system.)%>%
  dplyr::mutate(time = mdy_hms(creationdate, tz = "UTC")) %>%
  dplyr::mutate(local.time = with_tz(time, tz = "Australia/Perth")) %>%
  dplyr::mutate(date = substr(.$local.time, 1, 10)) %>%
  dplyr::select(system.number, left.camera, right.camera, rear.camera, date) %>%
  glimpse()

# Read in metadata
bruv.metadata <- read.csv("data/metadata/BRUV_metadata_template_0.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(system.number = ï.system., depth = depth.m., longitude = x, latitude = y, video.notes = notes)%>%
  dplyr::mutate(time = mdy_hms(creationdate, tz = "UTC")) %>%
  dplyr::mutate(local.time = with_tz(time, tz = "Australia/Perth")) %>%
  dplyr::select(system.number, sample, depth, video.notes, latitude, longitude, time, local.time) %>%
  dplyr::mutate(date = substr(.$local.time, 1, 10)) %>%
  left_join(bruv.cameras) %>%
  add_column(!!!metadata.names[!names(metadata.names) %in% names(.)]) %>%
  dplyr::select(system.number, sample, latitude, longitude, date, time, local.time, site, location, status, depth, observer, successful.count, successful.length, video.notes, left.camera, right.camera, rear.camera) %>%
  dplyr::mutate(local.time = as.character(local.time)) %>%
  glimpse()

# add to labsheet on google drive
# url <- "https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=830769099"
# sheet_append(url, bruv.metadata, sheet = "2022-11_Esperance_stereo-BRUVs")
