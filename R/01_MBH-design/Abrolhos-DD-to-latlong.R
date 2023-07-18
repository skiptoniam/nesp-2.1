library(tidyverse)
library(measurements)

dat <- read.csv("data/mbh-design/2023-08_Abrolhos_stereo-BRUVs.csv") %>%
  dplyr::mutate(Latitude.dec = conv_unit(Latitude, from = "dec_deg", to = "deg_dec_min"),
                Longitude.dec = conv_unit(Longitude, from = "dec_deg", to = "deg_dec_min")) %>%
  glimpse()

write.csv(dat, file = "data/mbh-design/2023-08_Abrolhos_stereo-BRUVs-decmin.csv",
          row.names = F)