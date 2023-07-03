library(tidyverse)
library(ggplot2)
library(sf)
library(terra)
library(ggnewscale)
library(scales)
library(viridis)
library(patchwork)

e <- ext(114.9, 115.8, -33.7,-33.2)

dat <- st_read("data/mbh-design/National_Sampling_Master_WGS84.shp") %>%
  st_crop(e) %>%
  glimpse()

aus <- st_read("data/spatial/shapefiles/cstauscd_r.mif", crs = 4283) %>%
  dplyr::filter(!FEAT_CODE %in% "sea") %>%
  glimpse()

ausc <- aus %>%
  st_crop(e)

bathy <- rast("data/spatial/rasters/raw bathymetry/bath_250_good.tif") %>%
  crop(e) %>%
  as.data.frame(xy = TRUE)

p1 <- ggplot() +
  geom_raster(data = bathy, aes(x = x, y = y, fill = bath_250_good),
              show.legend = F, alpha = 0.8) +
  scale_fill_viridis() +
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 5) +
  geom_sf(data = dat, shape = 4) +
  annotate(geom = "point", x = 115.6409, y = -33.3270) + # Bunbury
  annotate(geom = "point", x = 115.3473, y = -33.6516) + # Busselton
  annotate(geom = "text", x = c(115.6409 + 0.03, 115.3473 + 0.035), 
           y = c(-33.3270, -33.6516), label = c("Bunbury", "Busselton"),
           fontface = "italic", size = 3) +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim = c(115, 115.7), ylim = c(-33.65, -33.299)) +
  theme_minimal()

inset <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 5) +
  annotate(geom = "rect", xmin = 114, xmax = 116.5, ymin = -34, ymax = -32.5,
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  # annotate(geom = "rect", xmin = 114, xmax = 116.5, ymin = -35, ymax = -32.5,
  #          colour = "grey25", fill = "white", alpha = 1/5, size = 0.2)+       # Added this one for Tim's AMSA sea level film
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70"),
        plot.background = element_rect(fill='transparent', colour = NA),
        panel.background = element_rect(fill = "white", colour = NA)) +
  coord_sf()

p1_inset <- p1 + inset_element(inset, left = 0.72, bottom = 0, right = 1, top = 0.4)
  
png(filename = "plots/geographe-mbh-exerpt.png", height = 4.5, width = 8,
      units = "in", res = 300)
p1_inset
dev.off()

png(filename = "plots/aus-inset.png", height = 4, width = 5,
    res = 300, units = "in")
inset
dev.off()


