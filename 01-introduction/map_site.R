rm(list = ls())

# Libraries
library(geodata)
library(terra)
library(dplyr)
library(sf)
library(scales)
library(ggplot2)
library(ggfx)
library(tidyterra)


# Data preparation

country = world(resolution = 5, level = 0, path = "Geodata")
elevation = elevation_global(res = 5, path = "Geodata")
names(elevation) = "elevation"

ortho = "+proj=ortho +lat_0=-16.15 +lon_0=30.45 +a=6371000 +b=6371000 +units=m +no_defs"

country_ortho = project(country, ortho)
elevation_ortho = project(elevation, ortho)


# Convert to data frame

elevation_df = as.data.frame(elevation_ortho, xy = TRUE)
names(elevation_df)[3] = "elevation"

# Remove NAs and rescale contrast
elevation_df = elevation_df %>% filter(!is.na(elevation))

# Dynamic stretch based on visible values
lims = quantile(elevation_df$elevation, probs = c(0.01, 0.99), na.rm = TRUE)
elevation_df$elevation_clipped = pmax(pmin(elevation_df$elevation, lims[2]), lims[1])

# Nonlinear scaling to boost contrast in lowlands
elevation_df$elevation_stretched = sqrt(scales::rescale(elevation_df$elevation_clipped, to = c(0, 1)))


# Define color palette (green → tan → white)

terrain_colors = c(
  "#00441B",  # dark green lowlands
  "#1B7837",  # forest
  "#5AAE61",  # midlands
  "#A6DBA0",  # uplands
  "#E0C072",  # dry hills
  "#B97A3E",  # mountain base (deep brown)
  "#8B4513",  # dark rock
  "#B0B0B0",  # light rock
  "#E6E6E6",  # snow edge
  "#FFFFFF"   # snow peaks
)

# Circle (Earth outline) & oceans

circle = st_point(c(0, 0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = ortho)


# Site location

sites = data.frame(decimalLongitude = 30.45, decimalLatitude = -16.15)
sites = vect(sites, geom = c("decimalLongitude", "decimalLatitude"),
              crs = "+proj=longlat +datum=WGS84")
sites_ortho = project(sites, ortho)


# Plot

ggplot() +
  with_blur(
    geom_sf(data = circle, fill = "cornflowerblue", color = NA, size = 2),
    sigma = unit(3, "mm")) +
  geom_sf(data = circle, fill = "cornflowerblue", color = "black", size = 0.3) +
  geom_raster(data = elevation_df, aes(x, y, fill = elevation_stretched)) +
  geom_spatvector(data = country_ortho, fill = NA, color = "black", linewidth = 0.4) +
  geom_spatvector(data = sites_ortho, fill = "white", color = "red3", size = 5, stroke = 1) +
  scale_fill_gradientn(colors = terrain_colors, name = "Elevation (relative)") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black", color = NA),
    plot.margin = margin(10, 10, 10, 10))

