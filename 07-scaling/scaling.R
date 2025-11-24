
# DATA PREPARATION--------------------------------------------------------------

rm(list = ls())

if (!require('openxlsx')) install.packages("openxlsx")
if (!require('terra')) install.packages("terra")
if (!require('geodata')) install.packages("geodata")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('ggspatial')) install.packages("ggspatial")
if (!require('tidyterra')) install.packages("tidyterra")

library(openxlsx)
library(terra)
library(geodata)
library(ggplot2)
library(ggspatial)
library(tidyterra)

data = read.xlsx("BASELINE DATA.xlsx")


## Administrative shapefiles

zwe0 = gadm(country = 'ZWE', level = 0, path = "Geodata")

plot(zwe0)


zwe1 = gadm(country = 'ZWE', level = 1, path = "Geodata")

plot(zwe1)


## Cropland mask

cropland = geodata::cropland(source = "WorldCover", year = 2019, path = "Geodata")
cropland = ifel(cropland$cropland > 0, 1, NA)
cropland = crop(cropland, zwe0)
cropland = mask(cropland, zwe0)

plot(zwe0)
plot(cropland, col = "forestgreen", add = T, legend = F)
plot(zwe1, add = T)


# Rasters of predictors

elevation = geodata::elevation_30s(country = 'ZWE', path = "Geodata")
elevation = crop(elevation, cropland)
elevation = mask(elevation, cropland)
names(elevation) = 'elevation'

plot(zwe0)
plot(elevation, add = T, legend = T)
plot(zwe1, add = T)


sand_5 = geodata::soil_af(var = 'sand', depth = 5, path = "Geodata")
sand_15 = geodata::soil_af(var = 'sand', depth = 15, path = "Geodata")
sand_30 = geodata::soil_af(var = 'sand', depth = 30, path = "Geodata")
sand = (sand_5 * 5 + sand_15 * 10 + sand_30 * 15) / (5 + 10 + 15)
sand = terra::crop(sand, cropland)
sand = terra::mask(sand, cropland)
names(sand) = 'sand'

plot(zwe0)
plot(sand, add = T, legend = T)
plot(zwe1, add = T)


tavg = worldclim_country(country = 'ZWE', var = 'tavg', path = "Geodata", res = 0.5)
tavg = mean(tavg)
tavg = terra::crop(tavg, cropland)
tavg = terra::mask(tavg, cropland)
names(tavg) = 'tavg'

plot(zwe0)
plot(tavg, add = T, legend = T)
plot(zwe1, add = T)


prec = worldclim_country(country = 'ZWE', var = 'prec', path = "Geodata", res = 0.5)
prec = sum(prec)
prec = terra::crop(prec, cropland)
prec = terra::mask(prec, cropland)
names(prec) = 'prec'

plot(zwe0)
plot(prec, add = T, legend = T)
plot(zwe1, add = T)


pop = geodata::population(year = 2020, path = "Geodata")
pop = resample(pop, cropland)
pop = crop(pop, cropland)
pop = mask(pop, cropland)
names(pop) = 'pop'

plot(zwe0)
plot(pop, add = T, legend = T)
plot(zwe1, add = T)


travel_time = travel_time(to = "city", size = 6, up = TRUE, path = "Geodata")
travel_time = crop(travel_time, cropland)
travel_time = mask(travel_time, cropland)
names(travel_time) = "travel_time"

plot(zwe0)
plot(travel_time, add = T, legend = T)
plot(zwe1, add = T)


## Preprocessing

all = c(elevation, sand, tavg, prec, pop, travel_time)

means = as.numeric(global(all, "mean", na.rm = TRUE)[,1])
sds = as.numeric(global(all, "sd", na.rm = TRUE)[,1])
all_z = (all - means) / sds
names(all_z) = paste0(names(all), "_n")


data_vect = vect(data, geom = c("_Record.your.current.location_longitude", "_Record.your.current.location_latitude"), crs = "EPSG:4326")

vals = terra::extract(all_z, data_vect)

mean_site = colMeans(vals[, -1], na.rm = TRUE)

mean_site_df = data.frame(
  variable = names(all_z),
  mean_value = mean_site)

mean_site_df


# TRANSFERABILITY POTENTIAL-----------------------------------------------------

## Distance to project archetype

dist_to_site = sqrt(sum((all_z - mean_site)^2))

centroid = colMeans(crds(data_vect))

plot(dist_to_site, main = "Euclidean distance to project archetype")
plot(zwe1, add = T)
points(centroid["x"], centroid["y"], pch = 21, bg = 'orangered', cex = 2)


## Transferability potential

tp_site = 1 / (1 + dist_to_site)
names(tp_site) = "tp"

plot(tp_site, main = "Transferability potential")
plot(zwe1, add = T)
points(centroid["x"], centroid["y"], pch = 21, bg = 'orangered', cex = 2)


tp_site_q = classify(tp_site$tp, c(
  quantile(values(tp_site$tp, na.rm=T))[1],
  quantile(values(tp_site$tp, na.rm=T))[2],
  quantile(values(tp_site$tp, na.rm=T))[3],
  quantile(values(tp_site$tp, na.rm=T))[4],
  quantile(values(tp_site$tp, na.rm=T))[5]))


plot(tp_site_q, type = 'classes', levels = c('Low', 'Moderate', 'High', 'Very high'), main ='Transferability potential')
plot(zwe1, add = T)
points(centroid["x"], centroid["y"], pch = 21, bg = 'orangered', cex = 2)


## Publication-ready map

tp_site_q_df = as.data.frame(tp_site_q, xy = TRUE)

names(tp_site_q_df)[3] = "class_id"

tp_site_q_df$class_id_num = as.numeric(tp_site_q_df$class_id)

tp_site_q_df$class_label = factor(
  tp_site_q_df$class_id_num,
  levels = 1:4,
  labels = c("Low", "Moderate", "High", "Very high"),
  ordered = TRUE)

centroid_df = data.frame(x = centroid["x"], y = centroid["y"])

ggplot() + theme_bw() +
  geom_raster(data = na.omit(tp_site_q_df), aes(x = x, y = y, fill = class_label)) +
  geom_spatvector(data = zwe1, fill = NA, linewidth = 1, color = "black") +
  geom_point(data = centroid_df, aes(x = x, y = y), color = "black", fill = "tomato", shape = 21, size = 5, stroke = 2) +
  scale_fill_manual(values = c("Low" = "#420A68FF", "Moderate" = "#932667FF", "High" = "#DD513AFF", "Very high" = "#FCA50AFF")) +
  labs(fill = "Transferability") +
  theme(plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = ggplot2::margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"), pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"), style = north_arrow_fancy_orienteering(text_size = 10))

