
# DATA PREPARATION--------------------------------------------------------------

rm(list = ls())


if (!require('openxlsx')) install.packages("openxlsx")
if (!require('janitor')) install.packages("janitor")
if (!require('dplyr')) install.packages("dplyr")
if (!require('geodata')) install.packages("geodata")
if (!require('terra')) install.packages("terra")
if (!require('ggspatial')) install.packages("ggspatial")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('tidyterra')) install.packages("tidyterra")
if (!require('data.table')) install.packages("data.table")
if (!require('tidyr')) install.packages("tidyr")
if (!require('gtsummary')) install.packages("gtsummary")


library(openxlsx)
library(janitor)
library(dplyr)
library(geodata)
library(terra)
library(ggspatial)
library(ggplot2)
library(tidyterra)
library(data.table)
library(tidyr)
library(gtsummary)


data = read.xlsx("BASELINE DATA.xlsx")


## Data cleaning

data = clean_names(data)


data$district = ifelse(is.na(data$district), data$district_2, data$district)
data$ward = ifelse(is.na(data$ward), data$ward_2, data$ward)

data = data[, -c(3, 6:7)]


data$ward = ifelse(data$ward == "Ward 3", "Ward 3 9 12 17", data$ward)

table(data$ward)


# names(data)[c(7:10, 54:62, 68:85, 95:112)]
names(data)[c(7:10, 54:62, 68:85, 95:112)] = sub(".*_", "", names(data)[c(7:10, 54:62, 68:85, 95:112)])
names(data)[c(7:10, 54:62, 68:85, 95:112)]


names(data)[54] = "common_bean"
names(data)[59] = "velvet_bean"
names(data)[69] = "common_bean"
names(data)[70] = "sweet_potato"
names(data)[82] = "sugar_cane"
names(data)[96] = "common_bean"
names(data)[97] = "sweet_potato"
names(data)[109] = "sugar_cane"


names(data)[c(33:49, 65, 92, 173, 177, 187:188, 210, 243)] = c("cropped_area", "fallow", "maize", "sorghum", "pearl_millet", "finger_millet", "sesame", "groundnut", "tobacco", "cotton", "potato", "sweet_potato", "sunflower", "soybean", "common_bean", "cowpea", "roundnut", "ind_garden", "com_garden", "donkeys", "pigs", "bee_hives", "fish_pond", "main_food_source", "main_income_source")


# str(data[c(65, 92, 154:168, 188, 192:208, 225:241)])
data[c(65, 92, 154:168, 188, 192:208, 225:241)] = lapply(data[c(65, 92, 154:168, 187, 192:208, 225:241)], function(x) ifelse(x == "Yes", 1, 0))
# str(data[c(65, 92, 154:168, 188, 192:208, 225:241)])


data$bee_hives = ifelse(data$bee_hives > 0, 1, 0)

table(data$bee_hives)


## Calculation of new variables

data$elderly = data$total_number_of_adult_males_aged_61_or_more + data$total_number_of_adult_females_aged_61_or_more
data$adult = data$total_number_of_adult_males_aged_25_60 + data$total_number_of_adult_females_aged_25_60
data$youth = data$total_number_of_young_males_aged_15_25 + data$total_number_of_young_females_aged_15_25

data$family_size = data$elderly + data$adult + data$youth + data$total_number_of_children_and_teens_aged_3_14 + data$total_number_of_infants_of_age_0_to_2


data$eq_value = 10000 * data$nb_of_tractors + 2000 * data$nb_of_two_wheel_tractors + 100 * data$nb_of_ploughs + 150 * data$nb_of_cultivators + 500 * data$nb_of_scotchcarts + 50 * data$nb_of_wheelbarows + 50 * data$nb_of_solar_panels + 300 * data$nb_of_motorized_grinding_mill

summary(data$eq_value)


data$small_grains_area = data$sorghum + data$pearl_millet + data$finger_millet

data$legumes = data$groundnut + data$soybean + data$common_bean + data$cowpea + data$roundnut

data$other = data$sesame + data$tobacco + data$cotton + data$potato + data$sweet_potato + data$sunflower


data$other = ifelse(data$other > data$cropped_area - data$maize - data$small_grains_area - data$legumes, data$other, data$cropped_area - data$maize - data$small_grains_area - data$legumes)


data$prop_fallow = data$fallow * 100/ (data$cropped_area + data$fallow + 0.001)

summary(data$prop_fallow)


data$prop_non_cereals = (data$legumes + data$other) * 100 / (data$maize + data$small_grains_area + data$legumes + data$other+ 0.001)

summary(data$prop_non_cereals)


data[c(123, 136)][is.na(data[c(123, 136)])] = 0

data$cereal_prod = data$total_maize_production_in_the_season_2021_22_kg +
  data$total_sorghum_production_in_the_season_2021_22_kg

summary(data$cereal_prod)


data[c(88:91, 119:123, 132:153)][is.na(data[c(88:91, 119:123, 132:153)])] = 0

data$fertilizers = data$quantity_kg_of_basal_fertilizer_applied_to_cotton_in_the_season_2021_22 + data$quantity_kg_of_basal_fertilizer_applied_to_groundnut_in_the_season_2021_22 + data$quantity_kg_of_basal_fertilizer_applied_to_maize_in_the_season_2021_22 + data$quantity_kg_of_basal_fertilizer_applied_to_sesame_in_the_season_2021_22 + data$quantity_kg_of_basal_fertilizer_applied_to_sorghum_in_the_season_2021_22 + data$quantity_kg_of_basal_fertilizer_applied_to_the_garden_in_the_last_12_months + data$quantity_kg_of_topdressing_fertilizer_applied_to_cotton_in_the_season_2021_22 + data$quantity_kg_of_topdressing_fertilizer_applied_to_groundnut_in_the_season_2021_22 + data$quantity_kg_of_topdressing_fertilizer_applied_to_maize_in_the_season_2021_22 + data$quantity_kg_of_topdressing_fertilizer_applied_to_sesame_in_the_season_2021_22 + data$quantity_kg_of_topdressing_fertilizer_applied_to_sorghum_in_the_season_2021_22 + data$quantity_kg_of_topdressing_fertilizer_applied_to_the_garden_in_the_last_12_months

data$manure = data$quantity_kg_of_manure_applied_to_cotton_in_the_season_2021_22 + data$quantity_kg_of_manure_applied_to_groundnut_in_the_season_2021_22 + data$quantity_kg_of_manure_applied_to_maize_in_the_season_2021_22 + data$quantity_kg_of_manure_applied_to_sesame_in_the_season_2021_22 + data$quantity_kg_of_manure_applied_to_sorghum_in_the_season_2021_22 + data$quantity_kg_of_manure_applied_to_the_garden_in_the_last_12_months

data$compost = data$quantity_kg_of_compost_applied_to_cotton_in_the_season_2021_22 + data$quantity_kg_of_compost_applied_to_groundnut_in_the_season_2021_22 + data$quantity_kg_of_compost_applied_to_maize_in_the_season_2021_22 + data$quantity_kg_of_compost_applied_to_sesame_in_the_season_2021_22 + data$quantity_kg_of_compost_applied_to_sorghum_in_the_season_2021_22 + data$quantity_kg_of_compost_applied_to_the_garden_in_the_last_12_months

data$amendment = data$manure + data$compost


data$cattle = data$how_many_adult_indigenous_cattle_do_you_currently_own + data$how_many_improved_beef_cattle_do_you_currently_own + data$how_many_improved_dairy_cattle_do_you_currently_own + data$how_many_juvenile_cattle_3_yr_old_indigenous_and_improved_do_you_currently_own

data$goats = data$how_many_indigenous_goats_do_you_currently_own + data$how_many_improved_goats_do_you_currently_own

data$small_rum = data$goats + data$how_many_sheep_do_you_currently_own

data$poultry = data$how_many_indigenous_chickens_do_you_currently_own + data$how_many_layer_chickens_do_you_currently_own + data$how_many_broiler_chickens_do_you_currently_own + data$how_many_guinea_fowls_do_you_currently_own + data$how_many_turkeys_do_you_currently_own + data$how_many_ducks_do_you_currently_own + data$how_many_geese_do_you_currently_own


data$TLU = 0.7 * data$cattle + 0.5 * data$donkeys + 0.1 * data$small_rum + 0.2 * data$pigs + 0.01 * data$poultry

summary(data$TLU)


data[c(189:191)][is.na(data[c(189:191)])] = 0

data$offtake = 0.7 * data$number_of_cattle_sold_or_slaughtered_in_the_past_12_months + 0.1 * data$number_of_goats_sold_or_slaughtered_in_the_past_12_months + 0.01 * data$number_of_indigenous_chicken_sold_or_slaughtered_in_the_past_12_months

summary(data$TLU)


data$vegetables = data$vitamin_a_rich_vegetables_and_tubers + data$dark_green_leafy_vegetables + data$other_vegetables
data$vegetables = ifelse(data$vegetables > 0, 1, 0)

data$fruits = data$vitamin_a_rich_fruits + data$other_fruits
data$fruits = ifelse(data$fruits > 0, 1, 0)

data$meat = data$organ_meat + data$flesh_meat
data$meat = ifelse(data$meat > 0, 1, 0)

data$hdds = data$cereals + data$white_roots_and_tubers + data$vegetables + data$fruits + data$meat + data$eggs + data$fish_and_seafood + data$legumes_nuts_and_seeds + data$milk_and_milk_products + data$oils_and_fats + data$sweets + data$spices_condiments_beverages


data[which(data$hdds == 0), ] =  NA

summary(data$hdds)


data$food_security = rowMeans(data[212:223] != "Low")

summary(data$food_security)


data$cowpea.w = ifelse(data$list_other_main_crops_you_grew_during_the_season_2021_22_separated_by_commas == "Cowpeas" | data$list_other_main_crops_you_grew_during_the_season_2021_22_separated_by_commas == "Cowpeas, lablab", 1, 0)

data$lablab.w = ifelse(data$list_other_main_crops_you_grew_during_the_season_2021_22_separated_by_commas == "Cowpeas, lablab", 1, 0)

data$runinga.w = ifelse(data$list_other_main_crops_you_grew_during_the_season_2021_22_separated_by_commas == "Runinga, rossella", 1, 0)

data$rosella.w = ifelse(data$list_other_main_crops_you_grew_during_the_season_2021_22_separated_by_commas == "rosella" | data$list_other_main_crops_you_grew_during_the_season_2021_22_separated_by_commas == "Rosella" | data$list_other_main_crops_you_grew_during_the_season_2021_22_separated_by_commas == "rosella " | data$list_other_main_crops_you_grew_during_the_season_2021_22_separated_by_commas == "Rosella " | data$list_other_main_crops_you_grew_during_the_season_2021_22_separated_by_commas == "Rossella" | data$list_other_main_crops_you_grew_during_the_season_2021_22_separated_by_commas == "Rotella" | data$list_other_main_crops_you_grew_during_the_season_2021_22_separated_by_commas == "Runinga, rossella", 1, 0)


data$maize.x = ifelse(data$list_other_intercrops_separated_by_commas == "Maize,rosella", 1, 0)

data$rosella.x = ifelse(data$list_other_intercrops_separated_by_commas == "Maize,rosella", 1, 0)

data$sweet_sorghum.x = ifelse(data$list_other_intercrops_separated_by_commas == "Sweetsoughum,milet", 1, 0)

data$pearl_millet.x = ifelse(data$list_other_intercrops_separated_by_commas == "Sweetsoughum,milet", 1, 0)


data$chilli.y = ifelse(data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "Chill,potato", 1, 0)

data$potato.y = ifelse(data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "Chill,potato", 1, 0)

data$katarina.y = ifelse(data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "Katarina" | data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "Sugarloaf, katarina", 1, 0)

data$muboora.y = ifelse(data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "Muboora", 1, 0)

data$shoeshine.y = ifelse(data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "Shoeshine", 1, 0)

data$sugarloaf.y = ifelse(data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "sugarloaf" | data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "Sugarloaf" | data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "sugarloaf " | data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "Sugarloaf " | data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "Sugarloaf, katarina" | data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "Sugarloaf, tsunga" | data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "Tsunga,sugarloaf", 1, 0)

data$tsunga.y = ifelse(data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "Sugarloaf, tsunga" | data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "Tsunga" |
                         data$list_other_crops_you_grew_in_your_individual_garden_separated_by_commas == "Tsunga,sugarloaf", 1, 0)


data[c(35:49, 54:62, 68:85, 95:112, 280:294)][is.na(data[c(35:49, 54:62, 68:85, 95:112, 280:294)])] = 0

base_names = sub("\\..*", "", names(data)[c(35:49, 54:62, 68:85, 95:112, 280:294)])

collapsed = data[c(35:49, 54:62, 68:85, 95:112, 280:294)] %>%
  mutate(row_id = row_number()) %>%
  group_by(row_id) %>%
  summarise(across(everything(), ~ .x, .names = "{.col}")) %>%
  select(-row_id)

collapsed = as.data.frame(lapply(split.default(data[c(35:49, 54:62, 68:85, 95:112, 280:294)], base_names), function(df) rowSums(df > 0)))

data$crop_diversity = rowSums(collapsed > 0)

summary(data$crop_diversity)


data[c(269:270, 272, 173, 177, 181:188)][is.na(data[c(269:270, 272, 173, 177, 181:188)])] = 0

data$livestock_diversity = rowSums(data[c(269:270, 272, 173, 177, 181:188)] > 0)

summary(data$livestock_diversity)


data$tot_div = data$crop_diversity + data$livestock_diversity

summary(data$tot_div)


small_dataset = data[, c(253, 4, 3, 11:13, 257, 33:34, 263, 265, 268:269, 271:272, 187, 65, 92, 210, 243, 279, 278, 155, 157, 159:164, 167:168, 202:205, 297, 264, 274, 258)]

saveRDS(small_dataset, "small_dataset.rds")


# PRELIMINARY ANALYSIS----------------------------------------------------------

## Map of surveyed farms

zwe3 = gadm(country = "ZWE", level = 3, path = "Geodata")
plot(zwe3)


mbire = zwe3[zwe3$NAME_2 == "Mbire", ]
plot(mbire)


elevation = elevation_3s(lon = (30.04626 + 31.14537)/2, lat = (-16.41686 + -15.61071)/2, path = "Geodata")

names(elevation) = "elevation"

plot(elevation)


elevation = project(elevation, "EPSG:4326")

elevation = crop(elevation, ext(30.2, 30.65, -16.23, -15.97))

plot(elevation)


data_vect = vect(data, geom = c("longitude", "latitude"), crs = "EPSG:4326")


wards = mbire[mbire$NAME_3 == "Ward 2" | mbire$NAME_3 == "Ward 3" | mbire$NAME_3 == "Ward 9" | mbire$NAME_3 == "Ward 12" | mbire$NAME_3 == "Ward 17", ]

plot(wards)


ward_centroids = centroids(wards)

ward_centroids = data.frame(
  x    = crds(ward_centroids)[,1],
  y    = crds(ward_centroids)[,2],
  ward = wards$NAME_3 )

ward_centroids$y[ward_centroids$ward == "Ward 2"] =
  ward_centroids$y[ward_centroids$ward == "Ward 2"] - 0.05

ward_centroids$x[ward_centroids$ward == "Ward 2"] =
  ward_centroids$x[ward_centroids$ward == "Ward 2"] + 0.05


mozambique = data.frame(x = 30.55, y = -15.99, label = "MOZAMBIQUE")


elevation_df = as.data.frame(elevation, xy = TRUE)

ggplot() + theme_bw() +
  geom_raster(data = na.omit(elevation_df), aes(x = x, y = y, fill = elevation)) +
  geom_spatvector(data = mbire, fill = NA, linewidth = 1, color = "grey30") +
  geom_spatvector(data = data_vect, shape = 16, color = "#781C6DFF", alpha = 0.5, size = 3) +
  geom_text(data = ward_centroids, aes(x = x, y = y, label = ward), color = "black", size = 4, fontface = "bold") +
  geom_text(data = mozambique, aes(x = x, y = y, label = label),
            color = "black", size = 5, fontface = "italic") +
  scale_fill_distiller(palette = "RdYlGn", limits = c(350, 650), breaks = seq(350, 650, by = 75)) +
  scale_y_continuous(breaks = seq(-16.22, -15.97, by = 0.1)) +
  coord_sf(xlim = c(30.2 , 30.65), ylim = c(-15.97, -16.23), expand = FALSE) +
  labs(fill = "Elevation (m.a.s.l.)") +
  theme(plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"), pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"), style = north_arrow_fancy_orienteering(text_size = 10))


## Stacked bar chart

land_mgt = data[, c(115:118)]

names(land_mgt) = c("Soil fertility", "Soil erosion", "Soil compaction", "Tree cover")

land_mgt = na.omit(land_mgt)


land_mgt = gather(land_mgt, Characteristic, Status, "Soil fertility":"Tree cover")

land_mgt = land_mgt %>%
  group_by(Characteristic, Status) %>%
  summarise(n = n()) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  select(-n) %>%
  ungroup()

land_mgt$Status = factor(land_mgt$Status, levels = c("Improving", "Stable", "Deteriorating"))


ggplot(data = land_mgt, aes(x = Characteristic , y = Percentage, fill = Status)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  coord_flip() + theme_bw() +
  scale_fill_manual(values = c("#FCFFA4FF", "#ED6925FF", "#781C6DFF")) +
  theme(axis.title.x = element_text(size = 14, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12))


## More complex stacked bar chart

crop = data[, c(35, 259:261)]

crop$legumes = ifelse(crop$legumes > 5, 0, crop$legumes)

crop$cropped_area = crop$maize + crop$small_grains_area + crop$legumes + crop$other

crop = crop[order(-crop$cropped_area),]

crop$ID = 1:nrow(crop)


crop$maize = frollmean(crop$maize, 10)
crop$small_grains_area = frollmean(crop$small_grains_area, 10)
crop$legumes = frollmean(crop$legumes, 10)
crop$other = frollmean(crop$other, 10)


crop = gather(crop, crop, area, "maize":"other")

crop$crop = factor(crop$crop, levels = c("maize", "small_grains_area", "legumes", "other"))


crop %>%
  ggplot(aes(x = ID, y = area, fill = as.factor(crop))) +
  geom_bar(stat = 'identity', width = 1, alpha = 0.9,
           position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("#FCA50AFF", "#DD513AFF", "#932667FF", "#420A68FF"),
                    labels = c("Maize", "Small grains", "Legumes", "Other crops")) +
  xlab("Farms") + ylab("Area (ha)") +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12,  face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.ticks.x = element_blank(),
        legend.background = element_rect(color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = c(0.9, 0.9), legend.justification = c(0.9, 0.9))


## Lollipop diagram

inter = data[, c(54:62, 284:287)]

names(inter) = c("Common bean", "Cowpea", "Groundnut", "Lablab", "Pigeonpea", "Velvet bean", "Pumpkin", "Watermelon", "Local cucumber", "Maize", "Rosella", "Sweet sorghum", "Pearl millet")


inter = gather(inter, intercrop, presence, "Common bean":"Pearl millet")

inter = aggregate(presence ~ intercrop, FUN = sum, na.rm = TRUE, data = inter)

inter = subset(inter, presence > 0)


inter %>%
  arrange(presence) %>%
  mutate(order = factor(intercrop, intercrop)) %>%
  ggplot( aes(x = intercrop, y = presence)) +
  geom_segment( aes(x = order, xend = intercrop, y = 0, yend = presence), color = "#ED6925FF", linewidth = 1) +
  geom_point(color="#ED6925FF", size = 8, alpha = 0.5) +
  xlab("Intercrop species") + ylab("Nb of farms") +
  theme_bw() +
  coord_flip() +
  theme(axis.title.x = element_text(size = 14, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))


## Bubble chart

fsec = data[, c(212:223)]

names(fsec) = c("Nov 2022", "Oct 2022", "Sep 2022", "Aug 2022", "Jul 2022", "Jun 2022", "May 2022", "Apr 2022", "Mar 2022", "Feb 2022", "Jan 2022", "Dec 2021")

fsec = na.omit(fsec)


fsec = gather(fsec, month, status, "Nov 2022":"Dec 2021")

fsec$status = factor(fsec$status, levels = c("Low", "Medium", "High"))

fsec$month = factor(fsec$month, levels = c("Dec 2021", "Jan 2022", "Feb 2022", "Mar 2022", "Apr 2022", "May 2022", "Jun 2022", "Jul 2022", "Aug 2022", "Sep 2022", "Oct 2022", "Nov 2022"))


ggplot(data = fsec) +
  geom_count(mapping = aes(x = month, y = status), color = "#ED6925FF", alpha = 0.5) +
  scale_size_area(max_size = 10) +
  theme_bw() + xlab("") + ylab("Food security status") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 12, face = "bold", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12))


## Pie chart

maiz_res = data[, c(125:131)]

names(maiz_res) = c("Burnt", "Retained in the field", "Grazed in the field", "Harvested for feed", "Harvested for compost", "Harvested for fuel", "Harvested for fencing")


maiz_res = gather(maiz_res, use, perc, "Burnt":"Harvested for fencing")

maiz_res$perc = as.numeric(sub("%", "", maiz_res$perc))

maiz_res = aggregate(perc ~ use, FUN = mean, na.rm = TRUE, data = maiz_res)

maiz_res = maiz_res[maiz_res$perc > 0,]


ggplot(maiz_res, aes(x = "", y = perc, fill = use)) +
  geom_bar(alpha = 0.5, width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_viridis_d(option = "B", name = "Maize residue use") +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = "right")


## Summary table

small_dataset[, -c(1, 3)] %>%
  tbl_summary(
    by = ward,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    digits = all_continuous() ~ 1)%>%
  add_overall()


small_dataset[, -c(1, 3)] %>%
  tbl_summary(
    by = ward,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    type = list(hdds ~ "continuous"),
    digits = all_continuous() ~ 1)%>%
  add_overall()


small_dataset[, -c(1, 3)] %>%
  tbl_summary(
    by = ward,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    type = list(hdds ~ "continuous"),
    digits = all_continuous() ~ 1,
    label = list(how_old_is_the_head_of_the_household ~  "Age of the head of the household",
                 what_is_the_sex_of_the_head_of_the_household ~  "Female-headed households",
                 what_is_the_education_level_of_the_head_of_the_household ~  "Education of the head of the household higher than primary",
                 what_is_the_education_level_of_the_head_of_the_household ~  "Family size",
                 eq_value ~  "Equipment value (USD)",
                 cropped_area ~  "Total cropped area (ha)",
                 fallow ~ "Fallow land (ha)",
                 prop_non_cereals ~  "Non-cereal crops (% total cropped area)",
                 fertilizers ~  "Total fertilizer used (kg)",
                 amendment ~  "Total manure + compost used (kg)",
                 cattle ~  "Cattle (n)",
                 small_rum ~  "Small ruminants (n)",
                 poultry ~  "Poultry (n)",
                 bee_hives ~  "Bee keeping",
                 ind_garden ~  "Owning an individual garden",
                 com_garden ~  "Having access to a communal garden",
                 main_food_source ~  "Own production as main source of food",
                 main_income_source ~  "Farming as main source of income",
                 food_security ~ "Proportion of the year being food secured",
                 hdds ~  "24H household dietary diversity score (0-12)",
                 community_seed_banks ~  "Using a community seed bank",
                 small_grains ~  "Using small grains",
                 crop_rotation ~  "Using crop rotation",
                 intercropping ~  "Using intercropping",
                 cover_crops_for_erosion_control_and_or_soil_fertility ~  "Using cover crops",
                 mulching ~  "Using mulching",
                 integrated_pest_management_i_e_scouting_etc ~  "Using integrated pest management",
                 compost_manure ~  "Using compost and manure",
                 tree_planting_on_farm ~  "Planting trees on-farm",
                 tree_retention_natural_regeneration_on_farm	 ~  "Retaining naturally regenerated trees on-farm",
                 homemade_animal_feeds_made_with_locally_available_ingredients ~  "Using homemade animal feed",
                 fodder_production_for_ruminants_e_g_velvet_bean_lablab ~  "Produccing fodder",
                 fodder_preservation_for_ruminants_e_g_silage_making ~  "Preserving fodder",
                 survival_feeding_feeding_of_productive_livestock_in_lean_season ~  "Using survival feeding",
                 tot_div ~  "Total farm diversity (n species)",
                 cereal_prod ~  "Total cereal production (kg/yr)",
                 offtake ~  "Total livestock offtake (TLU/yr)"))%>%
  add_overall()


small_dataset[, -c(1, 3)] %>%
  tbl_summary(
    by = ward,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    type = list(hdds ~ "continuous"),
    digits = all_continuous() ~ 1,
    label = list(how_old_is_the_head_of_the_household ~  "Age of the head of the household",
                 what_is_the_sex_of_the_head_of_the_household ~  "Female-headed households",
                 what_is_the_education_level_of_the_head_of_the_household ~  "Education of the head of the household higher than primary",
                 what_is_the_education_level_of_the_head_of_the_household ~  "Family size",
                 eq_value ~  "Equipment value (USD)",
                 cropped_area ~  "Total cropped area (ha)",
                 fallow ~ "Fallow land (ha)",
                 prop_non_cereals ~  "Non-cereal crops (% total cropped area)",
                 fertilizers ~  "Total fertilizer used (kg)",
                 amendment ~  "Total manure + compost used (kg)",
                 cattle ~  "Cattle (n)",
                 small_rum ~  "Small ruminants (n)",
                 poultry ~  "Poultry (n)",
                 bee_hives ~  "Bee keeping",
                 ind_garden ~  "Owning an individual garden",
                 com_garden ~  "Having access to a communal garden",
                 main_food_source ~  "Own production as main source of food",
                 main_income_source ~  "Farming as main source of income",
                 food_security ~ "Proportion of the year being food secured",
                 hdds ~  "24H household dietary diversity score (0-12)",
                 community_seed_banks ~  "Using a community seed bank",
                 small_grains ~  "Using small grains",
                 crop_rotation ~  "Using crop rotation",
                 intercropping ~  "Using intercropping",
                 cover_crops_for_erosion_control_and_or_soil_fertility ~  "Using cover crops",
                 mulching ~  "Using mulching",
                 integrated_pest_management_i_e_scouting_etc ~  "Using integrated pest management",
                 compost_manure ~  "Using compost and manure",
                 tree_planting_on_farm ~  "Planting trees on-farm",
                 tree_retention_natural_regeneration_on_farm	 ~  "Retaining naturally regenerated trees on-farm",
                 homemade_animal_feeds_made_with_locally_available_ingredients ~  "Using homemade animal feed",
                 fodder_production_for_ruminants_e_g_velvet_bean_lablab ~  "Produccing fodder",
                 fodder_preservation_for_ruminants_e_g_silage_making ~  "Preserving fodder",
                 survival_feeding_feeding_of_productive_livestock_in_lean_season ~  "Using survival feeding",
                 tot_div ~  "Total farm diversity (n species)",
                 cereal_prod ~  "Total cereal production (kg/yr)",
                 offtake ~  "Total livestock offtake (TLU/yr)"))%>%
  add_p(test = list(all_continuous() ~ "kruskal.test",
                    all_categorical() ~ "chisq.test"))%>%
  add_overall()

