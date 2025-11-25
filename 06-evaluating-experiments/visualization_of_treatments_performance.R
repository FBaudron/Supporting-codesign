
# # VISUALIZATION OF TREATMENTSNPERFORMANCE-------------------------------------

rm(list = ls())

if (!require('openxlsx')) install.packages("openxlsx")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('ggdist')) install.packages("ggdist")
if (!require('ggthemes')) install.packages("ggthemes")
if (!require('egg')) install.packages("egg")
if (!require('ggradar')) install.packages("ggradar")

library(openxlsx)
library(ggplot2)
library(ggdist)
library(ggthemes)
library(egg)
library(ggradar)

data = read.xlsx("DATA FOR SHINY.xlsx", sheet = 1)

data$Treatment = factor(data$Treatment, levels = c("CONV", "CA", "PPULL"))

data_2022_23 = subset(data, Season == "2022/23")
data_2023_24 = subset(data, Season == "2023/24")


## Pest pressure in different treatments

cri = data_2022_23[, c(1:3, 4:5)]
names(cri)[4] = "Incidence"
names(cri)[5] = "Severity"

faw = data_2022_23[, c(1:3, 6:7)]
names(faw)[4] = "Incidence"
names(faw)[5] = "Severity"

stm = data_2022_23[, c(1:3, 8:9)]
names(stm)[4] = "Incidence"
names(stm)[5] = "Severity"


p1 = ggplot(cri, aes(x = Treatment, y = Incidence, fill = Treatment)) +
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.5, color = "black") +
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.5) +
  geom_point(shape = 21, size = 2, alpha = 0.5, position = position_jitter(seed = 1, width = .1)) +
  scale_fill_manual(values = c("#FCA50AFF", "#DD513AFF", "#932667FF")) +
  ylab("") + xlab("") +
  theme_few() +
  ggtitle("Armored cricket - Incidence (%)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", margin = ggplot2::margin(0, 0, 10, 0)),
        axis.text = element_text(size = 10),
        axis.title = element_blank(),
        legend.title = element_blank()) +
  coord_flip()

p2 = ggplot(faw, aes(x = Treatment, y = Incidence, fill = Treatment)) +
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.5, color = "black") +
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.5) +
  geom_point(shape = 21, size = 2, alpha = 0.5, position = position_jitter(seed = 1, width = .1)) +
  scale_fill_manual(values = c("#FCA50AFF", "#DD513AFF", "#932667FF")) +
  ylab("") + xlab("") +
  theme_few() +
  ggtitle("Fall armyworm - Incidence (%)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", margin = ggplot2::margin(0, 0, 10, 0)),
        axis.text = element_text(size = 10),
        axis.title = element_blank(),
        legend.title = element_blank()) +
  coord_flip()

p3 = ggplot(stm, aes(x = Treatment, y = Incidence, fill = Treatment)) +
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.5, color = "black") +
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.5) +
  geom_point(shape = 21, size = 2, alpha = 0.5, position = position_jitter(seed = 1, width = .1)) +
  scale_fill_manual(values = c("#FCA50AFF", "#DD513AFF", "#932667FF")) +
  ylab("") + xlab("") +
  theme_few() +
  ggtitle("Stemborer - Incidence (%)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", margin = ggplot2::margin(0, 0, 10, 0)),
        axis.text = element_text(size = 10),
        axis.title = element_blank(),
        legend.title = element_blank()) +
  coord_flip()

p4 = ggplot(cri, aes(x = Treatment, y = Severity, fill = Treatment)) +
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.5, color = "black") +
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.5) +
  geom_point(shape = 21, size = 2, alpha = 0.5, position = position_jitter(seed = 1, width = .1)) +
  scale_fill_manual(values = c("#FCA50AFF", "#DD513AFF", "#932667FF")) +
  ylab("") + xlab("") +
  theme_few() +
  ggtitle("Armored cricket - Severity (1-9)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", margin = ggplot2::margin(0, 0, 10, 0)),
        axis.text = element_text(size = 10),
        axis.title = element_blank(),
        legend.title = element_blank()) +
  coord_flip()

p5 = ggplot(faw, aes(x = Treatment, y = Severity, fill = Treatment)) +
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.5, color = "black") +
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.5) +
  geom_point(shape = 21, size = 2, alpha = 0.5, position = position_jitter(seed = 1, width = .1)) +
  scale_fill_manual(values = c("#FCA50AFF", "#DD513AFF", "#932667FF")) +
  ylab("") + xlab("") +
  theme_few() +
  ggtitle("Fall armyworm - Severity (1-9)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", margin = ggplot2::margin(0, 0, 10, 0)),
        axis.text = element_text(size = 10),
        axis.title = element_blank(),
        legend.title = element_blank()) +
  coord_flip()

p6 = ggplot(stm, aes(x = Treatment, y = Severity, fill = Treatment)) +
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.5, color = "black") +
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.5) +
  geom_point(shape = 21, size = 2, alpha = 0.5, position = position_jitter(seed = 1, width = .1)) +
  scale_fill_manual(values = c("#FCA50AFF", "#DD513AFF", "#932667FF")) +
  ylab("") + xlab("") +
  theme_few() +
  ggtitle("Stemborer - Severity (1-9)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", margin = ggplot2::margin(0, 0, 10, 0)),
        axis.text = element_text(size = 10),
        axis.title = element_blank(),
        legend.title = element_blank()) +
  coord_flip()

ggarrange(p1, p4, p2, p5, p3, p6,
                 ncol = 2, nrow = 3, widths = c(1, 1), heights = c(1, 1, 1))


## Productivity of different treatments

yldcrl = data_2022_23[, c(1:3, 10:11)]
yldcrl = unique(yldcrl)

p7 = ggplot(yldcrl, aes(x = Treatment, y = YLD, fill = Treatment)) +
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.5, color = "black") +
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.5) +
  geom_point(shape = 21, size = 2, alpha = 0.5, position = position_jitter(seed = 1, width = .1)) +
  scale_fill_manual(values = c("#FCA50AFF", "#DD513AFF", "#932667FF")) +
  ylab("") + xlab("") +
  theme_few() +
  ggtitle("Cereal yield - Grain (t/ha)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", margin = ggplot2::margin(0, 0, 10, 0)),
        axis.text = element_text(size = 10),
        axis.title = element_blank(),
        legend.title = element_blank()) +
  coord_flip()

p8 = ggplot(yldcrl, aes(x = Treatment, y = BIO, fill = Treatment)) +
  ggdist::stat_halfeye(adjust = 0.6, width = 0.4, .width = 0, justification = -.4, point_colour = NA, alpha = 0.5, color = "black") +
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.5) +
  geom_point(shape = 21, size = 2, alpha = 0.5, position = position_jitter(seed = 1, width = .1)) +
  scale_fill_manual(values = c("#FCA50AFF", "#DD513AFF", "#932667FF")) +
  ylab("") + xlab("") +
  theme_few() +
  ggtitle("Cereal yield - Biomass (t/ha)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", margin = ggplot2::margin(0, 0, 10, 0)),
        axis.text = element_text(size = 10),
        axis.title = element_blank(),
        legend.title = element_blank()) +
  coord_flip()

ggarrange(p7, p8,
                 ncol = 2, nrow = 1, widths = c(1, 1), heights = c(1))


## Rating of treatments by different demographic groups

of = data_2022_23[, c(1:2, 12:18)]
of$Group = rep("Older females", nrow(of))
of = unique(of)

om = data_2022_23[, c(1:2, 19:25)]
om$Group = rep("Older males", nrow(om))
om = unique(om)

yf = data_2022_23[, c(1:2, 26:32)]
yf$Group = rep("Young females", nrow(yf))
yf = unique(yf)

ym = data_2022_23[, c(1:2, 33:39)]
ym$Group = rep("Young males", nrow(ym))
ym = unique(ym)

names(of)[c(3:9)] = c("Grain", "Biomass", "Inputs", "Labour", "Pests", "Drought",
                      "Soil")

names(om)[c(3:9)] = c("Grain", "Biomass", "Inputs", "Labour", "Pests", "Drought",
                      "Soil")

names(yf)[c(3:9)] = c("Grain", "Biomass", "Inputs", "Labour", "Pests", "Drought",
                      "Soil")

names(ym)[c(3:9)] = c("Grain", "Biomass", "Inputs", "Labour", "Pests", "Drought",
                      "Soil")

rat = rbind(of, om, yf, ym)

conv = subset(rat, Treatment == "CONV")
ca = subset(rat, Treatment == "CA")
ppull = subset(rat, Treatment == "PPULL")

p9 = ggradar(
  conv[, c(10, 3:9)],
  plot.title = "Conventional Practice",
  values.radar = c("0", "5", "10"),
  grid.min = 0, grid.mid = 5, grid.max = 10,
  group.line.width = 1,
  group.point.size = 3,
  group.colours = c("#FCA50AFF", "#DD513AFF", "#932667FF", "#420A68FF"),
  fill = TRUE, fill.alpha = 0.2,
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "none") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

p10 = ggradar(
  ca[, c(10, 3:9)],
  plot.title = "Conservation Agriculture",
  values.radar = c("0", "5", "10"),
  grid.min = 0, grid.mid = 5, grid.max = 10,
  group.line.width = 1,
  group.point.size = 3,
  group.colours = c("#FCA50AFF", "#DD513AFF", "#932667FF", "#420A68FF"),
  fill = TRUE, fill.alpha = 0.2,
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))


p11 = ggradar(
  ppull[, c(10, 3:9)],
  plot.title = "Push-Pull",
  values.radar = c("0", "5", "10"),
  grid.min = 0, grid.mid = 5, grid.max = 10,
  group.line.width = 1,
  group.point.size = 3,
  group.colours = c("#FCA50AFF", "#DD513AFF", "#932667FF", "#420A68FF"),
  fill = TRUE, fill.alpha = 0.2,
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "none") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))


ggarrange(p9, p10, p11,
                 ncol = 3, nrow = 1, widths = c(1, 1, 1), heights=c(1))


