# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 11 : Circular
# Last updated 2022-03-28

# Load packages ----

library(patchwork)
library(showtext)
library(tidyverse)

# Load fonts ----

font_add_google("Fredericka the Great", "Fredericka")
showtext_auto()


# Data wrangling ----

layer_1 <- tibble(
  layer = 1,
  x.min = seq(0, 63, 16),
  x.max = seq(16, 64, 16),
  y.min = 0,
  y.max = 1,
  base = c("U", "C", "A", "G"))

layer_2 <- tibble(
  layer = 2,
  x.min = seq(0, 63, 4),
  x.max = seq(4, 64, 4),
  y.min = 1,
  y.max = 2,
  base = rep(c("U", "C", "A", "G"), times = 4))

layer_3 <- tibble(
  layer = 3,
  x.min = seq(0, 63, 1),
  x.max = seq(1, 64, 1),
  y.min = 2,
  y.max = 3,
  base = rep(c("U", "C", "A", "G"), times = 16))

prot <- tibble(
  layer = 4,
  x.min = c(0, 2, 4, 8, 10, 12, 14, 15, 16, 20, 24, 26, 28, 32, 35, 36, 40, 42, 44, 46, 48, 52, 56, 58, 60),
  x.max = c(2, 4, 8, 10, 12, 14, 15, 16, 20, 24, 26, 28, 32, 35, 36, 40, 42, 44, 46, 48, 52, 56, 58, 60, 64),
  y.min = 3,
  y.max = 4,
  prot = c("PHE", "LEU", "SER", "TYR", "STOP", "LYS", "STOP", "TRP", "LEU", "PRO", "HIS", "GLN", "ARG",
           "ILE", "MET", "THR", "ASN", "LYS", "SER", "ARG", "VAL", "ALA", "ASP", "GLU", "GLY"))

codons <- rbind(layer_1, layer_2, layer_3)

d1 <- tibble(
  quartet_nb = rep(1:4, each = 11),
  x = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5,
        10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5,
        10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5,
        8, 8, 8, 8, 8, 8, 8, 19, 8, 8, 8),
  y = c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68,
        9.14, 8.14, 8.74, 8.77, 9.26, 8.1, 6.13, 3.1, 9.13, 7.26, 4.74,
        7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73,
        6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.5, 5.56, 7.91, 6.89))

# Create plot ----

p1 <- ggplot() +
  geom_rect(data = codons,
            aes(xmin = x.min, xmax = x.max, ymin = y.min, ymax = y.max,
                fill = base),
            colour = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("#7f58af", "#64c5eb", "#e84d8a", "#feb326")) +
  geom_rect(data = prot,
            aes(xmin = x.min, xmax = x.max, ymin = y.min, ymax = y.max),
            colour = "white", fill = "#0b0742", show.legend = FALSE) +
  geom_text(data = codons,
            aes(x = x.min + (x.max - x.min)/2,
                y = y.min + (y.max - y.min) / 2,
                label = base), 
            colour = "white", size = 8) +
  geom_text(data = prot,
            aes(x = x.min + (x.max - x.min)/2,
                y = y.min + (y.max - y.min) / 2,
                label = prot), 
            colour = "white", size = 6) +
  coord_polar(theta = "x", start = 0) +
  # ggtitle("Genetic code") +
  # labs(caption = "Visualisation: Jonathan Kitt | Data source: Wikipedia| #30DayChartChallenge 2022 | Day 11: circular") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#0b0742", colour = "#0b0742"),
        plot.background = element_rect(fill = "#0b0742", colour = "#0b0742"),
        plot.title = element_text(family = "Fredericka", colour = "white", size = 100, hjust = 0.5,
                                  margin = margin(t = 20)),
        plot.caption = element_text(colour = "white", size = 25, hjust = 0.5))

p <- p1 + plot_spacer()

ggsave("2022/plots/work_in_progress/11_circular.png", p, dpi = 320, width = 12, height = 6)


