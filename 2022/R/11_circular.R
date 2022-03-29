# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 11 : Circular
# Last updated 2022-03-28

# Sources : 
# https://en.wikipedia.org/wiki/DNA_and_RNA_codon_tables#Translation_table_1
# https://quantum-society.com/2022/03/05/biochemistry-101-amino-acids/

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

aa <- tibble(
  layer = 4,
  x.min = c(0, 2, 4, 8, 10, 12, 14, 15, 16, 20, 24, 26, 28, 32, 35, 36, 40, 42, 44, 46, 48, 52, 56, 58, 60),
  x.max = c(2, 4, 8, 10, 12, 14, 15, 16, 20, 24, 26, 28, 32, 35, 36, 40, 42, 44, 46, 48, 52, 56, 58, 60, 64),
  y.min = 3,
  y.max = 4,
  code_1_letter = c("F", "L", "S", "Y", "STOP", "C", "STOP", "W", "L", "P",
                    "H", "Q", "R", "I", "M", "T", "N", "K", "S", "R", "V", "A", "D", "E", "G"),
  aa_name = c("Phenylalanine", "Leucine", "Serine", "Tyrosine", "Stop", "Cystein", "Stop",
             "Tryptophan", "Leucine", "Proline", "Histidine", "Glutamine", "Arginine",
             "Isoleucine", "Methionine", "Threonine", "Asparagine", "Lysine", "Serine", "Arginine",
             "Valine", "Alanine", "Aspartic acid", "Glutaminc acid", "Glycine"))

aa_names <- tibble(
  code = c("A", "G", "I", "L", "P", "V", "P", "W", "Y", "D", "E", "R", "H", "K", "S", "T", "C", "M", "N", "Q"),
  name = c("ALANINE", "GLYCINE", "ISOLEUCINE", "LEUCINE", "PROLINE", "VALINE", "PHENYLALANINE",
           "TRYPTOPHAN", "TYROSINE", "ASPARTIC ACID", "GLUTAMIC ACID", "ARGININE", "HISTIDINE",
           "LYSINE", "SERINE", "THREONINE", "CYSTEINE", "METHIONINE", "ASPARAGINE", "GLUTAMINE"),
  type = c(rep("ALIPHATIC", 6), rep("AROMATIC", 3), rep("ACIDIC", 2), rep("BASIC", 3), 
           rep("HYDROXYLIC", 2), rep("SULFUR-CONTAINING", 2), rep("AMIDIC", 2)),
  y.pos = rep(c(11:1, 11:3)),
  x.pos = c(rep(1, times = 11), rep(2, times = 9)))

codons <- rbind(layer_1, layer_2, layer_3)

# Create plot ----

p1 <- ggplot() +
  geom_rect(data = codons,
            aes(xmin = x.min, xmax = x.max, ymin = y.min, ymax = y.max,
                fill = base),
            colour = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("#7f58af", "#64c5eb", "#e84d8a", "#feb326")) +
  geom_rect(data = aa,
            aes(xmin = x.min, xmax = x.max, ymin = y.min, ymax = y.max),
            colour = "white", fill = "#0b0742", show.legend = FALSE) +
  geom_text(data = codons,
            aes(x = x.min + (x.max - x.min)/2,
                y = y.min + (y.max - y.min) / 2,
                label = base), 
            colour = "white", size = 8) +
  geom_text(data = aa,
            aes(x = x.min + (x.max - x.min)/2,
                y = y.min + (y.max - y.min) / 2,
                label = code_1_letter), 
            colour = "white", size = 8) +
  coord_polar(theta = "x", start = 0) +
  # ggtitle("Genetic code") +
  # labs(caption = "Visualisation: Jonathan Kitt | Data source: Wikipedia| #30DayChartChallenge 2022 | Day 11: circular") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#0b0742", colour = "#0b0742"),
        plot.background = element_rect(fill = "#0b0742", colour = "#0b0742"),
        plot.title = element_text(family = "Fredericka", colour = "white", size = 100, hjust = 0.5,
                                  margin = margin(t = 20)),
        plot.caption = element_text(colour = "white", size = 25, hjust = 0.5))

ggplot(data = aa_names) +
  geom_point(aes(x = x.pos, y = y.pos, colour = type),
             size = 8, shape = 19) +
  geom_text(aes(x = x.pos, y = y.pos,
                label = code)) +
  geom_text(aes(x = x.pos + 0.2, y = y.pos,
                label = name), hjust = 0) +
  xlim(0, 3) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

  geom_text(aes(x = 0.5, y = y.pos, label = code)) +
+
  geom_text(aes(x = 0.5, y = y.pos, label = code)) +
  geom_text(aes(x = 1, y = y.pos, label = name), hjust = 0) +
  xlim(0, 10)
  

p <- p1 + p2

ggsave("2022/plots/work_in_progress/11_circular.png", p, dpi = 320, width = 12, height = 6)


