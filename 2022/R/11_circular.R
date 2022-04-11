# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 11 : Circular
# Last updated 2022-03-29

# Sources : 
# https://en.wikipedia.org/wiki/DNA_and_RNA_codon_tables#Translation_table_1
# https://www.compoundchem.com/2014/09/16/aminoacids/

# Load packages ----

library(patchwork)
library(showtext)
library(tidyverse)

# Load fonts ----

font_add_google("Fredericka the Great", "Fredericka")
font_add_google("Gluten", "Gluten")
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
  code = c("A", "G", "I", "L", "P", "V", "F", "W", "Y", "D", "E", "R", "H", "K", "S", "T", "C", "M", "N", "Q"),
  name = c("ALANINE", "GLYCINE", "ISOLEUCINE", "LEUCINE", "PROLINE", "VALINE", "PHENYLALANINE",
           "TRYPTOPHAN", "TYROSINE", "ASPARTIC ACID", "GLUTAMIC ACID", "ARGININE", "HISTIDINE",
           "LYSINE", "SERINE", "THREONINE", "CYSTEINE", "METHIONINE", "ASPARAGINE", "GLUTAMINE"),
  type = c(rep("ALIPHATIC", 6), rep("AROMATIC", 3), rep("ACIDIC", 2), rep("BASIC", 3), 
           rep("HYDROXYLIC", 2), rep("SULFUR-CONTAINING", 2), rep("AMIDIC", 2)),
  y.pos = c(9:4, 9:5, 9:5, 9:6),
  x.pos = c(rep(1, times = 6), rep(5, times = 5), rep(10, times = 5), rep(14, times = 4))) %>% 
  mutate(type = factor(type, levels = c("ALIPHATIC", "AROMATIC", "ACIDIC", "BASIC",
                                        "HYDROXYLIC", "SULFUR-CONTAINING", "AMIDIC")))

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
                label = base, family = "Gluten"), 
            colour = "white", size = 8) +
  geom_text(data = aa,
            aes(x = x.min + (x.max - x.min)/2,
                y = y.min + (y.max - y.min) / 2,
                label = code_1_letter), 
            colour = "white", size = 8, family = "Gluten") +
  coord_polar(theta = "x", start = 0) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#0b0742", colour = "#0b0742"),
        plot.background = element_rect(fill = "#0b0742", colour = "#0b0742"))

p2 <- ggplot(data = aa_names) +
  geom_point(aes(x = x.pos, y = y.pos, colour = type, stroke = 1.5),
             size = 8,  shape = 21, show.legend = FALSE) +
  scale_colour_manual(values = c("#d11141", "#00b159", "#f37735", "#00aedb",
                                 "#8c8c8c", "#ffc425", "#cccccc")) +
  geom_text(aes(x = x.pos, y = y.pos,
                label = code), colour = "white",
            size = 10) +
  geom_text(aes(x = x.pos + 0.75, y = y.pos,
                label = name), hjust = 0, size = 10, colour = "white",
            family = "Gluten") +
  annotate("text", family = "Gluten", colour = "white", x = 0, y = 2, size = 15, hjust = 0,
           label = "The 20 amino-acids encoded by the human genetic code") +
  annotate("text", family = "Gluten", colour = "white", x = 0, y = 1.5, size = 15, hjust = 0,
           label = "are either") +
  annotate("text", family = "Gluten", colour = "#d11141", x = 2.75, y = 1.5, size = 15, hjust = 0,
           label = "aliphatic") +
  annotate("text", family = "Gluten", colour = "white", x = 5, y = 1.5, size = 15, hjust = 0,
           label = ", ") +
  annotate("text", family = "Gluten", colour = "#00b159", x = 5.3, y = 1.5, size = 15, hjust = 0,
           label = "aromatic") +
  annotate("text", family = "Gluten", colour = "white", x = 7.6, y = 1.5, size = 15, hjust = 0,
           label = ", ") +
  annotate("text", family = "Gluten", colour = "#f37735", x = 7.9, y = 1.5, size = 15, hjust = 0,
           label = "acidic") +
  annotate("text", family = "Gluten", colour = "white", x = 9.4, y = 1.5, size = 15, hjust = 0,
           label = ", ") +
  annotate("text", family = "Gluten", colour = "#00aedb", x = 9.7, y = 1.5, size = 15, hjust = 0,
           label = "basic") +
  annotate("text", family = "Gluten", colour = "white", x = 11.05, y = 1.5, size = 15, hjust = 0,
           label = ", ") +
  annotate("text", family = "Gluten", colour = "#8c8c8c", x = 11.35, y = 1.5, size = 15, hjust = 0,
           label = "hydroxylic") +
  annotate("text", family = "Gluten", colour = "white", x = 14.1, y = 1.5, size = 15, hjust = 0,
           label = ", ") +
  annotate("text", family = "Gluten", colour = "#ffc425", x = 0, y = 1, size = 15, hjust = 0,
           label = "sulfur-containing") +
  annotate("text", family = "Gluten", colour = "white", x = 4.85, y = 1, size = 15, hjust = 0,
           label = "or ") +
  annotate("text", family = "Gluten", colour = "#cccccc", x = 5.6, y = 1, size = 15, hjust = 0,
           label = "amidic") +
  annotate("text", family = "Gluten", colour = "white", x = 7.25, y = 1, size = 15, hjust = 0,
           label = ".") +
  xlim(0, 18) +
  ylim(0, 12) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#0b0742", colour = "#0b0742"),
        plot.background = element_rect(fill = "#0b0742", colour = "#0b0742"))


p <- p1 + p2 +
  plot_annotation(
    title = "Genetic code",
    caption = "Visualisation: Jonathan Kitt | Data source: Wikipedia & Compound Chemistry | #30DayChartChallenge 2022 | Day 11: circular",
    theme = theme(plot.title = element_text(family = "Fredericka", colour = "white", size = 120, hjust = 0.5,
                                            margin = margin(t = 20)),
                  plot.background = element_rect(fill = "#0b0742", colour = "#0b0742"),
                  plot.caption = element_text(colour = "white", hjust = 0.5, size = 25)))


# Save plot ----

ggsave("2022/plots/11_circular.png", p, dpi = 320, width = 12, height = 6)
