# 30DayChartChallenge
# 2022
# Category : Comparisons
# Day 2 : Pictogram
# Last updated 2022-03-07

# Load packages ----

library(tidyverse)
library(grid)
library(ggimage)
library(showtext)

# Load fonts ----

# font_add_google("Lobster", "Lobster")
# showtext_auto()
font_add_google("Rajdhani", "Rajdhani")
showtext_auto()

# Data wrangling ---- 

d1 <- tibble(
  x.pos = 0.25,
  y.pos = c(0.6, 0.5, 0.4, 0.3),
  img = c("2022/data/findable.png",
          "2022/data/accessible.png",
          "2022/data/interoperable.png",
          "2022/data/reusable.png"))


ggsave("2022/plots/02_pictogram.png", p, dpi = 320, width = 12, height = 6)


# Create plot ----

iceblue <- c("#71a6d1", "#85b5d9", "#99c4e1", "#acd3e8", "#c0e2f0", "#d4f1f8")
g <- rasterGrob(iceblue, width = unit(1, "npc"), height = unit(1, "npc"))


ggplot(data = d1) +
  xlim(0, 1) +
  ylim(0, 1) +
  annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_image(aes(x = x.pos, y = y.pos, image = img),
             colour = "white", size = 0.1) +
  annotate("text", x = 0.5, y = 0.95, label = "The FAIR principles", family = "Rajdhani", size = 25, colour = "white")

p1 <- ggplot() +
  annotate("text", x = -0.5, y = 1, label = "Munros are mountains in Scotland above 3,000 feet",
           colour = "white", family = "Lobster", size = 20) +
  annotate("text", x = 0.5, y = -1, label = "Out of 282 Munros, 186 are located in the Highlands",
           colour = "white", family = "Lobster", size = 20) +
  xlim(-15, 15) +
  ylim(-5, 5) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#041f32", colour = "#041f32"),
        plot.background = element_rect(fill = "#041f32", colour = "#041f32"))

p2 <- ggplot(data = highland_munros,
             aes(x = 6, y = ratio, fill = county)) +
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  scale_fill_manual(values = c("#04aed9", "#c5e0f5")) +
  coord_polar(theta = "y", start = 0) +
  xlim(c(0.025, 8)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#041f32", colour = "#041f32"),
        plot.background = element_rect(fill = "#041f32", colour = "#041f32"))

p <- p1 + p2 +
  plot_annotation(
    title = "How many Munros are located in the Highlands?",
    caption = "Visualisation: Jonathan Kitt | Data source: munro package | #30DayChartChallenge 2022 | Day 1: part-to-whole",
    theme = theme(plot.title = element_text(family = "Lobster", colour = "white", size = 100, hjust = 0.5,
                                            margin = margin(t = 40)),
                  plot.background = element_rect(fill = "#041f32"),
                  plot.caption = element_text(colour = "white", hjust = 0.5, size = 25)))

ggsave("2022/plots/02_pictogram.png", p, dpi = 320, width = 12, height = 6)
