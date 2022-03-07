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
  y.pos = c(0.7, 0.5, 0.3, 0.1),
  img = c("2022/data/findable.png",
          "2022/data/accessible.png",
          "2022/data/interoperable.png",
          "2022/data/reusable.png"))

# Create plot ----

#iceblue <- c("#71a6d1", "#85b5d9", "#99c4e1", "#acd3e8", "#c0e2f0", "#d4f1f8")
#g <- rasterGrob(iceblue, width = unit(1, "npc"), height = unit(1, "npc"))

# dark <- c("#390640", "#332859", "#253c59", "#1b818c", "#17a6a6")
# dark <- c("#350c3e", "#341042", "#321947", "#30214e", "#2e2a53", "#2d2e57", "#2a3b5f", "#284366", "#0eaeb1")
dark <- c("#332859", "#253c59", "#20788c", "#17a6a6", "#0fbfae")
g <- rasterGrob(dark, width = unit(1, "npc"), height = unit(1, "npc"))

# g <- rasterGrob(blues9, width = unit(1, "npc"), height = unit(1, "npc"))


p <- ggplot(data = d1) +
  xlim(0, 1) +
  ylim(0, 1) +
  annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_image(aes(x = x.pos, y = y.pos, image = img),
             colour = "white") +
  annotate("text", x = 0.5, y = 0.95, label = "The FAIR principles", family = "Rajdhani", size = 25, colour = "white") +
  annotate("text", x = 0.4, y = 0.7, label = "Findable", family = "Rajdhani", size = 25, colour = "white") +
  annotate("text", x = 0.4, y = 0.5, label = "Accessible", family = "Rajdhani", size = 25, colour = "white") +
  annotate("text", x = 0.4, y = 0.3, label = "Interoperable", family = "Rajdhani", size = 25, colour = "white") +
  annotate("text", x = 0.4, y = 0.1, label = "Reusable", family = "Rajdhani", size = 25, colour = "white") +
  annotate("text", x = 0.5, y = 0.01, label = "Visualisation: Jonathan Kitt | Data source: ... | #30DayChartChallenge 2022 | Day 2: pictogram") +
  theme_void()

ggsave("2022/plots/02_pictogram.png", p, dpi = 320, width = 12, height = 6)

p <- p1 + p2 +
  plot_annotation(
    title = "How many Munros are located in the Highlands?",
    caption = "Visualisation: Jonathan Kitt | Data source: munro package | #30DayChartChallenge 2022 | Day 1: part-to-whole",
    theme = theme(plot.title = element_text(family = "Lobster", colour = "white", size = 100, hjust = 0.5,
                                            margin = margin(t = 40)),
                  plot.background = element_rect(fill = "#041f32"),
                  plot.caption = element_text(colour = "white", hjust = 0.5, size = 25)))

ggsave("2022/plots/02_pictogram.png", p, dpi = 320, width = 12, height = 6)
