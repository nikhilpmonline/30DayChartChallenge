# 30DayChartChallenge
# 2022
# Category : Comparisons
# Day 3 : Historical
# Last updated 2022-03-09

# Source : https://en.wikipedia.org/wiki/Anscombe%27s_quartet

# Load packages ----

library(tidyverse)
library(showtext)
# library(ggwaffle)
# library(emojifont)
library(patchwork)

# Load fonts ----

# font_add_google("Rajdhani", "Rajdhani")
# showtext_auto()

# Data wrangling ----

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

ggplot(data = d1 %>% filter(quartet_nb == 1)) +
  geom_point(aes(x = x, y = y)) +
  xlim(0, 20) +
  ylim(0, 14) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13)

ggplot(data = d1 %>% filter(quartet_nb == 2)) +
  geom_point(aes(x = x, y = y)) +
  xlim(0, 20) +
  ylim(0, 14) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13)

ggplot(data = d1 %>% filter(quartet_nb == 3)) +
  geom_point(aes(x = x, y = y)) +
  xlim(0, 20) +
  ylim(0, 14) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13)

ggplot(data = d1 %>% filter(quartet_nb == 4)) +
  geom_point(aes(x = x, y = y)) +
  xlim(0, 20) +
  ylim(0, 14) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13)

p1 <- ggplot() +
  annotate("text", label = "Mobile OS market",
           x = 0, y = 0.25, family = "Rajdhani", size = 50, colour = "white") +
  annotate("text", label = "share worldwide",
           x = 0, y = -0.25, family = "Rajdhani", size = 50, colour = "white") +
  ylim(c(-1, 1)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#332859", colour = "#332859"),
        plot.background = element_rect(fill = "#332859", colour = "#332859"))

p2 <- ggplot(waffle_d1, aes(x, -y)) + 
  geom_text(aes(label = label, colour = group),
            family='fontawesome-webfont', size = 35,
            show.legend = FALSE) +
  coord_equal() + 
  scale_colour_manual(values = c("#a4c739", "#dadada", "#0fbfae")) + 
  theme_waffle() +
  theme_void() +
  theme(panel.background = element_rect(fill = "#332859", colour = "#332859"),
        plot.background = element_rect(fill = "#332859", colour = "#332859"),
        plot.margin = margin(t = 40, r = 40, b = 40, l = 40))

p3 <- p1 + p2 +
  plot_annotation(
    caption = "Visualisation: Jonathan Kitt | Data source: https://gs.statcounter.com/os-market-share/mobile/worldwide | #30DayChartChallenge 2022 | Day 2: pictogram",
    theme = theme(plot.background = element_rect(fill = "#332859", colour = "#332859"),
                  plot.caption = element_text(colour = "white", hjust = 0.5, size = 25)))

ggsave("2022/plots/02_pictogram.png", p3, dpi = 320, width = 12, height = 6)
