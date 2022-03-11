# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 7 : Physical
# Last updated 2022-03-11

# https://devstronomy.com/#/datasets

# Load packages ----

library(tidyverse)
library(showtext)
# library(ggwaffle)
# library(emojifont)
library(patchwork)
library(ggforce)

# Testing ----

ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 10),
              fill = "yellow", colour = "yellow") +
  coord_fixed() +
  xlim(0, 100) +
  ylim(-20, 20)

# Load fonts ----

font_add_google("Goldman", "Goldman")
font_add_google("Big Shoulders Inline Text", "Big Shoulders Inline Text")
showtext_auto()

# Import data ----

planets <- read_csv("2022/data/planets.csv")

# Data wrangling ----

d1 <- planets %>% 
  select(planet, diameter, distance_from_sun) %>% 
  filter(planet != "Pluto") %>% 
  rowid_to_column() %>% 
  mutate(radius = diameter / 2) %>% 
  mutate(cum_dist = lag(cumsum(diameter))) %>% 
  mutate(x.0 = case_when(planet == "Mercury" ~ radius,
                         TRUE ~ cum_dist + (rowid-1)*50e3 + radius)) %>% 
  mutate(planet = fct_inorder(planet))

p1 <- ggplot() +
  geom_circle(data = d1,
              aes(x0 = x.0, y0 = 0, r = radius, fill = planet),
              show.legend = FALSE) +
  scale_fill_manual(values = c("#97979f", "#e3bb76", "#8cb1de", "#c1440e",
                               "#e3dccc", "#e2bf7b", "#afdbf5", "#3e66f9")) +
  coord_fixed() +
  geom_text(data = d1,
            aes(x = x.0, y = radius + 10e3, label = planet,
                colour = planet),
            show.legend = FALSE, family = "Goldman", size = 12) +
  scale_colour_manual(values = c("#97979f", "#e3bb76", "#8cb1de", "#c1440e",
                               "#e3dccc", "#e2bf7b", "#afdbf5", "#3e66f9")) +
  annotate("text", x = 100e3, y = 60e3, label = "Relative sizes", family = "Goldman", colour = "white",
           size = 25) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.margin = margin(b = 20))

p2 <- ggplot() +
  geom_segment(data = d1, aes(x = 0, xend = distance_from_sun, 
                              y = planet, yend = planet),
               show.legend = FALSE, colour = "#ffffff", size = 0.1) +
  geom_point(data = d1, aes(x = distance_from_sun, y = planet, colour = planet),
             size = 6, show.legend = FALSE)  +
  geom_text(data = d1,
            aes(x = distance_from_sun + 100, y = planet, label = paste0(planet, " - ", round(distance_from_sun)),
                colour = planet),
            show.legend = FALSE, family = "Goldman", size = 12, hjust = 0) +
  scale_colour_manual(values = c("#97979f", "#e3bb76", "#8cb1de", "#c1440e",
                                 "#e3dccc", "#e2bf7b", "#afdbf5", "#3e66f9")) +
  xlim(0, 5500) +
  annotate("text", x = 3000, y = 3, label = "Distances from the Sun", family = "Goldman", colour = "white",
           size = 25) +
  annotate("text", x = 3000, y = 1.5, label = "(in millions km)", family = "Goldman", colour = "white",
           size = 25) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.margin = margin(t = 20, b = 20))

p <- p1 + p2 +
  plot_layout(ncol = 1, widths = c(1, 1)) +
  plot_annotation(
    title = "Planets of the solar system",
    caption = "Visualisation: Jonathan Kitt | Data source: www.devstronomy.com | #30DayChartChallenge 2022 | Day 7: physical",
    theme = theme(plot.title = element_text(family = "Goldman", colour = "white", size = 120, hjust = 0.5,
                                            margin = margin(t = 20, b = 20)),
                  plot.background = element_rect(fill = "black", colour = "black"),
                  plot.caption = element_text(colour = "white", hjust = 0.5, size = 25)))

ggsave("2022/plots/07_physical.png", p, dpi = 320, width = 12, height = 6)