# 30DayChartChallenge
# 2022
# Category : Relationships
# Day 17 : Connections
# Last updated 2022-04-01

# Load packages ----

library(osmdata)
library(patchwork)
library(showtext)
library(tidyverse)

# Load fonts ----

font_add_google("Turret Road", "Turret Road")
showtext_auto()

# Data wrangling ----

lille <- getbb("Lille") %>% 
  opq() %>% 
  add_osm_feature(key = "route",
                  value = "subway") %>% 
  osmdata_sf()

lyon <- getbb("Lyon") %>% 
  opq() %>% 
  add_osm_feature(key = "route",
                  value = "subway") %>% 
  osmdata_sf()

marseille <- getbb("Marseille") %>% 
  opq() %>% 
  add_osm_feature(key = "route",
                  value = "subway") %>% 
  osmdata_sf()

paris <- getbb("Paris") %>% 
  opq() %>% 
  add_osm_feature(key = "route",
                  value = "subway") %>% 
  osmdata_sf()

rennes <- getbb("Rennes") %>% 
  opq() %>% 
  add_osm_feature(key = "route",
                  value = "subway") %>% 
  osmdata_sf()

toulouse <- getbb("Toulouse") %>% 
  opq() %>% 
  add_osm_feature(key = "route",
                  value = "subway") %>% 
  osmdata_sf()

# Create plot ----

p1 <- ggplot() +
  geom_sf(data = lille$osm_lines,
          inherit.aes = FALSE,
          size = 0.25,
          colour = "white") +
  ggtitle(label = "Lille") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#284c5d", colour = "#284c5d"),
        plot.background = element_rect(fill = "#284c5d", colou = "#284c5d"),
        plot.title = element_text(family = "Turret Road", colour = "white", size = 35, hjust = 0),
        plot.margin = margin(t = 10, b = 10, l = 10, r = 10))

p2 <- ggplot() +
  geom_sf(data = lyon$osm_lines,
          inherit.aes = FALSE,
          size = 0.25,
          colour = "white") +
  ggtitle(label = "Lyon") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#284c5d", colour = "#284c5d"),
        plot.background = element_rect(fill = "#284c5d", colou = "#284c5d"),
        plot.title = element_text(family = "Turret Road", colour = "white", size = 35, hjust = 0),
        plot.margin = margin(t = 10, b = 10, l = 10, r = 10))

p3 <- ggplot() +
  geom_sf(data = marseille$osm_lines,
          inherit.aes = FALSE,
          size = 0.25,
          colour = "white") +
  ggtitle(label = "Marseille") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#284c5d", colour = "#284c5d"),
        plot.background = element_rect(fill = "#284c5d", colou = "#284c5d"),
        plot.title = element_text(family = "Turret Road", colour = "white", size = 35, hjust = 0),
        plot.margin = margin(t = 10, b = 10, l = 10, r = 10))

p4 <- ggplot() +
  geom_sf(data = paris$osm_lines,
          inherit.aes = FALSE,
          size = 0.25,
          colour = "white") +
  ggtitle(label = "Paris") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#284c5d", colour = "#284c5d"),
        plot.background = element_rect(fill = "#284c5d", colou = "#284c5d"),
        plot.title = element_text(family = "Turret Road", colour = "white", size = 35, hjust = 0),
        plot.margin = margin(t = 10, b = 10, l = 10, r = 10))

p5 <- ggplot() +
  geom_sf(data = rennes$osm_lines,
          inherit.aes = FALSE,
          size = 0.25,
          colour = "white") +
  ggtitle(label = "Rennes") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#284c5d", colour = "#284c5d"),
        plot.background = element_rect(fill = "#284c5d", colou = "#284c5d"),
        plot.title = element_text(family = "Turret Road", colour = "white", size = 35, hjust = 0),
        plot.margin = margin(t = 10, b = 10, l = 10, r = 10))

p6 <- ggplot() +
  geom_sf(data = toulouse$osm_lines,
          inherit.aes = FALSE,
          size = 0.25,
          colour = "white") + 
  ggtitle(label = "Toulouse") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#284c5d", colour = "#284c5d"),
        plot.background = element_rect(fill = "#284c5d", colou = "#284c5d"),
        plot.title = element_text(family = "Turret Road", colour = "white", size = 35, hjust = 0),
        plot.margin = margin(t = 10, b = 10, l = 10, r = 10))

p <- p1 + p2 + p3 + p4 + p5 + p6 +
  plot_annotation(
    title = "Subways in french cities",
    caption = "Visualisation: Jonathan Kitt | Data source: OpenStreetMap | #30DayChartChallenge 2022 | Day 17: connections",
    theme = theme(plot.title = element_text(family = "Turret Road", colour = "white", size = 75, hjust = 0.5,
                                            margin = margin(t = 10, b = 40)),
                  plot.background = element_rect(fill = "#284c5d", colour = "#284c5d"),
                  plot.caption = element_text(colour = "white", hjust = 0.5, size = 25)))

# Save plot ----

ggsave("2022/plots/work_in_progress/17_connections.png", p, dpi = 320, width = 12, height = 6)