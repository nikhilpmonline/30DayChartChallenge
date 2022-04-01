# 30DayChartChallenge
# 2022
# Category : Relationships
# Day 17 : Connections
# Last updated 2022-04-01

# Load packages ----

library(osmdata)
library(showtext)
library(tidyverse)

# Load fonts ----

font_add_google("Eagle Lake", "Eagle Lake")
showtext_auto()

# Data wrangling ----

edin_buses <- getbb("Edinburgh") %>% 
  opq() %>% 
  add_osm_feature(key = "route",
                  value = "bus") %>% 
  osmdata_sf()

# Create plot ----

p <- ggplot() +
  geom_sf(data = edin_buses$osm_lines,
          inherit.aes = FALSE,
          size = 0.2,
          colour = "white") +
  xlim(c(3.32, 3.1)) +
  ylim(c(55.895, 55.99)) +
  ggtitle("EDINBURGH BUSES") +
  labs(caption = "Visualisation: Jonathan Kitt | Data source: OpenStreetMap | #30DayChartChallenge 2022 | Day 17: connections") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#284c5d", colour = "#284c5d"),
        plot.background = element_rect(fill = "#284c5d", colou = "#284c5d"),
        plot.title = element_text(family = "Eagle Lake", hjust = 0.5,
                                  colour = "white", size = 75, margin = margin(t = 20)),
        plot.caption = element_text(colour = "white", hjust = 0.5,
                                    margin = margin(b = 15, t = 20), size = 25))

# Save plot ----

ggsave("2022/plots/work_in_progress/17_connections.png", p, dpi = 320, width = 12, height = 6)