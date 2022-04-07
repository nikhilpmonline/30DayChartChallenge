# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 7 : Physical
# Last updated 2022-04-07

# https://devstronomy.com/#/datasets

# Load packages ----

library(ggforce)
library(patchwork)
library(showtext)
library(tidyverse)

# Load fonts ----

font_add_google("Goldman", "Goldman")
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

# Create plot ----

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
  ggtitle(label = "Planets of the solar system") +
  labs(caption = "Visualisation: Jonathan Kitt | Data source: www.devstronomy.com | #30DayChartChallenge 2022 | Day 7: physical") +
  theme_void() +
  theme(plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.margin = margin(b = 20),
        plot.title = element_text(family = "Goldman", colour = "white",
                                  hjust = 0.5, size = 100, margin = margin(b = 80)),
        plot.caption = element_text(colour = "white", hjust = 0.5,
                                    size = 25, margin = margin(t = 80)))

# Save plot ----

ggsave("2022/plots/07_physical.png", p1, dpi = 320, width = 12, height = 6)
