# 30DayChartChallenge
# 2022
# Category : Relationships
# Day 15 - multivariate
# Last updated 2022-04-15

# https://allisonhorst.github.io/palmerpenguins/

# Load packages ----

library(GGally)
library(ggtext)
library(palmerpenguins)
library(showtext)
library(tidyverse)

# Load fonts ----

font_add_google("Righteous", "Righteous")
showtext_auto()

# Import data ----

penguins <- palmerpenguins::penguins

# Data wrangling ----

d1 <- penguins %>% 
  select(species, "Bill length (mm)" = bill_length_mm,
         "Bill depth (mm)" = bill_depth_mm, "Flipper length (mm)" = flipper_length_mm)

# Create plot ----

p <- ggpairs(data = d1, columns = 2:4,
        aes(colour = species),
        upper = "blank") +
  labs(
    title = "**Palmer penguins**",
    subtitle = "Comparing three penguin species from the Palmer Archipelago in Antarctica : 
    <span style='color:darkorange;'>**Adelie**</span>, 
    <span style='color:purple;'>Chinstrap</span>, and
    <span style='color:cyan4;'>Gentoo</span>.
    </span>",
    caption = "Visualisation : Jonathan Kitt | Data source : Palmer penguins | #30DayChartChallenge 2022 | Day 15 : multivariate") +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme(panel.background = element_rect(fill = "#dbf3fa", colour = "#dbf3fa"),
        plot.background = element_rect(fill = "#dbf3fa", colour = "#dbf3fa"),
        plot.title = element_markdown(family = "Righteous", size = 75, colour = "#6675bd",
                                      margin = margin(t = 10, b = 5)),
        plot.subtitle = element_markdown(family = "Righteous", size = 50, colour = "#6675bd",
                                      margin = margin(t = 5, b = 20)),
        plot.caption = element_text(colour = "#6675bd", size = 20, hjust = 0.5,
                                    margin = margin(t = 15)),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(family = "Righteous", colour = "#6675bd", size = 20),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = "#6675bd"),
        strip.text = element_text(colour = "white", size = 25, family = "Righteous"))

# Save plot ----

ggsave("2022/plots/15_multivariate.png", p, dpi = 320, width = 12, height = 6)
