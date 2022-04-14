# 30DayChartChallenge
# 2022
# Category : Relationships
# Day 16 : Environment
# Last updated 2022-04-01

# https://ourworldindata.org/environmental-impacts-of-food

# Load packages ----

library(tidyverse)
library(showtext)
# library(ggwaffle)
# library(emojifont)
library(patchwork)

# Load fonts ----

# font_add_google("Tangerine", "Tangerine")
# showtext_auto()

# Import data ----

food <- read_csv("2022/data/food-footprints.csv")

# Data wrangling ----

# https://ourworldindata.org/life-by-environment

biomass <- tibble(
  category = rep(c("All life", "Plants", "Fungi", "Protists", "Animals", "Bacteria", "Archaea"), each = 3),
  location = rep(c("Terrestrial", "Marine", "Deep subsurface"), times = 7),
  percent = c(86, 1, 13,
              100, 0, 0,
              98, 2, 0,
              44, 56, 0,
              22, 78, 0,
              9, 2, 89,
              6, 4, 90),
  biomass = c(470, 6, 70, rep(0, 18))) %>% 
  mutate(location = factor(location, levels = c("Terrestrial", "Marine", "Deep subsurface")))

# Create plot ----

d1 <- biomass %>% 
  filter(category == "All life")

d1

ggplot(data = d1,
       aes(x = category, y = percent, fill = fct_rev(location))) +
  geom_bar(position = "stack", stat = "identity", show.legend = FALSE,
           width = 0.25) +
  coord_flip() +
  scale_fill_manual(values = c("#1f9ce4", "#88f4ff", "#7339ab")) +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"))
