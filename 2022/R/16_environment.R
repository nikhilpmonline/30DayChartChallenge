# 30DayChartChallenge
# 2022
# Category : Relationships
# Day 16 : Environment
# Last updated 2022-04-01

# https://ourworldindata.org/environmental-impacts-of-food
# https://spiesmanecology.com/2017/04/30/network-vis1/

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
              99.8, 0.2, 0,
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

archaea <- biomass %>% 
  filter(category == "Archaea")


archaea.p1 <- ggplot(data = filter(archaea, location == "Terrestrial"),
       aes(x = 6, y = percent, fill = location)) +
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  ylim(0, 100) +
  scale_fill_manual(values = c("#7339ab", "#393c4b")) +
  coord_polar(theta = "y", start = 0) +
  xlim(c(0.025, 8)) +
  annotate("text", x = 0.025, y = 0.5, label = "6 %", colour = "#7339ab", size = 60) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"))

archaea.p2 <-
  ggplot(data = filter(archaea, location == "Marine"),
                     aes(x = 6, y = percent, fill = location)) +
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  ylim(0, 100) +
  scale_fill_manual(values = c("#88f4ff", "#393c4b")) +
  coord_polar(theta = "y", start = 0) +
  xlim(c(0.025, 8)) +
  annotate("text", x = 0.025, y = 0.5, label = "4 %", colour = "#88f4ff", size = 60) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"))

archaea.p3 <-
  ggplot(data = filter(archaea, location == "Deep subsurface"),
         aes(x = 6, y = percent, fill = location)) +
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  ylim(0, 100) +
  scale_fill_manual(values = c("#1f9ce4", "#393c4b")) +
  coord_polar(theta = "y", start = 0) +
  xlim(c(0.025, 8)) +
  annotate("text", x = 0.025, y = 0.5, label = "90 %", colour = "#1f9ce4", size = 60) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"))

p <- archaea.p1 + archaea.p2 + archaea.p3 +
  archaea.p1 + archaea.p2 + archaea.p3 +
  archaea.p1 + archaea.p2 + archaea.p3 +
  archaea.p1 + archaea.p2 + archaea.p3 +
  archaea.p1 + archaea.p2 + archaea.p3 +
  archaea.p1 + archaea.p2 + archaea.p3 +
  plot_layout(ncol = 3)
p

ggsave("2022/plots/work_in_progress/16_environment.png", p, dpi = 320, width = 12, height = 6)
