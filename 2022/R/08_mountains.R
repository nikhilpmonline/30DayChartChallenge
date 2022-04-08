# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 8 : Mountains
# Last updated 2022-04-08

# https://github.com/KittJonathan/munro

# Load packages ----

library(munro)
library(patchwork)
library(showtext)
library(tidyverse)

# Load fonts ----

font_add_google("Righteous", "Righteous")
showtext_auto()

# Data wrangling ----

munros <- munro::munros %>% 
  mutate(type = "Munro") %>% 
  select(type, name, height_meters, height_feet, longitude, latitude) %>% 
  mutate(name = fct_inorder(name))

corbetts <- munro::corbetts %>% 
  mutate(type = "Corbett") %>% 
  select(type, name, height_meters, height_feet, longitude, latitude) %>% 
  mutate(name = fct_inorder(name))

grahams <- munro::grahams %>%
  mutate(type = "Graham") %>% 
  select(type, name, height_meters, height_feet, longitude, latitude) %>% 
  mutate(name = fct_inorder(name))

top3 <- rbind(munros, corbetts, grahams) %>% 
  group_by(type) %>% 
  arrange(desc(height_feet)) %>% 
  filter(row_number() == 1)

uk <- map_data("world") %>% 
  filter(region == "UK", subregion != "Northern Ireland")

# Create plot ----

p1 <- ggplot() +
  geom_segment(data = munros,
               aes(x = name, xend = name, y = 0, yend = height_feet),
               colour = "#2e4b78", size = 1) +
  geom_segment(data = top3 %>% filter(type == "Munro"),
               aes(x = 1, xend = 1, y = 0, yend = height_feet),
               colour = "#a9b7d2", size = 1) +
  annotate("text", x = 1, y = top3$height_feet[top3$type == "Munro"] + 250,
           label = paste0(top3$name[top3$type == "Munro"], " (4,413 feet)"),
           colour = "#a9b7d2", size = 10, hjust = 0, family = "Righteous") +
  annotate("text", x = nrow(munros) / 2, y = 1500,
           label = paste0(nrow(munros), " Munros"),
           colour = "#0a2248", size = 15, family = "Righteous") +
  annotate("text", x = nrow(munros) / 2, y = 500,
           label = "> 3,000 feet",
           colour = "#0a2248", size = 15, family = "Righteous") +
  ylim(0, 5000) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
        plot.background = element_rect(fill = "#0a2248", colour = "#0a2248"))

p2 <- ggplot() +
  geom_segment(data = corbetts,
               aes(x = name, xend = name, y = 0, yend = height_feet),
               colour = "#2e4b78", size = 1) +
  geom_segment(data = top3 %>% filter(type == "Corbett"),
               aes(x = 1, xend = 1, y = 0, yend = height_feet),
               colour = "#a9b7d2", size = 1) +
  annotate("text", x = 1, y = top3$height_feet[top3$type == "Corbett"] + 250,
           label = paste0(top3$name[top3$type == "Corbett"], " (2,999 feet)"),
           colour = "#a9b7d2", size = 10, hjust = 0, family = "Righteous") +
  annotate("text", x = nrow(corbetts) / 2, y = 1500,
           label = paste0(nrow(corbetts), " Corbetts"),
           colour = "#0a2248", size = 15, family = "Righteous") +
  annotate("text", x = nrow(corbetts) / 2, y = 500,
           label = "2,500 - 3,000 feet",
           colour = "#0a2248", size = 15, family = "Righteous") +
  ylim(0, 5000) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
        plot.background = element_rect(fill = "#0a2248", colour = "#0a2248"))

p3 <- ggplot() +
  geom_segment(data = grahams,
               aes(x = name, xend = name, y = 0, yend = height_feet),
               colour = "#2e4b78", size = 1) +
  geom_segment(data = top3 %>% filter(type == "Graham"),
               aes(x = 1, xend = 1, y = 0, yend = height_feet),
               colour = "#a9b7d2", size = 1) +
  annotate("text", x = 1, y = top3$height_feet[top3$type == "Graham"] + 250,
           label = paste0(top3$name[top3$type == "Graham"], " (2,499 feet)"),
           colour = "#a9b7d2", size = 10, hjust = 0, family = "Righteous") +
  annotate("text", x = nrow(grahams) / 2, y = 1500,
           label = paste0(nrow(grahams), " Grahams"),
           colour = "#0a2248", size = 15, family = "Righteous") +
  annotate("text", x = nrow(grahams) / 2, y = 500,
           label = "2,000 - 2,500 feet",
           colour = "#0a2248", size = 15, family = "Righteous") +
  ylim(0, 5000) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
        plot.background = element_rect(fill = "#0a2248", colour = "#0a2248"))

p4 <- ggplot() +
  geom_polygon(data = uk,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "#2e4b78", size = 1) +
  coord_map("gilbert", xlim = c(-7.75, 0), ylim = c(54.6, 59.6)) +
  geom_point(data = munros, 
             aes(x = longitude, y = latitude),
             pch = 17, colour = "#a9b7d2", size = 0.5) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
        plot.background = element_rect(fill = "#0a2248", colour = "#0a2248"))

p5 <- ggplot() +
  geom_polygon(data = uk,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "#2e4b78", size = 1) +
  coord_map("gilbert", xlim = c(-7.75, 0), ylim = c(54.6, 59.6)) +
  geom_point(data = corbetts, 
             aes(x = longitude, y = latitude),
             pch = 17, colour = "#a9b7d2", size = 0.5) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
        plot.background = element_rect(fill = "#0a2248", colour = "#0a2248"))

p6 <- ggplot() +
  geom_polygon(data = uk,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "#2e4b78", size = 1) +
  coord_map("gilbert", xlim = c(-7.75, 0), ylim = c(54.6, 59.6)) +
  geom_point(data = grahams, 
             aes(x = longitude, y = latitude),
             pch = 17, colour = "#a9b7d2", size = 0.5) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
        plot.background = element_rect(fill = "#0a2248", colour = "#0a2248"))

p <- p1 + p2 + p3 + p4 + p5 + p6 +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Mountains of Scotland",
    caption = "Visualisation: Jonathan Kitt | Data source: munro package | #30DayChartChallenge 2022 | Day 8: mountains",
    theme = theme(
      plot.title = element_text(family = "Righteous", colour = "#a9b7d2", size = 120, hjust = 0.5,
                                margin = margin(t = 20, b = 20)),
      plot.caption = element_text(colour = "#a9b7d2", hjust = 0.5, size = 25,
                                  margin = margin(t = 10)),
      panel.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
      plot.background = element_rect(fill = "#0a2248", colour = "#0a2248")))

# Save plot ----

ggsave("2022/plots/08_mountains.png", p, dpi = 320, width = 12, height = 6)
