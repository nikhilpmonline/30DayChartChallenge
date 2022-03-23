# 30DayChartChallenge
# 2022
# Category : Comparisons
# Day 1 : Part-to-whole
# Last updated 2022-03-23

# Load packages ----

library(tidyverse)
library(showtext)
library(patchwork)
library(ggtext)

# Load fonts ----

font_add_google("Lobster", "Lobster")
showtext_auto()

# Data wrangling ---- 

d1 <- tibble(
  status = c("explored", "unexplored"),
  ratio = c(0.2, 0.8)) %>% 
  mutate(ymin = c(0, max(ratio)),
         ymax = c(max(ratio), 1))

# Create plot ----

p1 <- ggplot(data = d1,
       aes(x = 6, y = ratio, fill = status)) +
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  scale_fill_manual(values = c("#00a1e0", "white")) +
  coord_polar(theta = "y", start = 0) +
  xlim(c(0.025, 8)) +
  annotate("text", x = 0.025, y = 0.5, label = "20 %", colour = "#00a1e0", size = 60, family = "Lobster") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#003c54", colour = "#003c54"),
        plot.background = element_rect(fill = "#003c54", colour = "#003c54"))

p2 <- ggplot() +
  annotate("text", x = 0, y = 1, label = "More than", 
           colour = "#00a1e0", family = "Lobster", size = 25, hjust = 0) +
  annotate("text", x = 20, y = 1, label = "80%", 
           colour = "white", family = "Lobster", size = 25, hjust = 0) +
  annotate("text", x = 30, y = 1, label = "of our ocean is", 
           colour = "#00a1e0", family = "Lobster", size = 25, hjust = 0) +
  annotate("text", x = 0, y = -1, label = "unmapped, unobserved, and unexplored.", 
           colour = "white", family = "Lobster", size = 25, hjust = 0) +
  xlim(0, 100) +
  ylim(-5, 5) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#003c54", colour = "#003c54"),
        plot.background = element_rect(fill = "#003c54", colour = "#003c54"))

p <- p1 + p2 +
  plot_annotation(
    title = "How much of the ocean have we explored ?",
    caption = "Visualisation: Jonathan Kitt | Data source: National Ocean Service | #30DayChartChallenge 2022 | Day 1: part-to-whole",
    theme = theme(plot.title = element_text(family = "Lobster", colour = "#00a1e0", size = 100, hjust = 0.5,
                                            margin = margin(t = 40)),
                  plot.background = element_rect(fill = "#003c54", colour = "#003c54"),
                  plot.caption = element_text(colour = "white", hjust = 0.5, size = 25)))

# Save plot ----
  
ggsave("2022/plots/01_part_to_whole.png", p, dpi = 320, width = 12, height = 6)
