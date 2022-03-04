# 30DayChartChallenge

# Last updated 2022-03-04

# Load packages ----

library(tidyverse)
library(munro)
library(showtext)
library(patchwork)

# Comparisons ----

# 1 - Part-to-whole

font_add_google("Lobster", "Lobster")
showtext_auto()

munros <- munro::munros

d1 <- munros %>% 
  mutate(unique_name = paste(1:nrow(.), name, sep = " - ")) %>% 
  select(unique_name, county) %>% 
  separate_rows(county, sep = ",") %>% 
  mutate(county = factor(str_trim(county))) %>% 
  count(county)

highland_munros <- tibble(
  county = c("Highland", "Other"),
  n = c(d1$n[d1$county == "Highland"],
        nrow(munros) - d1$n[d1$county == "Highland"])) %>% 
  mutate(ratio = n / sum(n)) %>% 
  mutate(ymin = c(0, max(ratio)),
         ymax = c(max(ratio), 1))

p1 <- ggplot(data = highland_munros,
       aes(x = 4, y = ratio, fill = county)) +
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  scale_fill_manual(values = c("#04aed9", "#c5e0f5")) +
  coord_polar(theta = "y", start = 0) +
  xlim(c(0.025, 8)) +
  ggtitle(label = "How many Munros are located in the Highlands ?") +
  annotate("text", x = 7.5, y = 0, label = "Munros are mountains in Scotland above 3,000 feet",
           colour = "white", family = "Lobster", size = 15) +
  annotate("text", x = 6, y = 0, label = "Out of 282 Munros, 186 are located in the Highlands",
           colour = "white", family = "Lobster", size = 15) +
  annotate("text", x = 6, y = 0.5, label = "Visualisation: Jonathan Kitt | Data source: munro package | #30DayChartChallenge 2022 | Day 1: part-to-whole",
           colour = "white", size = 8) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#041f32", colour = "#041f32"),
        plot.background = element_rect(fill = "#041f32", colour = "#041f32"),
        plot.title = element_text(colour = "white", hjust = 0.5,
                                  family = "Lobster", size = 50, margin = margin(t = 30)))

ggsave("plots/01_part_to_whole.png", p1, dpi = 320, width = 12, height = 6)

p1 <- ggplot(data = highland_munros,
       aes(x = 4, y = ratio, fill = county)) +
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  scale_fill_manual(values = c("#04aed9", "#c5e0f5")) +
  coord_polar(theta = "y", start = 0) +
  xlim(c(0.025, 8)) +
  # ggtitle(label = "How many Munros are located in the Highlands ?") +
  # annotate("text", x = 7.5, y = 0, label = "Munros are mountains in Scotland above 3,000 feet",
  #          colour = "white", family = "Lobster", size = 15) +
  # annotate("text", x = 6, y = 0, label = "Out of 282 Munros, 186 are located in the Highlands",
  #          colour = "white", family = "Lobster", size = 15) +
  # annotate("text", x = 6, y = 0.5, label = "Visualisation: Jonathan Kitt | Data source: munro package | #30DayChartChallenge 2022 | Day 1: part-to-whole",
  #          colour = "white", size = 8) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#041f32", colour = "#041f32"),
        plot.background = element_rect(fill = "#041f32", colour = "#041f32"),
        plot.title = element_text(colour = "white", hjust = 0.5,
                                  family = "Lobster", size = 50, margin = margin(t = 30)))

p2 <- ggplot() +
  annotate("text", x = -0.5, y = 1, label = "Munros are mountains in Scotland above 3,000 feet",
           colour = "white", family = "Lobster", size = 20) +
  annotate("text", x = 0.5, y = -1, label = "Out of 282 Munros, 186 are located in the Highlands",
           colour = "white", family = "Lobster", size = 20) +
  xlim(-15, 15) +
  ylim(-5, 5) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#041f32", colour = "#041f32"),
        plot.background = element_rect(fill = "#041f32", colour = "#041f32"))

p <- p2 + p1 +
  plot_annotation(
    title = "How many Munros are located in the Highlands?",
    caption = "Visualisation: Jonathan Kitt | Data source: munro package | #30DayChartChallenge 2022 | Day 1: part-to-whole",
    theme = theme(plot.title = element_text(family = "Lobster", colour = "white", size = 100, hjust = 0.5,
                                            margin = margin(t = 40)),
                  plot.background = element_rect(fill = "#041f32"),
                  plot.caption = element_text(colour = "white", hjust = 0.5, size = 25)))

ggsave("plots/01_part_to_whole.png", p, dpi = 320, width = 12, height = 6)


  
# 2 - Pictogram
# 3 - Historical
# 4 - Flora
# 5 - Slope
# 6 - Data day: OWID

# Distributions ----
# 7 - Physical
# 8 - Mountains
# 9 - Statistics
# 10 - Experimental
# 11 - Circular
# 12 - Theme day: The Economist
# Relationships ----
# Timeseries ----
# Uncertainties ----