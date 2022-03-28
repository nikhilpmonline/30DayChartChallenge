# 30DayChartChallenge
# 2022
# Category : Comparisons
# Day 3 : Historical
# Last updated 2022-03-28

# Source : https://en.wikipedia.org/wiki/Anscombe's_quartet

# Load packages ----

library(patchwork)
library(showtext)
library(tidyverse)

# Load fonts ----

font_add_google("Tangerine", "Tangerine")
showtext_auto()

# Data wrangling ----

d1 <- tibble(
  quartet_nb = rep(1:4, each = 11),
  x = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5,
        10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5,
        10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5,
        8, 8, 8, 8, 8, 8, 8, 19, 8, 8, 8),
  y = c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68,
        9.14, 8.14, 8.74, 8.77, 9.26, 8.1, 6.13, 3.1, 9.13, 7.26, 4.74,
        7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73,
        6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.5, 5.56, 7.91, 6.89))

# Create plot ----

p1 <- ggplot(data = d1 %>% filter(quartet_nb == 1)) +
  scale_x_continuous(limits = c(2, 19), breaks = seq(4, 18, 2)) +
  scale_y_continuous(limits = c(2, 14), breaks = seq(4, 12, 2)) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13,
               colour = "#000000", size = 0.05) +
  geom_point(aes(x = x, y = y),
             size = 2, colour = "#ffa500") +
  annotate("text", family = "Tangerine", size = 12, x = 3, y = 12, label = "mean(y) = 7.5") +
  annotate("text", family = "Tangerine", size = 12, x = 3, y = 10, label = "var(y) = 4.13") +
  annotate("text", family = "Tangerine", size = 12, x = 16, y = 5, label = "mean(x) = 9") +
  annotate("text", family = "Tangerine", size = 12, x = 16, y = 3, label = "var(x) = 7.5") +
  annotate("text", family = "Tangerine", size = 12, x = 18, y = 11.5, angle = 12, label = "y = 3 + 0.5x") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#f3ddc2", colour = "#f3ddc2"),
        plot.margin = margin(t = 10, r = 40, b = 10, l = 10),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        axis.title = element_text(family = "Tangerine", size = 50),
        axis.text = element_text(family = "Tangerine", size = 40))
  
p2 <- ggplot(data = d1 %>% filter(quartet_nb == 2)) +
  scale_x_continuous(limits = c(2, 19), breaks = seq(4, 18, 2)) +
  scale_y_continuous(limits = c(2, 14), breaks = seq(4, 12, 2)) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13,
               colour = "#000000", size = 0.05) +
  geom_point(aes(x = x, y = y),
             size = 2, colour = "#0099ff") +
  annotate("text", family = "Tangerine", size = 12, x = 3, y = 12, label = "mean(y) = 7.5") +
  annotate("text", family = "Tangerine", size = 12, x = 3, y = 10, label = "var(y) = 4.13") +
  annotate("text", family = "Tangerine", size = 12, x = 16, y = 5, label = "mean(x) = 9") +
  annotate("text", family = "Tangerine", size = 12, x = 16, y = 3, label = "var(x) = 7.5") +
  annotate("text", family = "Tangerine", size = 12, x = 18, y = 11.5, angle = 12, label = "y = 3 + 0.5x") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#f3ddc2", colour = "#f3ddc2"),
        plot.margin = margin(t = 10, r = 40, b = 10, l = 10),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        axis.title = element_text(family = "Tangerine", size = 50),
        axis.text = element_text(family = "Tangerine", size = 40))

p3 <- ggplot(data = d1 %>% filter(quartet_nb == 3)) +
  scale_x_continuous(limits = c(2, 19), breaks = seq(4, 18, 2)) +
  scale_y_continuous(limits = c(2, 14), breaks = seq(4, 12, 2)) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13,
               colour = "#000000", size = 0.05) +
  geom_point(aes(x = x, y = y),
             size = 2, colour = "#009e73") +
  annotate("text", family = "Tangerine", size = 12, x = 3, y = 12, label = "mean(y) = 7.5") +
  annotate("text", family = "Tangerine", size = 12, x = 3, y = 10, label = "var(y) = 4.12") +
  annotate("text", family = "Tangerine", size = 12, x = 16, y = 5, label = "mean(x) = 9") +
  annotate("text", family = "Tangerine", size = 12, x = 16, y = 3, label = "var(x) = 7.5") +
  annotate("text", family = "Tangerine", size = 12, x = 18, y = 11.5, angle = 12, label = "y = 3 + 0.5x") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#f3ddc2", colour = "#f3ddc2"),
        plot.margin = margin(t = 10, r = 40, b = 10, l = 10),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        axis.title = element_text(family = "Tangerine", size = 50),
        axis.text = element_text(family = "Tangerine", size = 40))

p4 <- ggplot(data = d1 %>% filter(quartet_nb == 4)) +
  scale_x_continuous(limits = c(2, 19), breaks = seq(4, 18, 2)) +
  scale_y_continuous(limits = c(2, 14), breaks = seq(4, 12, 2)) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13,
               colour = "#000000", size = 0.05) +
  geom_point(aes(x = x, y = y),
             size = 2, colour = "#b32db5") +
  annotate("text", family = "Tangerine", size = 12, x = 3, y = 12, label = "mean(y) = 7.5") +
  annotate("text", family = "Tangerine", size = 12, x = 3, y = 10, label = "var(y) = 4.12") +
  annotate("text", family = "Tangerine", size = 12, x = 16, y = 5, label = "mean(x) = 9") +
  annotate("text", family = "Tangerine", size = 12, x = 16, y = 3, label = "var(x) = 7.5") +
  annotate("text", family = "Tangerine", size = 12, x = 18, y = 11.5, angle = 12, label = "y = 3 + 0.5x") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#f3ddc2", colour = "#f3ddc2"),
        plot.margin = margin(t = 10, r = 40, b = 10, l = 10),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        axis.title = element_text(family = "Tangerine", size = 50),
        axis.text = element_text(family = "Tangerine", size = 40))

p <- p1 + p2 + p3 + p4 +
  plot_annotation(
    title = "Anscombe's quartet (1973)",
    subtitle = "Nearly identical descriptive statistics but very different distributions",
    caption = "Visualisation: Jonathan Kitt | Data source: Wikipedia | #30DayChartChallenge 2022 | Day 3: historical",
    theme = theme(plot.title = element_text(family = "Tangerine", colour = "black", size = 120, hjust = 0.5,
                                            margin = margin(t = 20, b = 10)),
                  plot.subtitle = element_text(family = "Tangerine", colour = "black", size = 75, hjust = 0.5,
                                               margin = margin(b = 20)),
                  plot.background = element_rect(fill = "#f3ddc2", colour = "#f3ddc2"),
                  plot.caption = element_text(colour = "black", hjust = 0.5, size = 25)))

# Save plot ----

ggsave("2022/plots/finished/03_historical.png", p, dpi = 320, width = 12, height = 6)
