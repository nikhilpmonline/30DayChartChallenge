# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 14 : 3-dimensional
# Last updated 2022-03-18

# https://www.flerlagetwins.com/2019/08/ternary.html
# https://jserizay.com/blog/text_mining_and_sentiment_analysis_in_r/
# https://www.rayshader.com/reference/plot_gg.html

# Github contributions graph ----

d1 <- tibble(
  week = rep(1:13, each = 7),
  day = rep(7:1, times = 13),
  nb_contributions = c(0, 0, 6, 1, 2, 7, 0,
                       0, 0, 1, 0, 5, 6, 0,
                       0, 11, 7, 16, 11, 15, 0,
                       0, 20, 25, 8, 13, 21, 0,
                       0, 5, 15, 0, 0, 21, 0,
                       0, 13, 16, 17, 10, 20, 0,
                       0, 0, 0, 0, 0, 0, 0,
                       0, 5, 14, 1, 18, 0, 0,
                       0, 0, 11, 0, 2, 16, 0,
                       0, 21, 9, 19, 16, 14, 0,
                       0, 22, 9, 13, 24, 11, 0,
                       0, 13, 8, 24, 7, 7, 0,
                       0, 20, 8, 3, 0, 0, 0),
  level = c(0, 0, 1, 1, 1, 2, 0,
            0, 0, 1, 0, 1, 1, 0,
            0, 2, 2, 3, 2, 3, 0,
            0, 4, 4, 2, 3, 4, 0,
            0, 1, 2, 0, 0, 4, 0,
            0, 3, 3, 3, 2, 4, 0,
            0, 0, 0, 0, 0, 0, 0,
            0, 1, 3, 1, 3, 0, 0,
            0, 0, 2, 0, 1, 3, 0,
            0, 4, 2, 4, 3, 3, 0,
            0, 4, 2, 3, 4, 2, 0,
            0, 3, 2, 4, 2, 2, 0,
            0, 4, 2, 1, 0, 0, 0))

ggplot(data = d1,
       aes(x = week, y = day, fill = as.factor(level))) +
  geom_tile(colour = "white") +
  scale_fill_manual(values = c("#ebedf0", "#9be9a8", "#40c463", 
                               "#30a14e", "#216e39")) +
  theme(panel.background = element_rect(fill = "white", colour = "white"))

# Load packages ----

library(ggtern)
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
  mutate(bill_length_ratio = bill_length_mm / max(bill_length_mm, na.rm = TRUE),
         bill_depth_ratio = bill_depth_mm / max(bill_depth_mm, na.rm = TRUE),
         flipper_length_ratio = flipper_length_mm / max(flipper_length_mm, na.rm = TRUE)) %>% 
  select(species, island, bill_length_ratio, bill_depth_ratio, flipper_length_ratio) %>% 
  filter(!is.na(bill_depth_ratio))

# Create plot ----

p <- ggtern(data = d1, aes(x = bill_length_ratio, y = bill_depth_ratio, z = flipper_length_ratio)) +
  geom_point(aes(colour = species),
             show.legend = FALSE, size = 1) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_zoom_center(0.55) +
  annotate("text", x = 0.3, y = 0.5, z = 0.4, label = "Adelie", colour = "darkorange", family = "Righteous", size = 10) +
  annotate("text", x = 0.5, y = 0.5, z = 0.4, label = "Chinstrap", colour = "purple", family = "Righteous", size = 10) +
  annotate("text", x = 0.5, y = 0.5, z = 0.8, label = "Gentoo", colour = "cyan4", family = "Righteous", size = 10) +
  labs(x = "Bill length", y = "Bill depth", z = "Flipper length") +
  ggtitle("Palmer penguins",
          subtitle = "Clustering three species according to bill depth, bill length & flipper length") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#161b33", colour = "#161b33"),
        plot.background = element_rect(fill = "#161b33", colour = "#161b33"),
        axis.line = element_line(colour = "lightblue"),
        axis.title = element_text(family = "Righteous", colour = "lightblue", size = 22)) +
  theme_hidelabels() +
  theme_hideticks() +
  theme_hidegrid()

# Save plot ----
  
ggsave("2022/plots/work_in_progress/14_3dimensional_2.png", p, dpi = 320, width = 6, height = 6)
