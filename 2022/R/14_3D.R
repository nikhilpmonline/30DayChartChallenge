# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 14 : 3-dimensional
# Last updated 2022-03-18

# https://www.flerlagetwins.com/2019/08/ternary.html
# https://jserizay.com/blog/text_mining_and_sentiment_analysis_in_r/
# https://www.rayshader.com/reference/plot_gg.html

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
  ggtitle("How to differenciate penguins",
          subtitle = "Visualisation: Jonathan Kitt | Data source: Palmer Penguins | #30DayChartChallenge 2022 | Day 8: 3-dimensional") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#161b33", colour = "#161b33"),
        plot.background = element_rect(fill = "#161b33", colour = "#161b33"),
        axis.line = element_line(colour = "lightblue"),
        axis.title = element_text(family = "Righteous", colour = "lightblue", size = 22),
        plot.title = element_text(family = "Righteous", colour = "lightblue", size = 75, hjust = 0.5,
                                  margin = margin(t = 10)),
        plot.subtitle = element_text(family = "Righteous", colour = "lightblue", size = 20, hjust = 0.5)) +
  theme_hidelabels() +
  theme_hideticks() +
  theme_hidegrid()

# Save plot ----
  
ggsave("2022/plots/finished/14_3dimensional.png", p, dpi = 320, width = 12, height = 6)
