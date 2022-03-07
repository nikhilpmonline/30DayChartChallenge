# 30DayChartChallenge
# 2022
# Category : Comparisons
# Day 2 : Pictogram
# Last updated 2022-03-07

# https://gs.statcounter.com/os-market-share/mobile/worldwide

# Load packages ----

library(tidyverse)
library(showtext)
library(ggwaffle)
library(emojifont)
library(patchwork)

# Load fonts ----

font_add_google("Rajdhani", "Rajdhani")
showtext_auto()

# Data wrangling ----

d1 <- tibble(
  os = c(rep("Android", 71), rep("iOS", 28), "Other"))

waffle_d1 <- waffle_iron(d1, aes_d(group = os), rows = 10) %>% 
  mutate(label = fontawesome(c(rep("fa-android", 71), rep("fa-apple", 28), "fa-question")))

# Create plot ----

p1 <- ggplot() +
  annotate("text", label = "Mobile OS market",
           x = 0, y = 0.25, family = "Rajdhani", size = 50, colour = "white") +
  annotate("text", label = "share worldwide",
           x = 0, y = -0.25, family = "Rajdhani", size = 50, colour = "white") +
  ylim(c(-1, 1)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#332859", colour = "#332859"),
        plot.background = element_rect(fill = "#332859", colour = "#332859"))

p2 <- ggplot(waffle_d1, aes(x, -y)) + 
  geom_text(aes(label = label, colour = group),
            family='fontawesome-webfont', size = 35,
            show.legend = FALSE) +
  coord_equal() + 
  scale_colour_manual(values = c("#a4c739", "#dadada", "#0fbfae")) + 
  theme_waffle() +
  theme_void() +
  theme(panel.background = element_rect(fill = "#332859", colour = "#332859"),
        plot.background = element_rect(fill = "#332859", colour = "#332859"),
        plot.margin = margin(t = 40, r = 40, b = 40, l = 40))

p3 <- p1 + p2 +
  plot_annotation(
    caption = "Visualisation: Jonathan Kitt | Data source: https://gs.statcounter.com/os-market-share/mobile/worldwide | #30DayChartChallenge 2022 | Day 2: pictogram",
    theme = theme(plot.background = element_rect(fill = "#332859"),
                  plot.caption = element_text(colour = "white", hjust = 0.5, size = 25)))

ggsave("2022/plots/02_pictogram.png", p3, dpi = 320, width = 12, height = 6)