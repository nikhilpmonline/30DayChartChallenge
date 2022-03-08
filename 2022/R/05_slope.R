# 30DayChartChallenge
# 2022
# Category : Comparisons
# Day 5 : Slope
# Last updated 2022-03-08

# https://www.cyclinglocations.com/mortirolo-pass-mazzo
# https://scriptsandstatistics.wordpress.com/2018/03/29/how-to-plot-gps-data-using-r-ggplot2-and-ggmaps/
# https://stackoverflow.com/questions/49532911/calculate-distance-longitude-latitude-of-multiple-in-dataframe-r

# Load packages ----

library(tidyverse)
library(showtext)
library(patchwork)
library(plotKML)
library(geosphere)

# Load fonts ----

# font_add_google("Rajdhani", "Rajdhani")
# showtext_auto()

# Import data ----

d1 <- readGPX("../Desktop/Climb Alps - Mortirolo Pass from Mazzo di Valtellina.gpx")

d2 <- d1$tracks[[1]][[1]]

d2 <- as_tibble(d2) %>% 
  mutate(dist = distHaversine(cbind(lon, lat),
                              cbind(lag(lon), lag(lat)))) %>% 
  mutate(dist = ifelse(is.na(dist), 0, dist)) %>% 
  mutate(cum_dist = cumsum(dist))

plot(d2$cum_dist, d2$ele)

ggplot(d2, aes(x = lon, y = lat)) +
  coord_quickmap() +
  geom_point()

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
    theme = theme(plot.background = element_rect(fill = "#332859", colour = "#332859"),
                  plot.caption = element_text(colour = "white", hjust = 0.5, size = 25)))

ggsave("2022/plots/02_pictogram.png", p3, dpi = 320, width = 12, height = 6)
