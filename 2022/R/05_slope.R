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

# font_add_google("Preahvihear", "Preahvihear")
font_add_google("Snippet", "Snippet")
showtext_auto()

# Import data ----

mortirolo_pass <- plotKML::readGPX(gpx.file = "2022/data/Climb Alps - Mortirolo Pass from Mazzo di Valtellina.gpx")

# Data wrangling ----

d1 <- mortirolo_pass$tracks[[1]][[1]] %>% 
  as_tibble() %>% 
  rename(longitude = lon,
         latitude = lat,
         elevation = ele) %>% 
  mutate(elevation = as.numeric(elevation),
         distance = distHaversine(cbind(longitude, latitude),
                              cbind(lag(longitude), lag(latitude)))) %>% 
  mutate(distance = ifelse(is.na(distance), 0, distance)) %>% 
  mutate(distance_from_start = cumsum(distance)) %>% 
  select(-distance) %>% 
  mutate(km_nb = cut_interval(distance_from_start, length = 1000, labels = FALSE))

data_by_km <- d1 %>% 
  group_by(km_nb) %>% 
  filter(row_number() == 1 | row_number() == n()) %>% 
  ungroup() %>% 
  mutate(group = c(1, rep(2:12, each = 2), 13)) %>% 
  group_by(group) %>% 
  mutate(new_y = min(elevation)) %>% 
  ungroup()

rectangles <- tibble(
  km_nb = 1:12,
  x.min = seq(0, 11e3, 1e3),
  x.max = c(seq(1e3, 11e3, 1e3), max(data_by_km$distance_from_start)),
  y.min = 0,
  y.max = unique(data_by_km$new_y[data_by_km$km_nb != 12]))

triangles <- tibble(
  km_nb = rep(1:12, each = 3),
  point_nb = rep(1:3, times = 12),
  x = c(0, rep(seq(1e3, 11e3, 1e3), each = 3), rep(max(d1$distance_from_start), times = 2)),
  y = c(rep(min(data_by_km$new_y[1]), 2),
        rep(unique(data_by_km$new_y)[2:12], each = 3),
        max(data_by_km$new_y)))

slopes <- rectangles %>% 
  mutate(x.lab = x.min + ((x.max - x.min) / 2)) %>% 
  mutate(elevation = c(y.max[-1], max(data_by_km$elevation))) %>% 
  mutate(slope_pct = (elevation - y.max) / 10) %>% 
  select(km_nb, x.min, x.max, x.lab, slope_pct) %>% 
  mutate(slope_colour = case_when(slope_pct <= 2.9 ~ "green",
                                  slope_pct > 2.9 & slope_pct <= 5.9 ~ "blue",
                                  slope_pct > 5.9 & slope_pct <= 8.9 ~ "red",
                                  slope_pct > 8.9 ~ "black"))

segments <- triangles %>% 
  group_by(km_nb) %>% 
  filter(row_number() == 1) %>% 
  filter(km_nb != 1)

slopes

p <- ggplot() +
  xlim(-200, 13000) +
  geom_segment(aes(x = 0, xend = 12300,
                   y = seq(500, 1800, 100), yend = seq(500, 1800, 100)),
               linetype = "dotted", colour = "white", size = 0.2) +
  geom_text(aes(x = 12400, y = seq(500, 1800, 100),
                label = seq(500, 1800, 100)),
            family = "Snippet", size = 15, colour = "white", hjust = 0) +
  geom_rect(data = rectangles,
            aes(xmin = x.min, xmax = x.max,
                ymin = y.min, ymax = y.max),
            fill = "#f6cc49", colour = "#f6cc49") +
  geom_polygon(data = triangles,
               aes(x = x, y = y, group = km_nb),
               fill = "#f6cc49", colour = "#f6cc49") +
  geom_rect(data = slopes,
            aes(xmin = x.min, xmax = x.max,
                ymin = -100, ymax = 0,
                fill = slope_colour),
            colour = "white",
            show.legend = FALSE) +
  scale_fill_manual(values = c("#141307", "#e6010c", "#024f93", "#81bb21")) +
  geom_text(data = slopes,
            aes(x = x.lab, y = -50, label = round(slope_pct, digits = 1)),
            colour = "white", family = "Snippet", size = 15) +
  annotate("text", x = 12000, y = -50, label = "slope %", colour = "white", size = 15, family = "Snippet", hjust = 0) +
  annotate("text", x = 12000, y = -150, label = "km", colour = "white", size = 15, family = "Snippet", hjust = 0) +
  geom_rect(data = slopes,
            aes(xmin = x.min, xmax = x.max,
                ymin = -200, ymax = -100),
            fill = "grey40", colour = "white",
            show.legend = FALSE) +
  geom_text(data = slopes,
            aes(x = x.lab, y = -150, label = km_nb),
            colour = "white", family = "Snippet", size = 15) +
  geom_segment(data = segments,
               aes(x = x, xend = x,
                   y = 100, yend = y),
               linetype = "dashed", colour = "white") +
  scale_y_continuous(position = "right",
                     breaks = seq(500, 1800, 100)) +
  geom_segment(aes(x = 0, xend = 0,
                   y = 0, yend = 1750),
               colour = "white") +
  annotate("text", x = -150, y = 1200, label = "537m - MAZZO IN VALTELLINA", angle = 90,
           family = "Snippet", colour = "white", size = 15) +
  geom_segment(aes(x = max(data_by_km$distance_from_start), xend = max(data_by_km$distance_from_start),
                   y = 0, yend = 2000),
               colour = "white") +
  geom_segment(aes(x = 8350, xend = max(data_by_km$distance_from_start),
                   y = 2000, yend = 2000),
               colour = "white") +
  annotate("text", x = 10000, y = 2050, label = "1854m - PASSO DEL MORTIROLO",
           family = "Snippet", colour = "white", size = 15) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#355c7d", colour = "#355c7d"),
        plot.background = element_rect(fill = "#355c7d", colour = "#355c7d"),
        panel.grid = element_blank(),
        # panel.grid.major.x = element_blank(),
        # panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

ggsave("2022/plots/05_slope.png", p, dpi = 320, width = 12, height = 6)
