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

cut_interval(d1$distance_from_start, length = 1000)

plot(d1$distance_from_start, d1$elevation)

d1

%>% 
  mutate(dist = ifelse(is.na(dist), 0, dist)) %>% 
  mutate(cum_dist = cumsum(dist)) %>% 
  mutate(km = case_when(cum_dist <= 1000 ~ 1,
                        cum_dist > 1000 & cum_dist <= 2000 ~ 2,
                        cum_dist > 2000 & cum_dist <= 3000 ~ 3,
                        cum_dist > 3000 & cum_dist <= 4000 ~ 4,
                        cum_dist > 4000 & cum_dist <= 5000 ~ 5,
                        cum_dist > 5000 & cum_dist <= 6000 ~ 6,
                        cum_dist > 6000 & cum_dist <= 7000 ~ 7,
                        cum_dist > 7000 & cum_dist <= 8000 ~ 8,
                        cum_dist > 8000 & cum_dist <= 9000 ~ 9,
                        cum_dist > 9000 & cum_dist <= 10000 ~ 10,
                        cum_dist > 10000 & cum_dist <= 11000 ~ 11,
                        cum_dist > 11000 & cum_dist <= 12000 ~ 12)) %>% 
  group_by(km) %>% 
  mutate(km_slope_pct = 0.1 * (max(ele) - min(ele))) %>% 
  mutate(slope_colour = case_when(km_slope_pct <= 2.9 ~ "green",
                                  km_slope_pct > 2.9 & km_slope_pct <= 5.9 ~ "blue",
                                  km_slope_pct > 5.9 & km_slope_pct <= 8.9 ~ "red",
                                  km_slope_pct > 8.9 ~ "black")) %>% 
  mutate(km.x.min = (km * 1000) - 1000,
         km.x.max = case_when(km == 12 ~ max(cum_dist),
                              TRUE ~ 1000 * km),
         km.y.min = 0,
         km.y.max = min(ele)) %>% 
  ungroup()

rectangles <- d1 %>% 
  group_by(km) %>% 
  filter(row_number() == 1) %>% 
  select(km, km.x.min:km.y.max) %>% 
  ungroup()

triangles <- d1 %>% 
  group_by(km) %>% 
  mutate(ele.min = min(ele), ele.max = max(ele)) %>% 
  select(km, km.x.min, km.x.max, ele.min, ele.max) %>% 
  filter(row_number() == 1)

triangles.x <- list()
triangles.y <- list()

for (i in 1:nrow(triangles)) {
  
  triangles.x[[i]] <- c(triangles$km.x.min[triangles$km == i],
                        triangles$km.x.max[triangles$km == i],
                        triangles$km.x.max[triangles$km == i])
  
  triangles.y[[i]] <- c(triangles$ele.min[triangles$km == i],
                        triangles$ele.min[triangles$km == i],
                        triangles$ele.max[triangles$km == i])
  
}

triangles <- tibble(
  triangle = rep(1:12, each = 3),
  x = unlist(triangles.x),
  y = unlist(triangles.y))

rm(triangles.x, triangles.y, i)

slopes <- d1 %>% 
  group_by(km) %>% 
  filter(row_number() == 1) %>% 
  mutate(lab.x = km.x.min + (km.x.max - km.x.min) / 2) %>% 
  select(km, km_slope_pct, slope_colour, km.x.min, km.x.max, lab.x) %>% 
  ungroup()

p <- ggplot() +
  geom_rect(data = rectangles,
            aes(xmin = km.x.min, xmax = km.x.max,
                ymin = km.y.min, ymax = km.y.max),
            fill = "#f6cc49", colour = "#f6cc49") +
  geom_polygon(data = triangles,
               aes(x = x, y = y, group = triangle),
               fill = "#f6cc49", colour = "#f6cc49") +
  geom_line(data = d1,
             aes(x = cum_dist, y = ele),
             colour = "#f6cc49", size = 4) +
  geom_rect(data = slopes,
            aes(xmin = km.x.min, xmax = km.x.max,
                ymin = -100, ymax = 0,
                fill = slope_colour, colour = slope_colour),
            show.legend = FALSE) +
  scale_fill_manual(values = c("#141307", "#e6010c", "#024f93", "#81bb21")) +
  scale_colour_manual(values = c("#141307", "#e6010c", "#024f93", "#81bb21")) +
  geom_text(data = slopes,
            aes(x = lab.x, y = -50, label = km_slope_pct),
            colour = "white")

ggsave("2022/plots/05_slope.png", p, dpi = 320, width = 12, height = 6)


km_slopes <- profile %>% 
  group_by(km) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  mutate(label.x = km.x.min + ((km.x.max - km.x.min)/2))


km_slopes

triangles <- profile %>% 
  group_by(km) %>% 
  mutate(ele.min = min(ele), ele.max = max(ele)) %>% 
  select(km, km.x.min, km.x.max, ele.min, ele.max) %>% 
  filter(row_number() == 1)

triangles.x <- list()
triangles.y <- list()

for (i in 1:nrow(triangles)) {
  
  triangles.x[[i]] <- c(triangles$km.x.min[triangles$km == i],
                   triangles$km.x.max[triangles$km == i],
                   triangles$km.x.max[triangles$km == i])
  
  triangles.y[[i]] <- c(triangles$ele.min[triangles$km == i],
                   triangles$ele.min[triangles$km == i],
                   triangles$ele.max[triangles$km == i])
  
}

triangles.data <- tibble(
  triangle = rep(1:12, each = 3),
  x = unlist(triangles.x),
  y = unlist(triangles.y),
  colour = rep(km_slopes$slope_colour, each = 3)
)

profile %>% 
  group_by(km) %>% 
  filter(row_number() == 1)

p <- ggplot() +
  geom_rect(data = profile,
            aes(xmin = km.x.min, xmax = km.x.max,
                ymin = km.y.min, ymax = km.y.max,
                fill = slope_colour),
            show.legend = FALSE) +
  geom_polygon(data = triangles.data,
               aes(x = x, y = y, group = triangle,
                   fill = colour),
               show.legend = FALSE) +
  scale_fill_manual(values = c("#141307", "#e6010c", "#024f93", "#81bb21")) +
  geom_line(data = profile,
             aes(x = cum_dist, y = ele),
            size = 3, colour = "white") +
  scale_colour_manual(values = c("#141307", "#e6010c", "#024f93", "#81bb21")) +
  geom_segment(data = triangles,
               aes(x = km.x.max, xend = km.x.max,
                   y = 0, yend = ele.max),
               size = 0.5, colour = "white", linetype = "dashed") +
  geom_text(data = km_slopes,
            aes(x = label.x, y = 100, label = km),
            colour = "white") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#355c7d", colour = "#355c7d"),
        plot.background = element_rect(fill = "#355c7d", colour = "#355c7d"),
        panel.grid = element_blank())


ggsave("2022/plots/05_slope.png", p, dpi = 320, width = 12, height = 6)


d=data.frame(x=c(1,2,2, 3,4,4), y=c(1,1,2, 2,2,3), t=c('a', 'a', 'a',  'b', 'b', 'b'), r=c(1,2,3, 4,5,6))
ggplot() +
  geom_polygon(data=d, mapping=aes(x=x, y=y, group=t)) +
  geom_point(data=d, aes(x=x, y=y, color=t)) +
  geom_text(data=d, aes(x=x, y=y, label=r), hjust=0, vjust=1, size=4)
  #opts(title="geom_polygon", plot.title=theme_text(size=40, vjust=1.5))


slopes <- profile %>%
  mutate(km = case_when(cum_dist <= 1000 ~ 1,
                        cum_dist > 1000 & cum_dist <= 2000 ~ 2,
                        cum_dist > 2000 & cum_dist <= 3000 ~ 3,
                        cum_dist > 3000 & cum_dist <= 4000 ~ 4,
                        cum_dist > 4000 & cum_dist <= 5000 ~ 5,
                        cum_dist > 5000 & cum_dist <= 6000 ~ 6,
                        cum_dist > 6000 & cum_dist <= 7000 ~ 7,
                        cum_dist > 7000 & cum_dist <= 8000 ~ 8,
                        cum_dist > 8000 & cum_dist <= 9000 ~ 9,
                        cum_dist > 9000 & cum_dist <= 10000 ~ 10,
                        cum_dist > 10000 & cum_dist <= 11000 ~ 11,
                        cum_dist > 11000 & cum_dist <= 12000 ~ 12)) %>% 
  
  group_by(km) %>% 
  mutate(slope_pct = 0.1 * (max(ele) - min(ele))) %>% 
  filter(ele == max(ele)) %>% 
  filter(row_number() == 1) %>% 
  select(km, ele, slope_pct) %>% 
  mutate(x = case_when(km == 12 ~ max(profile$cum_dist),
                       TRUE ~ 1000 * km))

slopes


ggplot(data = test) +
  geom_segment(aes(x = x.min, xend = x.max,
                   y = y.min, yend = y.max))

ggplot() +
  geom_point(data = profile,
             aes(x = cum_dist, y = ele, colour = slope_colour))

+
  geom_segment(data = slopes,
               aes(x = x, xend = x, y = 0, yend = ele)) +
  geom_rect(data = slopes, aes(xmin = ))
  

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
