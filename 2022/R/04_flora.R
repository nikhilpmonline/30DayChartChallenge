# 30DayChartChallenge
# 2022
# Category : Comparisons
# Day 4 : flora
# Last updated 2022-03-17

# https://en.wikipedia.org/wiki/List_of_tallest_trees
# https://www.treehugger.com/tallest-trees-world-4858795
# Load packages ----

library(tidyverse)
library(showtext)
library(patchwork)
library(rvest)

# Load fonts ----

font_add_google("Stick", "Stick")
showtext_auto()

# Import data ----

url <- "https://en.wikipedia.org/wiki/List_of_tallest_trees"

webpage <- rvest::read_html(url)

tables <- rvest::html_nodes(webpage, "table.wikitable") %>%
  rvest::html_table(header = TRUE, na.strings = c(NA, ""), convert = TRUE)

d1 <- tables[[1]]
d1 <- d1[2:nrow(d1), ]

# Data wrangling ----


names(d1) <- c("species", "height_m", "height_ft", "tree_name", "class", "location", "continent", "references")

d1 <- d1 %>% 
  mutate(taxonomy = str_extract(species, "\\([^()]+\\)")) %>% 
  mutate(species = str_remove(species, " \\([^()]+\\)")) %>% 
  mutate(taxonomy = substring(taxonomy, 2, nchar(taxonomy)-1)) %>% 
  select(species, taxonomy, tree_name, class, location, continent, height_m, height_ft) %>% 
  mutate(height_m = as.numeric(height_m),
         height_ft = as.numeric(height_ft)) %>% 
  arrange(desc(height_m)) %>% 
  mutate(species = fct_inorder(factor(species))) %>% 
  head(10)

# Test ----

top10 <- d1 %>% 
  arrange(desc(height_m)) %>% 
  head(10) %>% 
  mutate(height_m = as.numeric(height_m)) %>% 
  mutate(x.pos = seq(1, 40, 4))

ggplot() +
  geom_segment(data = top10,
               aes(x = species, xend = species,
                   y = 0, yend = height_m)) +
  geom_circle(data = top10,
            aes(x0 = 1:10, y0 = height_m, r = 1)) +
  coord_fixed()

ggplot() +
  geom_segment(data = top10,
               aes(x = species, xend = species,
                   y = 80, yend = height_m),
               arrow = arrow(length = unit(1, "cm"), type = "closed")) +
  geom_segment(data = top10,
               aes(x = species, xend = species,
                   y = 80, yend = height_m - 0.5),
               arrow = arrow(length = unit(2, "cm"), type = "closed")) +
  geom_segment(data = top10,
               aes(x = species, xend = species,
                   y = 80, yend = height_m - 1.5),
               arrow = arrow(length = unit(4, "cm"), type = "closed")) +
  geom_segment(data = top10,
               aes(x = species, xend = species,
                   y = 80, yend = height_m - 2),
               size = 6)

ggplot() +
  geom_segment(aes(x = 0, xend = 0,
                   y = 0, yend = 10),
               arrow = arrow(length = unit(3, "cm"),
                             type = "closed"),
               colour = "darkgreen") +
  geom_segment(aes(x = 0, xend = 0,
                   y = 0, yend = 9),
               arrow = arrow(length = unit(4, "cm"),
                             type = "closed"),
               colour = "darkgreen") +
  geom_segment(aes(x = 0, xend = 0,
                   y = 0, yend = 8),
               arrow = arrow(length = unit(6, "cm"),
                             type = "closed"),
               colour = "darkgreen") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 5),
               colour = "darkgreen", size = 5)

ggplot() +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 0.5),
               arrow = arrow(length = unit(5, "cm"),
                             type = "closed"))
  
  
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 8:10),
               arrow = arrow(length = unit(5, "cm"),
                             type = "closed"))
geom_segment(aes(x = 0, xend = 0, y = 0, yend = 8:10),
             arrow = arrow(length = unit(5, "cm"),
                           type = "closed"))

# Create plot ----

wrld2 <- st_as_sf(map("world2", plot = FALSE, fill = TRUE))

p1 <- ggplot() +
  geom_sf(data = wrld2, fill = "#66bb6a", col = "#66bb6a") +
  coord_sf(xlim = c(80, 300), ylim = c(-50, 65)) +
  geom_point(aes(x = 147, y = -42), size = 4, colour = "white") +
  geom_text(aes(x = 147, y = -47), size = 25, label = "Tasmania", family = "Stick", colour = "white") +
  geom_point(aes(x = 239, y = 38), size = 4, colour = "white") +
  geom_text(aes(x = 222, y = 38), size = 25, label = "California", family = "Stick", colour = "white") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#a5d6a7", colour = "#a5d6a7"),
        plot.background = element_rect(fill = "#a5d6a7", colour = "#a5d6a7"))

ggsave("2022/plots/04_flora_1.png", p1, dpi = 320, width = 12, height = 6)

p2 <- ggplot() +
  geom_col(data = flowering_plants,
           aes(x = species, y = height_m),
           fill = "darkgreen", width = 0.75) +
  geom_text(data = flowering_plants,
            aes(x = species, y = height_m, label = height_m),
            vjust = -0.5) +
  geom_text(data = flowering_plants,
            aes(x = species, y = 10, label = species),
            angle = 90, hjust = 0, colour = "white") +
  ggtitle("Ten tallest flowering plants") +
  theme_minimal() +
  theme(plot.title = element_text(family = "Stick", size = 25),
        axis.title = element_blank(),
        axis.text = element_blank())

ggsave("2022/plots/04_flora_2.png", p2, dpi = 320, width = 12, height = 6)

p <- p1 + p2 +
  plot_layout(ncol = 1)
ggsave("2022/plots/04_flora.png", p, dpi = 320, width = 12, height = 6)




d1 <- tibble(
  name = c("King Stringy", "Alpine Ash", "Neeminah Loggorale Meena", "White Knight", "Yellow Meranti",
           "Unnamed Giant Sequoia", "Raven's Tower", "Doerner Fir", "Centurion", "Hyperion"),
  taxonomy = c("Eucalyptus obliqua", "Eucalyptus delegatensis", "Eucalyptus globulus", "Eucalyptus viminalis", "Shorea faguetiana",
               "Sequoiadendron giganteum", "Picea sitchensis", "Pseudotsuga menziesii", "Eucalyptus regnans", "Sequoia sempervirens"),
  place = c("Tasmania", "Tasmania", "Tasmania", "Tasmania", "Borneo", 
            "California", "California", "Oregon", "Tasmania", "California"),
  height_feet = c(282, 288, 298, 301, 309, 314, 317, 327, 327.5, 380.1))

d1 %>% arrange(desc(height_feet))

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
  scale_x_continuous(limits = c(4, 19), breaks = seq(4, 18, 2)) +
  scale_y_continuous(limits = c(4, 14), breaks = seq(4, 12, 2)) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13,
               colour = "#000000", size = 0.05) +
  geom_point(aes(x = x, y = y),
             size = 2, colour = "#ffa500") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#f3ddc2", colour = "#f3ddc2"),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        axis.title = element_text(family = "Tangerine", size = 50),
        axis.text = element_text(family = "Tangerine", size = 40))
  
p2 <- ggplot(data = d1 %>% filter(quartet_nb == 2)) +
  scale_x_continuous(limits = c(4, 19), breaks = seq(4, 18, 2)) +
  scale_y_continuous(limits = c(4, 14), breaks = seq(4, 12, 2)) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13,
               colour = "#000000", size = 0.05) +
  geom_point(aes(x = x, y = y),
             size = 2, colour = "#0099ff") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#f3ddc2", colour = "#f3ddc2"),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        axis.title = element_text(family = "Tangerine", size = 50),
        axis.text = element_text(family = "Tangerine", size = 40))

p3 <- ggplot(data = d1 %>% filter(quartet_nb == 3)) +
  scale_x_continuous(limits = c(4, 19), breaks = seq(4, 18, 2)) +
  scale_y_continuous(limits = c(4, 14), breaks = seq(4, 12, 2)) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13,
               colour = "#000000", size = 0.05) +
  geom_point(aes(x = x, y = y),
             size = 2, colour = "#009e73") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#f3ddc2", colour = "#f3ddc2"),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        axis.title = element_text(family = "Tangerine", size = 50),
        axis.text = element_text(family = "Tangerine", size = 40))

p4 <- ggplot(data = d1 %>% filter(quartet_nb == 4)) +
  scale_x_continuous(limits = c(4, 19), breaks = seq(4, 18, 2)) +
  scale_y_continuous(limits = c(4, 14), breaks = seq(4, 12, 2)) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13,
               colour = "#000000", size = 0.05) +
  geom_point(aes(x = x, y = y),
             size = 2, colour = "#b32db5") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#f3ddc2", colour = "#f3ddc2"),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        axis.title = element_text(family = "Tangerine", size = 50),
        axis.text = element_text(family = "Tangerine", size = 40))

p <- p1 + p2 + p3 + p4 +
  plot_annotation(
    title = "Anscombe's quartet",
    subtitle = "Four datasets with nearly identical descriptive statistics but very different distributions",
    caption = "Visualisation: Jonathan Kitt | Data source: www.cyclinglocations.com | #30DayChartChallenge 2022 | Day 3: historical",
    theme = theme(plot.title = element_text(family = "Tangerine", colour = "black", size = 120, hjust = 0.5,
                                            margin = margin(t = 20, b = 10)),
                  plot.subtitle = element_text(family = "Tangerine", colour = "black", size = 75, hjust = 0.5,
                                               margin = margin(b = 20)),
                  plot.background = element_rect(fill = "#f3ddc2", colour = "#f3ddc2"),
                  plot.caption = element_text(colour = "black", hjust = 0.5, size = 25)))

ggsave("2022/plots/03_historical.png", p, dpi = 320, width = 12, height = 6)
