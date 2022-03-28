# 30DayChartChallenge
# 2022
# Category : Comparisons
# Day 4 : flora
# Last updated 2022-03-28

# https://en.wikipedia.org/wiki/List_of_tallest_trees
# https://www.treehugger.com/tallest-trees-world-4858795

# Load packages ----

library(ggforce)
library(patchwork)
library(rvest)
library(showtext)
library(tidyverse)

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

names(d1) <- c("species", "height_m", "height_ft", "tree_name", "class",
               "location", "continent", "references")

d1 <- d1 %>% 
  mutate(taxonomy = str_extract(species, "\\([^()]+\\)")) %>% 
  mutate(species = str_remove(species, " \\([^()]+\\)")) %>% 
  mutate(taxonomy = substring(taxonomy, 2, nchar(taxonomy)-1)) %>% 
  select(species, taxonomy, tree_name, class, location, continent, height_m, height_ft) %>% 
  mutate(height_m = as.numeric(height_m),
         height_ft = as.numeric(height_ft)) %>% 
  arrange(desc(height_m)) %>% 
  mutate(species = fct_inorder(factor(species))) %>% 
  head(10) %>% 
  rowid_to_column() %>% 
  mutate(place = c("California", "Island of Borneo", "Tasmania", "Oregon", "California",
                   "California", "Tasmania", "Tasmania", "Washington", "Brazil"))

# Create plot ----

p1 <- ggplot() +
  geom_segment(data = d1,
               aes(x = rowid, xend = rowid,
                   y = 0, yend = height_m - 5),
               size = 2, colour = "#a67b51") +
  geom_ellipse(data = d1,
               aes(x0 = rowid, y0 = height_m - 40,
                   a = 0.3, b = 40, angle = 0),
               fill = "#0cae5b", colour = "#0cae5b") +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = seq(80, 120, 10)) +
  ggtitle("Height in meters") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#a5d6a7", colour = "#a5d6a7"),
        plot.background = element_rect(fill = "#a5d6a7", colour = "#a5d6a7"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "white", linetype = "dotted"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(family = "Stick", size = 25, colour = "#263e31"),
        axis.text.y = element_text(family = "Stick", size = 20, colour = "#263e31"),
        plot.title = element_text(hjust = -0.025,
                                  margin = margin(t = 20, b = 10, l = 20),
                                  family = "Stick", colour = "#263e31", size = 40))

p2 <- ggplot() +
  xlim(0, 10) +
  geom_point(data = d1 %>% filter(rowid %in% 1:5),
             aes(x = 2, y = rowid),
             size = 10, colour = "#263e31") +
  geom_point(data = d1 %>% filter(rowid %in% 6:10),
             aes(x = 6, y = rowid - 5),
             size = 10, colour = "#263e31") +
  geom_text(data = d1 %>% filter(rowid %in% 1:5),
            aes(x = 2, y = rowid, label = 5:1),
            family = "Stick", colour = "white", hjust = 0.5, size = 10) +
  geom_text(data = d1 %>% filter(rowid %in% 6:10),
            aes(x = 6, y = rowid - 5, label = 10:6),
            family = "Stick", colour = "white", hjust = 0.5, size = 10) +
  geom_text(data = d1 %>% filter(rowid %in% 1:5),
            aes(x = 2.5, y = rev(rowid) + 0.25, label = species),
            family = "Stick", colour = "#263e31", hjust = 0, size = 15) +
  geom_text(data = d1 %>% filter(rowid %in% 6:10),
            aes(x = 6.5, y = rev(rowid) - 5 + 0.25, label = species),
            family = "Stick", colour = "#263e31", hjust = 0, size = 15) +
  geom_text(data = d1 %>% filter(rowid %in% 1:5),
            aes(x = 2.5, y = rev(rowid), label = paste0(height_m, " m")),
            family = "Stick", colour = "#0cae5b", hjust = 0, size = 15) +
  geom_text(data = d1 %>% filter(rowid %in% 6:10),
            aes(x = 6.5, y = rev(rowid) - 5, label = paste0(height_m, " m")),
            family = "Stick", colour = "#0cae5b", hjust = 0, size = 15) +
  geom_text(data = d1 %>% filter(rowid %in% 1:5),
            aes(x = 2.5, y = rev(rowid) - 0.25, label = place),
            family = "Stick", colour = "#263e31", hjust = 0, size = 15) +
  geom_text(data = d1 %>% filter(rowid %in% 6:10),
            aes(x = 6.5, y = rev(rowid) - 5 - 0.25, label = place),
            family = "Stick", colour = "#263e31", hjust = 0, size = 15) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#a5d6a7", colour = "#a5d6a7"),
        plot.background = element_rect(fill = "#a5d6a7", colour = "#a5d6a7"))
  

p <- p1 + p2 +
  plot_annotation(
    title = "The ten tallest trees",
    caption = "Visualisation: Jonathan Kitt | Data source: Wikipedia | #30DayChartChallenge 2022 | Day 4: flora",
    theme = theme(plot.title = element_text(family = "Stick", colour = "#263e31", size = 120, hjust = 0.5,
                                            margin = margin(t = 20, b = 10)),
                  plot.background = element_rect(fill = "#a5d6a7", colour = "#a5d6a7"),
                  plot.caption = element_text(colour = "#263e31", hjust = 0.5, size = 25)))

# Save plot ----

ggsave("2022/plots/finished/04_flora.png", p, dpi = 320, width = 12, height = 6)
