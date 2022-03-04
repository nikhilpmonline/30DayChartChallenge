# 30DayChartChallenge
# 2022
# Category : Comparisons
# Day 2 : Pictogram
# Last updated 2022-03-04

# Load packages ----

library(tidyverse)
library(datardis)
library(ggimage)
# library(showtext)
# library(patchwork)

# Load fonts ----

# font_add_google("Lobster", "Lobster")
# showtext_auto()

# Import data ----

episodes <- datardis::episodes

# Data wrangling ---- 

d1 <- episodes %>% 
  mutate(season_number = case_when(story_number %in% c("199", "200", "201", "202a", "202b") ~ 4.5,
                                   story_number %in% c("240", "241") ~ 7.5,
                                   TRUE ~ season_number)) %>% 
  mutate(group = cumsum(!duplicated(season_number))) %>% 
  select(group, season_number, episode_title, type) %>% 
  mutate(season_name = case_when(group == "5" ~ "2008-2010 specials",
                                 group == "9" ~ "2013 specials",
                                 group == "15" ~ "Season 13 (Flux)",
                                 TRUE ~ paste("Season", as.integer(season_number), sep = " "))) %>% 
  group_by(season_name) %>% 
  mutate(id = row_number()) %>% 
  select(season_name, id, type) %>% 
  mutate(img = "2022/data/tardis.png")

ggplot(data = d1) +
  geom_image(aes(x = id,
                 y = season_name,
                 image = img),
             by = "width", size = 0.025) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#a6b8c7", colour = "#a6b8c7"),
        plot.background = element_rect(fill = "#a6b8c7", colour = "#a6b8c7"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank())

ggsave("2022/plots/02_pictogram.png", p, dpi = 320, width = 12, height = 6)

# d1 <- munros %>% 
#   mutate(unique_name = paste(1:nrow(.), name, sep = " - ")) %>% 
#   select(unique_name, county) %>% 
#   separate_rows(county, sep = ",") %>% 
#   mutate(county = factor(str_trim(county))) %>% 
#   count(county)
# 
# highland_munros <- tibble(
#   county = c("Highland", "Other"),
#   n = c(d1$n[d1$county == "Highland"],
#         nrow(munros) - d1$n[d1$county == "Highland"])) %>% 
#   mutate(ratio = n / sum(n)) %>% 
#   mutate(ymin = c(0, max(ratio)),
#          ymax = c(max(ratio), 1))

# Create plot ----

p1 <- ggplot() +
  annotate("text", x = -0.5, y = 1, label = "Munros are mountains in Scotland above 3,000 feet",
           colour = "white", family = "Lobster", size = 20) +
  annotate("text", x = 0.5, y = -1, label = "Out of 282 Munros, 186 are located in the Highlands",
           colour = "white", family = "Lobster", size = 20) +
  xlim(-15, 15) +
  ylim(-5, 5) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#041f32", colour = "#041f32"),
        plot.background = element_rect(fill = "#041f32", colour = "#041f32"))

p2 <- ggplot(data = highland_munros,
             aes(x = 6, y = ratio, fill = county)) +
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  scale_fill_manual(values = c("#04aed9", "#c5e0f5")) +
  coord_polar(theta = "y", start = 0) +
  xlim(c(0.025, 8)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#041f32", colour = "#041f32"),
        plot.background = element_rect(fill = "#041f32", colour = "#041f32"))

p <- p1 + p2 +
  plot_annotation(
    title = "How many Munros are located in the Highlands?",
    caption = "Visualisation: Jonathan Kitt | Data source: munro package | #30DayChartChallenge 2022 | Day 1: part-to-whole",
    theme = theme(plot.title = element_text(family = "Lobster", colour = "white", size = 100, hjust = 0.5,
                                            margin = margin(t = 40)),
                  plot.background = element_rect(fill = "#041f32"),
                  plot.caption = element_text(colour = "white", hjust = 0.5, size = 25)))

ggsave("2022/plots/01_part_to_whole.png", p, dpi = 320, width = 12, height = 6)
