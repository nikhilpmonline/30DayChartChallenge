# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 12 : Theme day - The Economist
# Last updated 2022-03-18

# https://jrnold.github.io/ggthemes/reference/theme_economist.html
# https://www.economist.com/big-mac-index

# Load packages ----

library(tidyverse)
library(showtext)
library(patchwork)
library(lubridate)
library(ggthemes)

# Load fonts ----

# font_add_google("Tangerine", "Tangerine")
# showtext_auto()

# Download data ----

# d1 <- read_csv("https://raw.githubusercontent.com/TheEconomist/big-mac-data/master/output-data/big-mac-adjusted-index.csv")
# d1 <- read_csv("https://github.com/TheEconomist/big-mac-data/blob/master/output-data/big-mac-raw-index.csv")
# d1 <- read_tsv("https://github.com/TheEconomist/big-mac-data/blob/master/output-data/big-mac-full-index.csv")
d1 <- read_csv("https://raw.githubusercontent.com/TheEconomist/big-mac-data/master/output-data/big-mac-raw-index.csv")

# Data wrangling ----

d2 <- d1 %>% 
  filter(date == "2022-01-01") %>% 
  select(country_code = iso_a3, 
         country_name = name, 
         USD) %>% 
  arrange(USD) %>% 
  mutate(country_name = fct_inorder(factor(country_name))) %>% 
  mutate(versus_dollar = ifelse(USD > 0, "overvalued", "undervalued")) %>% 
  filter(country_name != "United States")

# Create plot ----

ggplot(data = d2) +
  geom_point(aes(x = USD,
                 y = country_name,
                 colour = versus_dollar),
             show.legend = FALSE, size = 2) +
  geom_segment(aes(x = 0, xend = USD, y = country_name, yend = country_name,
                 colour = versus_dollar),
             show.legend = FALSE, size = 0.25) +
  geom_text(aes(x = ifelse(versus_dollar == "undervalued", USD - 0.01, USD + 0.01),
                y = country_name,
                label = country_name,
                hjust = ifelse(versus_dollar == "undervalued", 1, 0),
                colour = versus_dollar),
            show.legend = FALSE, size = 8) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.25) +
  scale_colour_manual(values = c("#0ba5c4", "#de5864")) +
  xlim(-0.75, 0.75) +
  ggtitle("The Big Mac index") +
  theme_economist() +
  theme(panel.grid.major.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 50))

ggsave("2022/plots/12_theme_day_economist.png", p, dpi = 320, width = 12, height = 6)
  

nuclear_weapons <- read_csv("2022/data/nuclear-warhead-stockpiles.csv")

d1 <- nuclear_weapons %>% 
  filter(!Entity %in% c("United States", "Russia", "United Kingdom", "France"))

d2 <- nuclear_weapons %>% 
  mutate(bin = cut_interval(nuclear_weapons_stockpile, length = 10000))

ggplot(data = d2,
       aes(x = Year, y = Entity, fill = bin)) +
  geom_tile()

boxplot(nuclear_weapons$Year ~ nuclear_weapons$Code)

head(nuclear_weapons)

ggplot(data = nuclear_weapons,
       mapping = aes(x = Year, y = nuclear_weapons_stockpile,
                     colour = Entity)) +
  geom_line()

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
