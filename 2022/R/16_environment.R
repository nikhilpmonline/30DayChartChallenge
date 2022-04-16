# 30DayChartChallenge
# 2022
# Category : Relationships
# Day 16 : Environment
# Last updated 2022-04-16

# https://github.com/vincentarelbundock/countrycode
# https://ourworldindata.org/emissions-by-sector

# Load packages ----

library(countrycode)
library(tidyverse)
library(showtext)
library(patchwork)

# Load fonts ----

font_add_google("Gruppo", "Gruppo")
showtext_auto()

# Import data ----

country_list <- countrycode::codelist
co2 <- read_csv("2022/data/per-capita-co2-sector.csv")

# Data wrangling ----

country_list <- country_list %>% 
  select(iso3c, continent, country = country.name.en)

d1 <- co2 %>% 
  filter(Code %in% country_list$iso3c,
         Year %in% c(1990, 2018)) %>% 
  select(code = Code, country = Entity, year = Year,
         tons_person = `Aviation and shipping (per capita)`) %>% 
  pivot_wider(names_from = year,
              values_from = tons_person) %>% 
  rename(tons_person_1990 = `1990`,
         tons_person_2018 = `2018`)

# Create plot ----

p <- ggplot(d1, aes(x = tons_person_1990, y = tons_person_2018)) +
  geom_segment(aes(x = 0, xend = 30, y = 0, yend = 30),
               colour = "#1f9ce4", size = 0.25) +
  geom_point(shape = 16, colour = "#88f4ff", size = 2.5, alpha = 0.5) +
  labs(x = "1990", y = "2018",
       title = "Carbon dioxyde emissions",
       subtitle = "evolution of aviation- and shipping-related emissions between 1990 and 2018",
       caption = "Visualisation : Jonathan Kitt | Data source : Our World In Data | #30DayChartChallenge 2022 | Day 16 : environment") +
  annotate("text", x = 13.5, y = 31.5, family = "Gruppo", size = 15, colour = "#88f4ff", hjust = 0,
           label = "Singapore") +
  annotate("text", x = 16.2, y = 7.77, family = "Gruppo", size = 15, colour = "#88f4ff", hjust = 0,
           label = "United Arab Emirates") +
  annotate("text", x = 1.156, y = 17.2, family = "Gruppo", size = 15, colour = "#88f4ff", hjust = 0,
           label = "Malta") +
  annotate("text", x = 4.82, y = 8.52, family = "Gruppo", size = 15, colour = "#88f4ff", hjust = 0,
           label = "Seychelles") +
  annotate("text", x = 5, y = 25, family = "Gruppo", size = 20, colour = "#1f9ce4", hjust = 0,
           label = "countries above the line have") +
  annotate("text", x = 5, y = 23, family = "Gruppo", size = 20, colour = "#1f9ce4", hjust = 0,
           label = "increased their emissions") +
  annotate("text", x = 10, y = 3, family = "Gruppo", size = 20, colour = "#1f9ce4", hjust = 0,
           label = "countries below the line have") +
  annotate("text", x = 10, y = 1, family = "Gruppo", size = 20, colour = "#1f9ce4", hjust = 0,
           label = "decreased their emissions") +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"),
        panel.grid = element_blank(),
        axis.title = element_text(family = "Gruppo", colour = "white", size = 40),
        axis.text = element_text(family = "Gruppo", colour = "white", size = 30),
        plot.title = element_text(family = "Gruppo", colour = "white", size = 100,
                                  margin = margin(t = 10, b = 5)),
        plot.subtitle = element_text(family = "Gruppo", colour = "white", size = 50,
                                     margin = margin(b = 10)),
        plot.caption = element_text(colour = "white", size = 25, hjust = 0.5,
                                    margin = margin(t = 20, b = 10)))

# Save plot ----

ggsave("2022/plots/16_environment.png", p, dpi = 320, width = 12, height = 6)
