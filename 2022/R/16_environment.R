# 30DayChartChallenge
# 2022
# Category : Relationships
# Day 16 : Environment
# Last updated 2022-04-01

# https://github.com/vincentarelbundock/countrycode
# https://ourworldindata.org/emissions-by-sector

# Load packages ----

library(countrycode)
library(tidyverse)
library(showtext)
# library(ggwaffle)
# library(emojifont)
library(patchwork)

# Load fonts ----

# font_add_google("Tangerine", "Tangerine")
# showtext_auto()

# Import data ----

country_list <- countrycode::codelist
co2 <- read_csv("2022/data/per-capita-co2-sector.csv")

# Data wrangling ----

country_list <- country_list %>% 
  select(iso3c, continent, country = country.name.en)

d1 <- co2 %>% 
  filter(Code %in% country_list$iso3c,
         Year %in% c(1990, 2018))

electricity <- d1 %>% 
  select(code = Code, country = Entity, year = Year,
         tons_person = `Electricity and heat (per capita)`) %>% 
  pivot_wider(names_from = year,
              values_from = tons_person) %>% 
  rename(tons_person_1990 = `1990`,
         tons_person_2018 = `2018`)

buildings <- d1 %>% 
  select(code = Code, country = Entity, year = Year,
         tons_person = `Buildings (per capita)`) %>% 
  pivot_wider(names_from = year,
              values_from = tons_person) %>% 
  rename(tons_person_1990 = `1990`,
         tons_person_2018 = `2018`)

transport <- d1 %>% 
  select(code = Code, country = Entity, year = Year,
         tons_person = `Transport (per capita)`) %>% 
  pivot_wider(names_from = year,
              values_from = tons_person) %>% 
  rename(tons_person_1990 = `1990`,
         tons_person_2018 = `2018`)

aviation <- d1 %>% 
  select(code = Code, country = Entity, year = Year,
         tons_person = `Aviation and shipping (per capita)`) %>% 
  pivot_wider(names_from = year,
              values_from = tons_person) %>% 
  rename(tons_person_1990 = `1990`,
         tons_person_2018 = `2018`)

# Create plot ----

p1 <- ggplot(electricity, aes(x = tons_person_1990, y = tons_person_2018)) +
  geom_segment(aes(x = 0, xend = 20, y = 0, yend = 20),
               colour = "#1f9ce4") +
  geom_point(shape = 16, colour = "#88f4ff", size = 2.5, alpha = 0.5) +
  labs(x = "1990", y = "2018",
       title = "Electricity & heat",
       subtitle = "Carbon dioxyde emissions in tons per person") +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"),
        panel.grid = element_blank(),
        axis.title = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        plot.title = element_text(colour = "#1f9ce4"),
        plot.subtitle = element_text(colour = "#1f9ce4"))

p2 <- ggplot(buildings, aes(x = tons_person_1990, y = tons_person_2018)) +
  geom_segment(aes(x = 0, xend = 4, y = 0, yend = 4),
               colour = "#1f9ce4") +
  geom_point(shape = 16, colour = "#88f4ff", size = 2.5, alpha = 0.5) +
  labs(x = "1990", y = "2018",
       title = "Buildings",
       subtitle = "Carbon dioxyde emissions in tons per person") +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"),
        panel.grid = element_blank(),
        axis.title = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        plot.title = element_text(colour = "#1f9ce4"),
        plot.subtitle = element_text(colour = "#1f9ce4"))

p3 <- ggplot(transport, aes(x = tons_person_1990, y = tons_person_2018)) +
  geom_segment(aes(x = 0, xend = 10.5, y = 0, yend = 10.5),
               colour = "#1f9ce4") +
  geom_point(shape = 16, colour = "#88f4ff", size = 2.5, alpha = 0.5) +
  labs(x = "1990", y = "2018",
       title = "Transport",
       subtitle = "Carbon dioxyde emissions in tons per person") +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"),
        panel.grid = element_blank(),
        axis.title = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        plot.title = element_text(colour = "#1f9ce4"),
        plot.subtitle = element_text(colour = "#1f9ce4"))

p4 <- ggplot(aviation, aes(x = tons_person_1990, y = tons_person_2018)) +
  geom_segment(aes(x = 0, xend = 32, y = 0, yend = 32),
               colour = "#1f9ce4") +
  geom_point(shape = 16, colour = "#88f4ff", size = 2.5, alpha = 0.5) +
  labs(x = "1990", y = "2018",
       title = "Aviation & shipping",
       subtitle = "Carbon dioxyde emissions in tons per person") +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"),
        panel.grid = element_blank(),
        axis.title = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        plot.title = element_text(colour = "#1f9ce4"),
        plot.subtitle = element_text(colour = "#1f9ce4"))

p <- p1 + p2 + p3

ggsave("2022/plots/work_in_progress/16_environment.png", p, dpi = 320, width = 12, height = 6)


ggplot(data = d1,
       aes(x = category, y = percent, fill = fct_rev(location))) +
  geom_bar(position = "stack", stat = "identity", show.legend = FALSE,
           width = 0.25) +
  coord_flip() +
  scale_fill_manual(values = c("#1f9ce4", "#88f4ff", "#7339ab")) +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"))

archaea <- biomass %>% 
  filter(category == "Archaea")


archaea.p1 <- ggplot(data = filter(archaea, location == "Terrestrial"),
       aes(x = 6, y = percent, fill = location)) +
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  ylim(0, 100) +
  scale_fill_manual(values = c("#7339ab", "#393c4b")) +
  coord_polar(theta = "y", start = 0) +
  xlim(c(0.025, 8)) +
  annotate("text", x = 0.025, y = 0.5, label = "6 %", colour = "#7339ab", size = 60) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"))

archaea.p2 <-
  ggplot(data = filter(archaea, location == "Marine"),
                     aes(x = 6, y = percent, fill = location)) +
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  ylim(0, 100) +
  scale_fill_manual(values = c("#88f4ff", "#393c4b")) +
  coord_polar(theta = "y", start = 0) +
  xlim(c(0.025, 8)) +
  annotate("text", x = 0.025, y = 0.5, label = "4 %", colour = "#88f4ff", size = 60) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"))

archaea.p3 <-
  ggplot(data = filter(archaea, location == "Deep subsurface"),
         aes(x = 6, y = percent, fill = location)) +
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  ylim(0, 100) +
  scale_fill_manual(values = c("#1f9ce4", "#393c4b")) +
  coord_polar(theta = "y", start = 0) +
  xlim(c(0.025, 8)) +
  annotate("text", x = 0.025, y = 0.5, label = "90 %", colour = "#1f9ce4", size = 60) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"))

p <- archaea.p1 + archaea.p2 + archaea.p3 +
  archaea.p1 + archaea.p2 + archaea.p3 +
  archaea.p1 + archaea.p2 + archaea.p3 +
  archaea.p1 + archaea.p2 + archaea.p3 +
  archaea.p1 + archaea.p2 + archaea.p3 +
  archaea.p1 + archaea.p2 + archaea.p3 +
  plot_layout(ncol = 3)
p

ggsave("2022/plots/work_in_progress/16_environment.png", p, dpi = 320, width = 12, height = 6)
