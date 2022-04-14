# 30DayChartChallenge
# 2022
# Category : Relationships
# Day 16 : Environment
# Last updated 2022-04-01

# https://ourworldindata.org/fish-and-overfishing#wild-fish-catch

# Load packages ----

library(tidyverse)
library(showtext)
# library(ggwaffle)
# library(emojifont)
library(patchwork)

# Load fonts ----

# font_add_google("Tangerine", "Tangerine")
# showtext_auto()

# Import data ----


fish <- read_csv("2022/data/capture-fishery-production.csv")

# Data wrangling ----

d1 <- fish %>% 
  filter(Year %in% c(min(Year), max(Year))) %>% 
  select(country = Entity,
         year = Year,
         tons = `Capture fisheries production (metric tons)`)

d1

ggplot(data = d1,
       aes(x = year, y = tons, group = country)) +
  geom_line(alpha = 0.5, size = 2)

d1 <- plastic %>% 
  filter(Year == max(Year)) %>% 
  mutate(origin = "waste") %>% 
  select(year = Year,
         origin,
         percent = `Estimated historic plastic fate`,
         dest = Entity)

ggplot(data = d1,
       aes(y = percent, axis1 = origin, axis2 = dest)) +
  geom_alluvium(aes(fill = dest), width = 1/12)

test <- as.tibble(UCBAdmissions)  

ggplot(as.data.frame(UCBAdmissions),
       aes(y = Freq, axis1 = Gender, axis2 = Dept)) +
  geom_alluvium(aes(fill = Admit), width = 1/12)

# Create plot ----

biomass <- read_csv(file = "2022/data/")

d1 <- biomass %>% 
  filter(category == "All life")

d1

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
