# 30DayChartChallenge
# 2022
# Category : Comparisons
# Day 6 : Data day - Our World In Data
# Last updated 2022-03-28

# https://ourworldindata.org/water-sanitation-2020-update

# Load packages ----

library(ggtext)
library(patchwork)
library(showtext)
library(tidyverse)

# Load fonts ----

font_add_google("Zen Tokyo Zoo", "Zen Tokyo Zoo")
font_add_google("Glory", "Glory")
showtext_auto()

# Import data ----

water <- read_csv("2022/data/access-drinking-water-stacked.csv")

# Data wrangling ----

cat <- c("High income", "North America and Europe",
         "Western Asia and Northern Africa",
         "Upper-middle income", "Latin America and the Caribbean",
         "World", "Central and Southern Asia", "Lower-middle income",
         "Sub-Saharan Africa", "Low income")

access_2000 <- water %>% 
  filter(Entity %in% c("High income", "North America and Europe",
                       "Western Asia and Northern Africa",
                       "Upper-middle income", "Latin America and the Caribbean",
                       "World", "Central and Southern Asia", "Lower-middle income",
                       "Sub-Saharan Africa", "Low income"),
         Year == 2000) %>% 
  select(-c(Code, Year)) %>% 
  rename(Category = Entity, "Safely managed" = wat_sm,
         "Basic" = wat_bas_minus_sm, "Limited" = wat_lim,
         "Unimproved" = wat_unimp, "No access (surface water only)" = wat_sur) %>% 
  pivot_longer(cols = -Category, names_to = "Access", values_to = "Percent") %>% 
  mutate(Category = factor(Category, levels = rev(cat)),
         Access = factor(Access, levels = c("No access (surface water only)", "Unimproved",
                                            "Limited", "Basic", "Safely managed")))

access_2020 <- water %>% 
  filter(Entity %in% c("High income", "North America and Europe",
                       "Western Asia and Northern Africa",
                       "Upper-middle income", "Latin America and the Caribbean",
                       "World", "Central and Southern Asia", "Lower-middle income",
                       "Sub-Saharan Africa", "Low income"),
         Year == 2020) %>% 
  select(-c(Code, Year)) %>% 
  rename(Category = Entity, "Safely managed" = wat_sm,
         "Basic" = wat_bas_minus_sm, "Limited" = wat_lim,
         "Unimproved" = wat_unimp, "No access (surface water only)" = wat_sur) %>% 
  pivot_longer(cols = -Category, names_to = "Access", values_to = "Percent") %>% 
  mutate(Category = factor(Category, levels = rev(cat)),
         Access = factor(Access, levels = c("No access (surface water only)", "Unimproved",
                                            "Limited", "Basic", "Safely managed")))

# Create plot ----

p1 <- ggplot(access_2000, aes(x = Category, y = Percent, fill = Access)) +
  geom_bar(position = "stack", stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("#cf4d5f", "#f39654", "#ebcc85", "#269691", "#59697d")) +
  coord_flip() +
  geom_text(aes(label = ifelse(Percent > 5.8, paste0(round(Percent), "%"), "")),
            position = position_stack(vjust = 0.5), colour = "white", size = 10, family = "Glory") +
  ggtitle(label = "2000") +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(family = "Zen Tokyo Zoo", size = 100, hjust = 0.5, colour = "#b4d2d3"),
        plot.background = element_rect(fill = "#152636", colour = "#152636"),
        panel.background = element_rect(fill = "#152636", colour = "#152636"))

p2 <- ggplot(access_2020, aes(x = Category, y = Percent, fill = Access)) +
  geom_bar(position = "stack", stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("#cf4d5f", "#f39654", "#ebcc85", "#269691", "#59697d")) +
  coord_flip() +
  geom_text(aes(label = ifelse(Percent > 5.8, paste0(round(Percent), "%"), "")),
            position = position_stack(vjust = 0.5), colour = "white", size = 10, family = "Glory") +
  ggtitle(label = "2020") +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(hjust = 0.5, size = 30, family = "Glory", colour = "#b4d2d3"),
        axis.ticks = element_blank(),
        plot.title = element_text(family = "Zen Tokyo Zoo", size = 100, hjust = 0.5, colour = "#b4d2d3"),
        plot.background = element_rect(fill = "#152636", colour = "#152636"),
        panel.background = element_rect(fill = "#152636", colour = "#152636"))

p <- p1 + p2 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Clean water and sanitation",
    subtitle = paste0("Access to <b style = 'color:#59697d'>safely managed</b>",
                      ", <b style = 'color:#269691'>basic</b>",
                      ", <b style = 'color:#ebcc85'>limited</b>",
                      ", <b style = 'color:#f39654'>unimproved</b>",
                      ", or <b style = 'color:#cf4d5f'>no access</b> to water facilities"),
    caption = "Visualisation: Jonathan Kitt | Data source: Our World In Data | #30DayChartChallenge 2022 | Day 6: data day - OWID",
    theme = theme(plot.title = element_text(family = "Zen Tokyo Zoo", colour = "#b4d2d3", size = 130, hjust = 0.5,
                                            margin = margin(t = 20, b = 20)),
                  plot.subtitle = element_markdown(family = "Glory", colour = "#b4d2d3", size = 75, hjust = 0.5,
                                               margin = margin(b = 20)),
                  plot.background = element_rect(fill = "#152636", colour = "#152636"),
                  plot.caption = element_text(colour = "#b4d2d3", hjust = 0.5, size = 25,
                                              margin = margin(t = 20))))

# Save plot ----

ggsave("2022/plots/finished/06_data_day_owid.png", p, dpi = 320, width = 12, height = 6)
