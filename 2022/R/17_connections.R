# 30DayChartChallenge
# 2022
# Category : Relationships
# Day 17 : Connections
# Last updated 2022-03-30

# Load packages ----

library(osmdata)
library(patchwork)
library(showtext)
library(tidyverse)

# Load fonts ----

font_add_google("Cinzel Decorative", "Cinzel")
font_add_google("Aladin", "Aladin")
font_add_google("Cormorant SC", "Cormorant SC")
font_add_google("Metamorphous", "Metamorphous")
font_add_google("Almendra", "Almendra")
font_add_google("Uncial Antiqua", "Uncial Antiqua")
font_add_google("Eagle Lake", "Eagle Lake")
font_add_google("Snowburst One", "Snowburst One")
showtext_auto()

# Import datasets ----

# Lille

lille_subway <- getbb("Lille") %>% 
  opq() %>% 
  add_osm_feature(key = "route",
                  value = "subway") %>% 
  osmdata_sf()

lille_stops <- getbb("Lille") %>% 
  opq() %>% 
  add_osm_feature(key = "railway",
                  value = "subway_entrance") %>% 
  osmdata_sf()

# Lyon

lyon_subway <- getbb("Lyon") %>% 
  opq() %>% 
  add_osm_feature(key = "route",
                  value = "subway") %>% 
  osmdata_sf()

lyon_stops <- getbb("Lyon") %>% 
  opq() %>% 
  add_osm_feature(key = "railway",
                  value = "subway_entrance") %>% 
  osmdata_sf()

# Edinburgh

edinburgh_tramway <- getbb("Edinburgh") %>% 
  opq() %>% 
  add_osm_feature(key = "route",
                  value = "tram") %>% 
  osmdata_sf()

edinburgh_buses <- getbb("Edinburgh") %>% 
  opq() %>% 
  add_osm_feature(key = "route",
                  value = "bus") %>% 
  osmdata_sf()

list_of_buses <- tibble(line = unique(lothian_buses$name))

filtered_buses <- list_of_buses %>% filter(across(line, ~ grepl("Lothian", .))) %>% 
  mutate(line = gsub("Lothian City Buses", "", line)) %>% 
  mutate(line = gsub("Lothian Buses", "", line)) %>% 
  arrange(line)

line_1 <- edinbu

lothian_buses <- subset(edinburgh_buses$osm_multilines, operator %in% c("Lothian Buses", "Lothian City Buses"))

line_1_a <- subset(lothian_buses, name == "Lothian City Buses 1: Clermiston => Seafield")
line_1_b <- subset(lothian_buses, name == "Lothian City Buses 1: Seafield => Clermiston")


unique(edinburgh_buses$osm_lines$operator)

edinburgh_tramway_stops <- getbb("Edinburgh") %>% 
  opq() %>% 
  add_osm_feature(key = "railway",
                  value = "tram_stop") %>% 
  osmdata_sf()

ggplot() + 
  geom_sf(data = line_1_b$geometry)

# Data wrangling ----

# Lille

lille_ligne_1 <- subset(lille_subway$osm_lines, name == "Ligne 1")
lille_ligne_2 <- subset(lille_subway$osm_lines, name == "Ligne 2")

porte_des_postes <- subset(lille_stops$osm_points, name == "Porte des Postes")[1, ]
lille_flandres <- subset(lille_stops$osm_points, name == "Gare Lille Flandres")[1, ]

# Lyon

lyon_ligne_a <- subset(lyon_subway$osm_lines, name == "Ligne A")
lyon_ligne_b <- subset(lyon_subway$osm_lines, name == "Ligne B")
lyon_ligne_c <- subset(lyon_subway$osm_lines, name == "Ligne C")
lyon_ligne_d <- subset(lyon_subway$osm_lines, name == "Ligne D")

saxe_gambetta <- subset(lyon_stops$osm_points, name == "Saxe - Gambetta")[1, ]
charpennes <- subset(lyon_stops$osm_points, name == "Charpennes - Charles Hernu")[1, ]
hotel_de_ville <- subset(lyon_stops$osm_points, name == "HÃ´tel de Ville - Louis Pradel")[1, ]
bellecour <- subset(lyon_stops$osm_points, name == "Bellecour")[1, ]



# Create plot ----

p1 <- ggplot() +
  geom_sf(data = lille_ligne_1$geometry,
          inherit.aes = FALSE,
          color = "yellow",
          size = 4) +
  geom_sf(data = lille_ligne_2$geometry,
          inherit.aes = FALSE,
          color = "red",
          size = 4) +
  geom_sf(data = lille_flandres$geometry,
          inherit.aes = FALSE,
          color = "white",
          size = 8) +
  geom_sf(data = porte_des_postes$geometry,
          inherit.aes = FALSE,
          color = "white",
          size = 8) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "grey70", colour = "grey70"),
        plot.background = element_rect(fill = "grey70", colour = "grey70"),
        panel.grid = element_blank(),
        axis.text = element_blank())

p2 <- ggplot() +
  geom_sf(data = lyon_ligne_a$geometry,
          inherit.aes = FALSE,
          color = "#ee1927",
          size = 4) +
  geom_sf(data = lyon_ligne_b$geometry,
          inherit.aes = FALSE,
          color = "#00a0e8",
          size = 4) +
  geom_sf(data = lyon_ligne_c$geometry,
          inherit.aes = FALSE,
          color = "#fab211",
          size = 4) +
  geom_sf(data = lyon_ligne_d$geometry,
          inherit.aes = FALSE,
          color = "#22af5f",
          size = 4) +
  geom_sf(data = saxe_gambetta$geometry,
          inherit.aes = FALSE,
          color = "white",
          size = 8) +
  geom_sf(data = charpennes$geometry,
          inherit.aes = FALSE,
          color = "white",
          size = 8) +
  geom_sf(data = hotel_de_ville$geometry,
          inherit.aes = FALSE,
          color = "white",
          size = 8) +
  geom_sf(data = bellecour$geometry,
          inherit.aes = FALSE,
          color = "white",
          size = 8) +
  coord_sf(xlim = c(4.80, 4.92),
           ylim = c(45.70, 45.80)) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "grey70", colour = "grey70"),
        plot.background = element_rect(fill = "grey70", colour = "grey70"),
        panel.grid = element_blank(),
        axis.text = element_blank())

p <- p1 + p2
p

ggsave("2022/plots/17_connections.png", p, dpi = 320, width = 12, height = 6)


ggplot() +
  geom_sf(data = lyon_subway$osm_lines,
          inherit.aes = FALSE,
          color = "red",
          size = .4,
          alpha = .8)

roads <- getbb("Paris") %>% 
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("motorway", "trunk", "primary", "secondary", "tertiary",
                            "unclassified", "residential", "motorway_link", "trunk_link",
                            "primary_link", "secondary_link", "tertiary_link")) %>% 
  osmdata_sf()

ggplot() +
  geom_sf(data = roads$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .4,
          alpha = .8) +
  coord_sf(xlim = c(2.28, 2.30),
           ylim = c(48.86,  48.88))

roundabouts <- getbb("Swindon") %>% 
  opq() %>% 
  add_osm_feature(key = "junction",
                  value = "roundabout") %>% 
  osmdata_sf()

motorways <- getbb("Swindon") %>% 
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = "motorway") %>% 
  osmdata_sf()

primary <- getbb("Swindon") %>% 
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("primary", "secondary", "tertiary",
                            "primary_link", "secondary_link", "tertiary_link",
                            "mini_roundabout")) %>% 
  osmdata_sf()

ggplot() +
  geom_sf(data = roundabouts$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .4,
          alpha = .8) +
  geom_sf(data = primary$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .4,
          alpha = .8) +
  coord_sf(xlim = c(-1.775, -1.770),
           ylim = c(51.560,  51.565))

# Load fonts ----

# font_add_google("Tangerine", "Tangerine")
# showtext_auto()

# Import data ----

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
