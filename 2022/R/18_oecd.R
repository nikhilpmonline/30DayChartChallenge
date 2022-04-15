# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 18 : Data day - OECD
# Last updated 2022-04-14

# https://data.oecd.org/agroutput/crop-production.htm
# https://www.cedricscherer.com/2019/05/17/the-evolution-of-a-ggplot-ep.-1/

# Load packages ----

library(tidyverse)
#library(showtext)
#library(patchwork)

# Load fonts ----

font_add_google("Grandstander", "Grandstander")
font_add_google("Codystar", "Codystar")
showtext_auto()

# Import data ----

oecd_prod <- read_csv("2022/data/oecd_production.csv")

# Data wrangling ----

d1 <- oecd_prod %>% 
  filter(LOCATION == "OECD", MEASURE == "TONNE_HA", TIME <= 2021) %>% 
  select(crop = SUBJECT, year = TIME, ton_ha = Value) %>% 
  group_by(crop) %>% 
  mutate(mean = mean(ton_ha),
         crop = factor(crop, levels = c("SOYBEAN", "WHEAT", "RICE", "MAIZE")))

p <- ggplot(data = d1,
       aes(x = year, y = ton_ha, colour = crop)) +
  geom_line(size = 2,
            show.legend = FALSE) +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  annotate("text", x = 2013, y = 7.5, label = "MAIZE", family = "Codystar", size = 25,
           colour = "#c77cff", hjust = 0) + 
  annotate("text", x = 2013, y = 5.5, label = "RICE", family = "Codystar", size = 25,
           colour = "#00bfc4", hjust = 0) + 
  annotate("text", x = 2013, y = 4.4, label = "WHEAT", family = "Codystar", size = 25,
           colour = "#7cae00", hjust = 0) + 
  annotate("text", x = 2013, y = 2.5, label = "SOYBEAN", family = "Codystar", size = 25,
           colour = "#f8766d", hjust = 0) + 
  labs(title = "Crop production for OECD members",
       subtitle = "In tons/hectare from 1990 to 2021") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.title = element_text(family = "Codystar", size = 100, colour = "white"),
        plot.subtitle = element_text(family = "Codystar", size = 75, colour = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.05),
        axis.title = element_blank(),
        axis.text = element_text(family = "Grandstander", colour = "white", size = 35))

ggsave("2022/plots/work_in_progress/18_oecd.png", p, dpi = 320, width = 12, height = 6)


# Create plot ----

p1 <- ggplot() +
  geom_line(data = maize,
            aes(x = year, y = ton_ha),
            colour = "#b524dc", size = 2) +
  geom_point(data = maize_points,
             aes(x = year, y = ton_ha),
             shape = 21, colour = "#b524dc", fill = "#242632",
             size = 5, stroke = 2) +
  annotate("text", x = 1990, y = 8, label = "MAIZE", family = "Codystar", size = 100,
           colour = "#e4a9ee", alpha = 0.5, hjust = 0) + 
  annotate("text", x = 1993.5, y = 5.3, label = "5.3 (1993)", family = "Grandstander", size = 25,
           colour = "#b524dc", hjust = 0) + 
  annotate("text", x = 2012.5, y = 6.9, label = "6.9 (2012)", family = "Grandstander", size = 25,
           colour = "#b524dc", hjust = 0) + 
  annotate("text", x = 2018, y = 9.7, label = "9.5 (2018)", family = "Grandstander", size = 25,
           colour = "#b524dc", hjust = 0) + 
  theme_void() +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"),
        panel.grid = element_blank(),
        axis.title = element_blank())

p2 <- ggplot() +
  geom_line(data = rice,
            aes(x = year, y = ton_ha),
            colour = "#b524dc", size = 2) +
  geom_point(data = rice_points,
             aes(x = year, y = ton_ha),
             shape = 21, colour = "#b524dc", fill = "#242632",
             size = 5, stroke = 2) +
  annotate("text", x = 2010, y = 4.2, label = "RICE", family = "Codystar", size = 100,
           colour = "#e4a9ee", alpha = 0.5, hjust = 0) +
  annotate("text", x = 1993.5, y = 3.77, label = "3.8 (1993)", family = "Grandstander", size = 25,
           colour = "#b524dc", hjust = 0) +
  annotate("text", x = 2003.5, y = 4.39, label = "4.4 (2003)", family = "Grandstander", size = 25,
           colour = "#b524dc", hjust = 0) +
  annotate("text", x = 2016, y = 5.1, label = "5 (2016)", family = "Grandstander", size = 25,
           colour = "#b524dc", hjust = 0) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#242632", colour = "#242632"),
        plot.background = element_rect(fill = "#242632", colour = "#242632"),
        panel.grid = element_blank(),
        axis.title = element_blank())

p <- p1 + p2

ggsave("2022/plots/work_in_progress/18_oecd.png", p, dpi = 320, width = 12, height = 6)

ggplot(data = rice) +
  geom_line(aes(x = year, y = ton_ha))

ggplot(data = wheat) +
  geom_line(aes(x = year, y = ton_ha))

ggplot(data = soybean) +
  geom_line(aes(x = year, y = ton_ha))


ggplot(data = d1, 
       aes(x = ton_ha, y = crop)) +
  geom_point(aes(colour = crop), shape = 1,
             size = 3, alpha = 0.5, show.legend = FALSE) +
  stat_summary(aes(colour = crop),
               fun = mean, geom = "point", size = 5, shape = 16,
               show.legend = FALSE) +
  annotate("text", x = 7, y = 4.5, label = "Full circles represent the mean",
           family = "Gluten", size = 12, hjust = 1) +
  geom_curve(aes(x = 7.1, xend = 7.55, y = 4.5, yend = 4.15),
             curvature = -0.2,
             arrow = arrow(length = unit(0.07, "inch")), size = 0.4) +
  labs(title = "Global crop production in OECD members",
       subtitle = "Production in tons per hectare from 1990 to 2018",
       x = "Tons / hectare",
       y = "",
       caption = "Visualisation : Jonathan Kitt | Data source : OECD | #30DayChartChallenge 2022 | Day 18 : data day - OECD") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#faebd7", colour = "#faebd7"),
        plot.background = element_rect(fill = "#faebd7", colour = "#faebd7"),
        plot.title = element_text(family = "Londrina", size = 100,
                                  margin = margin(t = 20)),
        plot.subtitle = element_text(family = "Kranky", size = 50,
                                     margin = margin(b = 20)),
        plot.caption = element_text(hjust = 0.5, size = 25),
        axis.text = element_text(family = "Gluten", size = 30),
        axis.title = element_text(family = "Gluten", size = 40))

# Save plot ----

ggsave("2022/plots/work_in_progress/18_oecd.png", p, dpi = 320, width = 12, height = 6)

d1 <- crops %>% 
  filter(LOCATION == "OECD", MEASURE == "TONNE_HA", TIME <= 2021)

ggplot(d1, aes(x = TIME, y = Value, colour = SUBJECT)) +
  geom_line()

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
