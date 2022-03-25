# 30DayChartChallenge
# 2022
# Category : Timeseries
# Day 19 : Global change
# Last updated 2022-03-12

# https://dominicroye.github.io/en/2018/how-to-create-warming-stripes-in-r/
# https://www.climate-lab-book.ac.uk/2018/warming-stripes/
# https://www.metoffice.gov.uk/research/climate/maps-and-data/historic-station-data
# https://stats4sd.org/blog/67

# Load packages ----

library(tidyverse)
library(showtext)
library(lubridate)
library(RColorBrewer)
library(patchwork)

# Lerwick data ----

raw <- read.table(url("https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/lerwickdata.txt"),
                 skip = 5, header = TRUE, fill = TRUE, na.strings = "---") %>% 
  as_tibble()

tidied <- raw %>% 
  slice(-1) %>% 
  filter(yyyy < 2021) %>% 
  mutate_all(~str_remove(., "[*]")) %>% 
  mutate_all(~as.numeric(.))

summary(tidied)

head(tidied)

refmean <- tidied %>% 
  filter(yyyy >= 1971 & yyyy <= 2000) %>% 
  summarise()

lerwick_data <- tidied %>% 
  mutate(tmean = (tmin + tmax) / 2) %>% 
  group_by(yyyy) %>% 
  summarise(tmean = mean(tmean)) %>% 
  mutate(anomaly = tmean)

head(lerwick_data)

lerwick_yr <- tidied %>% 
  group_by(yyyy) %>% 
  summarise(tmax_av = mean(tmax),
            tmin_av = mean(tmin))

lerwick_yr

plot(lerwick_yr$yyyy, lerwick_yr$tmax_av)

plot(1:nrow(tidied), tidied$rain)

head(tidied)

head(raw)
tail(raw)

# Working through Dominic Royé's tutorial ----

temp_lisboa <- read_csv("2022/data/temp_lisboa.csv")

#select only the annual temperature and year column
temp_lisboa_yr <- select(temp_lisboa, YEAR, metANN)

#rename the temperature column
temp_lisboa_yr <- rename(temp_lisboa_yr, ta = metANN)

#missing values 999.9
summary(temp_lisboa_yr) 

# Replace 999.9 by NAs
temp_lisboa_yr <- mutate(temp_lisboa_yr, ta = ifelse(ta == 999.9, NA, ta))

temp_lisboa_yr <- mutate(temp_lisboa_yr, date = str_c(YEAR, "01-01", sep = "-") %>% ymd())

# Create the stripes
theme_strip <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(vjust = 3),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold"))

col_strip <- brewer.pal(11, "RdBu")

brewer.pal.info

p <- ggplot(temp_lisboa_yr,
       aes(x = date, y = 1, fill = ta))+
  geom_tile()+
  scale_x_date(date_breaks = "6 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "LISBOA 1880-2018",
       caption = "Datos: GISS Surface Temperature Analysis")+
  # theme_strip
  theme_void()

p

ggsave("2022/plots/19_global_change.png", p, dpi = 320, width = 12, height = 6)


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
