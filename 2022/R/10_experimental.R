# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 10 : Experimental
# Last updated 2022-03-18

# https://www.angio.net/pi/digits.html


# Load packages ----

library(tidyverse)
library(showtext)
library(viridis)
library(patchwork)

# Load fonts ----

# font_add_google("DotGothic16", "DotGothic16")
font_add_google("Odibee Sans", "Odibee Sans")
showtext_auto()

# Import data ----

pi <- read_file("2022/data/PI10K_DP.TXT")

# Data wrangling ----

pi <- pi %>% 
  str_remove(pattern = "\r") %>% 
  str_remove(pattern = "\n") %>% 
  str_sub(start = 1, end = 1000)

pi_table <- tibble(
  x = rep(1:40, times = 25),
  y = rep(25:1, each = 40),
  value = unlist(str_split(pi, pattern = "")))

pi_string <- str_split(string = pi, pattern = "") %>% unlist()

positions <- list()
numbers <- as.character(0:9)

for (i in 1:length(numbers)) {
  
  positions[[i]] <- tibble(
    number = numbers[i],
    position = str_which(pi_string, numbers[i])
  )
  
}

positions_table <- data.table::rbindlist(positions) %>% 
  as_tibble() %>% 
  mutate(y.pos = 9 - as.numeric(number))

# Create plot ----

p1 <- ggplot() +
  geom_text(data = pi_table,
            aes(x = x, y = y, label = value, colour = value),
            size = 12, family = "Odibee Sans",
            show.legend = FALSE) +
  scale_colour_viridis(option = "magma", discrete = TRUE, direction = -1) +
  annotate("text", x = 20, y = 12.5, label = "pi", parse = TRUE,
           size = 400, alpha = 0.1) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#858b97", colour = "#858b97"),
        plot.background = element_rect(fill = "#858b97", colour = "#858b97"))

ggsave("2022/plots/10_experimental_1.png", p1, dpi = 320, width = 12, height = 6)

p2 <- ggplot() +
  geom_segment(data = positions_table,
               aes(x = position, xend = position,
                   y = y.pos - 0.4, yend = y.pos + 0.4,
                   colour = number),
               show.legend = FALSE) +
  scale_colour_viridis(option = "magma", discrete = TRUE, direction = -1) +
  scale_x_continuous(breaks = seq(0, 1000, 100),
                     labels = c(1, seq(100, 1000, 100))) +
  annotate("text", x = -25, y = 9, label = 0, colour = "#fcfdbf", family = "Odibee Sans", size = 25, hjust = 0.5) +
  annotate("text", x = -25, y = 8, label = 1, colour = "#fec98f", family = "Odibee Sans", size = 25, hjust = 0.5) +
  annotate("text", x = -25, y = 7, label = 2, colour = "#fd9567", family = "Odibee Sans", size = 25, hjust = 0.5) +
  annotate("text", x = -25, y = 6, label = 3, colour = "#f1605d", family = "Odibee Sans", size = 25, hjust = 0.5) +
  annotate("text", x = -25, y = 5, label = 4, colour = "#cd4071", family = "Odibee Sans", size = 25, hjust = 0.5) +
  annotate("text", x = -25, y = 4, label = 5, colour = "#9f2f7f", family = "Odibee Sans", size = 25, hjust = 0.5) +
  annotate("text", x = -25, y = 3, label = 6, colour = "#721f81", family = "Odibee Sans", size = 25, hjust = 0.5) +
  annotate("text", x = -25, y = 2, label = 7, colour = "#451077", family = "Odibee Sans", size = 25, hjust = 0.5) +
  annotate("text", x = -25, y = 1, label = 8, colour = "#180f3e", family = "Odibee Sans", size = 25, hjust = 0.5) +
  annotate("text", x = -25, y = 0, label = 9, colour = "#000004", family = "Odibee Sans", size = 25, hjust = 0.5) +
  theme(panel.background = element_rect(fill = "#858b97", colour = "#858b97"),
        plot.background = element_rect(fill = "#858b97", colour = "#858b97"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_line(colour = "white"),
        axis.ticks.length.x = unit(0.25, "cm"),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(family = "Odibee Sans", colour = "white", size = 30),
        axis.text.y = element_blank(),
        axis.line.x = element_line(colour = "white"))

p <- p1 + p2 +
  plot_annotation(
    title = "First 1,000 decimals of pi",
    caption = "Visualisation: Jonathan Kitt | Data source: www.angio.net | #30DayChartChallenge 2022 | Day 10: experimental",
    theme = theme(plot.title = element_text(family = "Odibee Sans", colour = "white", size = 120, hjust = 0.5,
                                            margin = margin(t = 20, b = 10)),
                  plot.background = element_rect(fill = "#858b97", colour = "#858b97"),
                  plot.caption = element_text(colour = "#263e31", hjust = 0.5, size = 25)))

ggsave("2022/plots/10_experimental.png", p, dpi = 320, width = 12, height = 6)

################################################################################

number_list <- list()

for (i in 0:9) {
  
  number_list[i] <- number_distribution == i
  
}

test <- 
dec_0 <- test == 0 
dec_1 <- pi_string == 1
dec_2 <- test == 2 
dec_3 <- test == 3 
dec_4 <- test == 4 
dec_5 <- test == 5 
dec_6 <- test == 6 
dec_7 <- test == 7 
dec_8 <- test == 8 
dec_9 <- test == 9 

decimal_places <- tibble(
  number = rep(0:9, each = 1000),
  x = rep(1:1000, times = 10),
  present = c(dec_0, dec_1, dec_2, dec_3, dec_4, dec_5, dec_6, dec_7, dec_8, dec_9))

# Create plot ----




p <- p1 + p1 +
  plot_annotation(
    title = "First 1,000 decimals of pi",
    caption = "Visualisation: Jonathan Kitt | Data source: www.angio.net | #30DayChartChallenge 2022 | Day 10: experimental",
    theme = theme(plot.title = element_text(family = "Odibee Sans", colour = "#263e31", size = 120, hjust = 0.5,
                                            margin = margin(t = 20, b = 10)),
                  plot.background = element_rect(fill = "#858b97", colour = "#858b97"),
                  plot.caption = element_text(colour = "#263e31", hjust = 0.5, size = 25)))

ggsave("2022/plots/10_experimental.png", p, dpi = 320, width = 12, height = 6)

ggsave("2022/plots/10_experimental_1.png", p, dpi = 320, width = 12, height = 6)



p2 <- ggplot(data = decimal_places %>% filter(present == TRUE)) +
  geom_segment(aes(x = x, xend = x,
                   y = number - 0.4, yend = number + 0.4,
                   colour = as.factor(number)),
               show.legend = FALSE, size = 2) +
  scale_colour_viridis(option = "magma", discrete = TRUE, direction = -1) +
  scale_x_continuous(breaks = seq(0, 1000, 100),
                     labels = c("", seq(100, 1000, 100))) +
  theme(panel.background = element_rect(fill = "#858b97", colour = "#858b97"),
        plot.background = element_rect(fill = "#858b97", colour = "#858b97"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "white", linetype = "dashed"),
        axis.ticks = element_blank(),
        axis.title = element_blank())
  
  

## Here are packages I'm going to use.
library(tidyverse)  
library(tidytext) ## so I can break single digit per line 
library(circlize)


df <-data.frame(x =read_lines("http://www.geom.uiuc.edu/~huberty/math5337/groupe/digits.html"))


df$x <- as.character(df$x)
df <- df %>% slice(-1:-12)  ## discard first 12 lines
df <- df %>% slice(1:1283)  ## anything after 1283 is not pi so i only want to grab 1~1283

pi <- read_file("2022/data/pi1000000.txt") %>% 
  as.double()

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
