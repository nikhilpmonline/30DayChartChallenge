# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 10 : Experimental
# Last updated 2022-03-28

# https://www.angio.net/pi/digits.html


# Load packages ----

library(patchwork)
library(showtext)
library(tidyverse)
library(viridis)


# Load fonts ----

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
           size = 400, alpha = 0.2) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#858b97", colour = "#858b97"),
        plot.background = element_rect(fill = "#858b97", colour = "#858b97"))

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

# Save plot ----

ggsave("2022/plots/finished/10_experimental.png", p, dpi = 320, width = 12, height = 6)
