# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 9 : Statistics
# Last updated 2022-04-09

# Load packages ----

library(patchwork)
library(showtext)
library(tidyverse)

# Load fonts ----

font_add_google("Big Shoulders Inline Text", "Big Shoulders Inline Text")
showtext_auto()

# Data wrangling ----

binomial_data <- rbinom(n = 10000, size = 10, prob = 0.5)
poisson_data <- rpois(n = 10000, lambda = 3)
normal_data <- rnorm(n = 10000, mean = 0, sd = 1)
lognorm_data <- rlnorm(n = 10000, meanlog = 0.2)
beta_data <- rbeta(n = 10000, shape1 = 1, shape2 = 10)
gamma_data <- rgamma(n = 10000, shape = 5)

# Create plot ----

p1 <- ggplot() +
  geom_bar(aes(x = binomial_data),
           width = 0.2, fill = "white", colour = "white") +
  annotate("text", x = 0, y = 2000, label = "Binomial",
           family = "Big Shoulders Inline Text", size = 35,
           colour = "#b589d6", hjust = 0) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#804fb3", colour = "#804fb3"),
        plot.background = element_rect(fill = "#804fb3", colour = "#804fb3"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

p2 <- ggplot() +
  geom_bar(aes(x = poisson_data),
           width = 0.2, fill = "white", colour = "white") +
  annotate("text", x = 5, y = 1500, label = "Poisson",
           family = "Big Shoulders Inline Text", size = 35,
           colour = "#b589d6", hjust = 0) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#804fb3", colour = "#804fb3"),
        plot.background = element_rect(fill = "#804fb3", colour = "#804fb3"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

p3 <- ggplot() +
  geom_histogram(aes(x = normal_data),
                 bins = 100, fill = "white", colour = "white") +
  annotate("text", x = 0, y = 50, label = "Normal",
           family = "Big Shoulders Inline Text", size = 35,
           colour = "#b589d6", hjust = 0.5) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#804fb3", colour = "#804fb3"),
        plot.background = element_rect(fill = "#804fb3", colour = "#804fb3"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

p4 <- ggplot() +
  geom_histogram(aes(x = lognorm_data),
                 bins = 100, fill = "white", colour = "white") +
  annotate("text", x = 5, y = 1000, label = "Log Normal",
           family = "Big Shoulders Inline Text", size = 35,
           colour = "#b589d6", hjust = 0) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#804fb3", colour = "#804fb3"),
        plot.background = element_rect(fill = "#804fb3", colour = "#804fb3"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

p5 <- ggplot() +
  geom_histogram(aes(x = beta_data),
                 bins = 100, fill = "white", colour = "white") +
  annotate("text", x = 0.2, y = 200, label = "Beta",
           family = "Big Shoulders Inline Text", size = 35,
           colour = "#b589d6", hjust = 0) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#804fb3", colour = "#804fb3"),
        plot.background = element_rect(fill = "#804fb3", colour = "#804fb3"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

p6 <- ggplot() +
  geom_histogram(aes(x = gamma_data),
                 bins = 100, fill = "white", colour = "white") +
  annotate("text", x = 10, y = 200, label = "Gamma",
           family = "Big Shoulders Inline Text", size = 35,
           colour = "#b589d6", hjust = 0) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#804fb3", colour = "#804fb3"),
        plot.background = element_rect(fill = "#804fb3", colour = "#804fb3"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

p <- p1 + p2 + p3 + p4 + p5 + p6 +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Statistical distributions",
    caption = "Visualisation: Jonathan Kitt | #30DayChartChallenge 2022 | Day 9: statistical",
    theme = theme(plot.title = element_text(family = "Big Shoulders Inline Text", colour = "white", size = 120, hjust = 0.5,
                                            margin = margin(t = 20, b = 10)),
                  plot.background = element_rect(fill = "#804fb3", colour = "#804fb3"),
                  plot.caption = element_text(colour = "white", hjust = 0.5, size = 25)))

# Save plot ----

ggsave("2022/plots/09_statistics.png", p, dpi = 320, width = 12, height = 6)