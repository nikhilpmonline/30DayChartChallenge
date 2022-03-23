# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 8 : Mountains
# Last updated 2022-03-23

# https://education.rstudio.com/blog/2020/07/palmerpenguins-cran/

# Load packages ----

library(palmerpenguins)
library(ggridges)
library(tidyverse)

# Import data ----

penguins <- palmerpenguins::penguins

# Data wrangling ----

d1 <- penguins %>% 
  mutate(ratio = bill_length_mm / bill_depth_mm)

# Create plot ----

p <- ggplot(d1, aes(x = ratio, fill = species)) +
  geom_density_ridges(aes(y = species)) +
  theme_minimal()

# Save plot ----

ggsave("2022/plots/08_mountains.png", p, dpi = 320, width = 12, height = 6)
