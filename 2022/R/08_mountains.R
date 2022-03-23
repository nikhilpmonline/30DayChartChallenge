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

p <- ggplot(d1, aes(x = ratio, fill = species, colour = species)) +
  geom_density_ridges(aes(y = species),
                      show.legend = FALSE) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  annotate("text", x = 2, y = 1.2, label = "Adelie", colour = "white", alpha = 0.5, size = 15) +
  annotate("text", x = 2.75, y = 2.2, label = "Chinstrap", colour = "white", alpha = 0.5, size = 15) +
  annotate("text", x = 3.25, y = 3.2, label = "Gentoo", colour = "white", alpha = 0.5, size = 15) +
  ggtitle(label = "Penguin bill dimensions",
          subtitle = "Distribution of bill length to bill depth ratio for 3 penguin species") +
  labs(caption = "Visualisation: Jonathan Kitt | Data source: Palmer Penguins | #30DayChartChallenge 2022 | Day 8: mountains") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#9ebfe0", colour = "#9ebfe0"),
        plot.background = element_rect(fill = "#9ebfe0", colour = "#9ebfe0"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(colour = "#041f32", size = 40, hjust = 0.5,
                                  margin = margin(t = 20, b = 10)),
        plot.subtitle = element_text(colour = "#041f32", size = 20, hjust = 0.5,
                                  margin = margin(t = 0, b = 20)),
        plot.caption = element_text(colour = "#041f32", hjust = 0.5,
                                    margin = margin(b = 15, t = 20)))

# Save plot ----

ggsave("2022/plots/08_mountains.png", p, dpi = 320, width = 12, height = 6)
