# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 8 : Mountains
# Last updated 2022-03-28

# https://education.rstudio.com/blog/2020/07/palmerpenguins-cran/

# Load packages ----

library(ggridges)
library(palmerpenguins)
library(showtext)
library(tidyverse)

# Load fonts ----

font_add_google("Righteous", "Righteous")
showtext_auto()

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
  annotate("text", x = 2, y = 1.5, hjust = 0,label = "Adelie", colour = "white", alpha = 0.5, size = 30, family = "Righteous") +
  annotate("text", x = 2.46, y = 2.5, hjust = 0, label = "Chinstrap", colour = "white", alpha = 0.5, size = 30, family = "Righteous") +
  annotate("text", x = 3.05, y = 3.5, hjust = 0, label = "Gentoo", colour = "white", alpha = 0.5, size = 30, family = "Righteous") +
  ggtitle(label = "The biggest bill",
          subtitle = "Distribution of bill length to bill depth ratio for 3 penguin species") +
  labs(caption = "Visualisation: Jonathan Kitt | Data source: Palmer Penguins | #30DayChartChallenge 2022 | Day 8: mountains") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#9ebfe0", colour = "#9ebfe0"),
        plot.background = element_rect(fill = "#9ebfe0", colour = "#9ebfe0"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "Righteous", colour = "#041f32", size = 25),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.5),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(colour = "#041f32", size = 100, hjust = 0.5,
                                  family = "Righteous",
                                  margin = margin(t = 20, b = 10)),
        plot.subtitle = element_text(colour = "#041f32", size = 60, hjust = 0.5,
                                  margin = margin(t = 0, b = 40), family = "Righteous"),
        plot.caption = element_text(colour = "#041f32", hjust = 0.5,
                                    margin = margin(b = 15, t = 20), size = 25))

# Save plot ----

ggsave("2022/plots/finished/08_mountains.png", p, dpi = 320, width = 12, height = 6)
