# 30DayChartChallenge

# Last updated 2022-03-04

# Load packages ----

library(tidyverse)
library(munro)
library(showtext)

# Comparisons ----

# 1 - Part-to-whole

font_add_google("Lobster", "Lobster")
showtext_auto()

munros <- munro::munros

d1 <- munros %>% 
  mutate(unique_name = paste(1:nrow(.), name, sep = " - ")) %>% 
  select(unique_name, county) %>% 
  separate_rows(county, sep = ",") %>% 
  mutate(county = factor(str_trim(county))) %>% 
  count(county)

highland_munros <- tibble(
  county = c("Highland", "Other"),
  n = c(d1$n[d1$county == "Highland"],
        nrow(munros) - d1$n[d1$county == "Highland"])) %>% 
  mutate(ratio = n / sum(n)) %>% 
  mutate(ymin = c(0, max(ratio)),
         ymax = c(max(ratio), 1))

ggplot(data = highland_munros,
       aes(x = 1.5, y = ratio, fill = county)) +
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  scale_fill_manual(values = c("#04aed9", "#c5e0f5")) +
  coord_polar(theta = "y", start = 0) +
  xlim(c(0.025, 2.5)) +
  ggtitle(label = "How many Munros are located in the Highlands ?") +
  annotate("text", x = 2.25, y = 0, label = "Munros are mountains in Scotland above 3,000 feet",
           colour = "white", family = "Lobster", size = 8) +
  annotate("text", x = 2.25, y = 0.5, label = "Out of 282 Munros, 186 are located in the Highlands",
           colour = "white", family = "Lobster", size = 8) +
  annotate("text", x = 2.5, y = 0.5, label = "Visualisation: Jonathan Kitt | Data source: munro package | #30DayChartChallenge 2022 | Day 1: part-to-whole",
           colour = "white", size = 3) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#041f32", colour = "#041f32"),
        plot.background = element_rect(fill = "#041f32", colour = "#041f32"),
        plot.title = element_text(colour = "white", hjust = 0.5,
                                  family = "Lobster", size = 30, margin = margin(t = 30)))
  
# 2 - Pictogram
# 3 - Historical
# 4 - Flora
# 5 - Slope
# 6 - Data day: OWID

# Distributions ----
# 7 - Physical
# 8 - Mountains
# 9 - Statistics
# 10 - Experimental
# 11 - Circular
# 12 - Theme day: The Economist
# Relationships ----
# Timeseries ----
# Uncertainties ----