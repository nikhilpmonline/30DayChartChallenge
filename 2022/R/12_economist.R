# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 12 : Theme day - The Economist
# Last updated 2022-04-12

# https://jrnold.github.io/ggthemes/reference/theme_economist.html
# https://www.economist.com/big-mac-index

# Load packages ----

library(ggthemes)
library(lubridate)
library(tidyverse)


# Download data ----

d1 <- read_csv("https://raw.githubusercontent.com/TheEconomist/big-mac-data/master/output-data/big-mac-raw-index.csv")

# Data wrangling ----

d1 <- d1 %>% 
  filter(date == "2022-01-01") %>% 
  select(country_code = iso_a3, 
         country_name = name, 
         USD) %>% 
  arrange(USD) %>% 
  mutate(country_name = fct_inorder(factor(country_name))) %>% 
  mutate(versus_dollar = ifelse(USD > 0, "overvalued", "undervalued")) %>% 
  filter(country_name != "United States")

# Create plot ----

p <- ggplot() +
  geom_point(data = d1,
             aes(x = USD,
                 y = country_name,
                 colour = versus_dollar),
             show.legend = FALSE, size = 1.5) +
  geom_segment(data = d1,
               aes(x = 0, xend = USD, y = country_name, yend = country_name,
                 colour = versus_dollar),
             show.legend = FALSE, size = 0.25) +
  geom_text(data = d1,
            aes(x = ifelse(versus_dollar == "undervalued", USD - 0.01, USD + 0.01),
                y = country_name,
                label = country_name,
                hjust = ifelse(versus_dollar == "undervalued", 1, 0),
                colour = versus_dollar),
            show.legend = FALSE, size = 2) +
  scale_colour_manual(values = c("#0ba5c4", "#de5864")) +
  scale_x_continuous(breaks = c(-0.5, 0, 0.5), labels = c("- 50%", "US $", "+ 50%"), limits = c(-0.8, 0.8)) +
  xlab("US $") +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.25) +
  ggtitle("The Big Mac Index (2022)",
          subtitle = "Introduced in The Economist in 1986,\nthis index highlights whether currencies are\nunder- or over-valued relative to the US $,\nbased on the price of a Big Mac.") +
  labs(caption = "Visualisation: Jonathan Kitt | Data source: The Economist | #30DayChartChallenge 2022 | Day 12: theme day - The Economist") +
  theme_economist(base_family = "ITC Officina Sans") +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.line.x = element_line(size = 0.25),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, margin = margin(b = 10)),
        plot.caption = element_text(size = 6, margin = margin(t = 10, b = 10)))

# Save plot ----

ggsave("2022/plots/12_economist.png", p, dpi = 320, width = 12, height = 6)
