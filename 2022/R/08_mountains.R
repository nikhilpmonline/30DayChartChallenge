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





# Scottish mountains ----

library(tidyverse)
library(munro)
library(patchwork)

munros <- munro::munros %>% 
  mutate(type = "Munro") %>% 
  select(type, name, height_meters, height_feet, longitude, latitude)

top5_munros <- munros %>% 
  arrange(desc(height_meters)) %>% 
  head(5) %>% 
  mutate(name = fct_rev(fct_inorder(name)))

corbetts <- munro::corbetts %>% 
  mutate(type = "Corbett") %>% 
  select(type, name, height_meters, height_feet, longitude, latitude)

top5_corbetts <- corbetts %>% 
  arrange(desc(height_meters)) %>% 
  head(5) %>% 
  mutate(name = fct_rev(fct_inorder(name)))

grahams <- munro::grahams %>%
  mutate(type = "Graham") %>% 
  select(type, name, height_meters, height_feet, longitude, latitude)

top5_grahams <- grahams %>% 
  arrange(desc(height_meters)) %>% 
  head(5) %>% 
  mutate(name = fct_rev(fct_inorder(name)))

uk <- map_data("world") %>% 
  filter(region == "UK", subregion != "Northern Ireland")

p1 <- ggplot() +
  geom_polygon(data = uk,
               aes(x = long, y = lat, group = group),
               fill = "#a9b7d2", colour = "#a9b7d2") +
  coord_map("gilbert", xlim = c(-7.75, 0), ylim = c(54.6, 59.6)) +
  geom_point(data = munros, 
             aes(x = longitude, y = latitude),
             pch = 17, colour = "#3caea3", size = 3) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
        plot.background = element_rect(fill = "#0a2248", colour = "#0a2248"))

p2 <- ggplot() +
  geom_polygon(data = uk,
               aes(x = long, y = lat, group = group),
               fill = "#a9b7d2", colour = "#a9b7d2") +
  coord_map("gilbert", xlim = c(-7.75, 0), ylim = c(54.6, 59.6)) +
  geom_point(data = corbetts, 
             aes(x = longitude, y = latitude),
             pch = 17, colour = "#f6d55c", size = 3) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
        plot.background = element_rect(fill = "#0a2248", colour = "#0a2248"))

p3 <- ggplot() +
  geom_polygon(data = uk,
               aes(x = long, y = lat, group = group),
               fill = "#a9b7d2", colour = "#a9b7d2") +
  coord_map("gilbert", xlim = c(-7.75, 0), ylim = c(54.6, 59.6)) +
  geom_point(data = grahams, 
             aes(x = longitude, y = latitude),
             pch = 17, colour = "#ed553b", size = 3) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
        plot.background = element_rect(fill = "#0a2248", colour = "#0a2248"))

p4 <- ggplot(top5_munros,
       aes(x = name, y = height_meters)) +
  geom_col(width = 0.25, colour = "#3caea3", fill = "#3caea3") +
  geom_text(aes(x = name, y = height_meters + 10, label = paste0(height_meters, " m"), hjust = 0),
            colour = "white") +
  geom_text(aes(x = rev(seq(1.2, 5.2, 1)), y = 0, label = name, hjust = 0),
            colour = "white") +
  coord_flip() +
  ylim(c(0, 1500)) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
        plot.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
        plot.margin = margin(t = 50, b = 50),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

p5 <- ggplot(top5_corbetts,
             aes(x = name, y = height_meters)) +
  geom_col(width = 0.5, colour = "#f6d55c", fill = "#f6d55c") +
  geom_text(aes(x = name, y = height_meters + 10, label = paste0(height_meters, " m"), hjust = 0),
            colour = "white") +
  coord_flip() +
  ylim(c(0, 1500)) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
        plot.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "white"),
        panel.grid = element_blank())

p6 <- ggplot(top5_grahams,
             aes(x = name, y = height_meters)) +
  geom_col(width = 0.5, colour = "#ed553b", fill = "#ed553b") +
  geom_text(aes(x = name, y = height_meters + 10, label = paste0(height_meters, " m"), hjust = 0),
            colour = "white") +
  coord_flip() +
  ylim(c(0, 1500)) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
        plot.background = element_rect(fill = "#0a2248", colour = "#0a2248"),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "white"),
        panel.grid = element_blank())
  

p <- p1 + p2 + p3 + p4 + p5 + p6 +
  plot_layout(ncol = 3)
p
  coord_cartesian(xlim = c(-7.75, 0), ylim = c(54.6, 60.75))
  
  coord_fixed(1.3) +
  geom_point(data = d1, 
             aes(x = longitude, y = latitude, colour = type)) +
  xlim(-7.75, 0) +
  geom_hline(yintercept = c(54.6, 61))
  # geom_vline(xintercept = c(-7.75, 0))
