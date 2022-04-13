# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 14 : 3-dimensional
# Last updated 2022-04-14

# http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization#text3d-and-scatter3d
# https://stackoverflow.com/questions/38905661/r-white-plot-elements-on-black-background

# Load packages ----

library(palmerpenguins)
library(plot3D)
library(showtext)
library(tidyverse)

# Load fonts ----

# font_add_google("Righteous", "Righteous")
# showtext_auto()

# Import data ----

penguins <- palmerpenguins::penguins

# Data wrangling ----

points <- penguins %>% 
  select(species, bill_length_mm:flipper_length_mm)

centroids <- penguins %>% 
  group_by(species) %>% 
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            mean_bill_depth = mean(bill_depth_mm, na.rm = TRUE),
            mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE))

# Create plot ----

par(bg = "#161b33", fg = "white",
    mfrow = c(1, 2))

plot3D::scatter3D(x = points$bill_length_mm,
                        y = points$bill_depth_mm,
                        z = points$flipper_length_mm, colvar = as.integer(points$species),
                        xlab = "Bill length",
                        ylab = "Bill depth",
                        zlab = "Flipper length",
                  xlim = c(32, 60),
                  ylim = c(13, 22),
                  zlim = c(171, 232),
                        type = "p",
                        phi = 0, bty = "u",col = c("darkorange", "purple", "cyan4"),
                  colkey = FALSE,
                        pch = 19, cex = 1.5, col.axis = "#6675bd", col.panel = "#161b33", col.grid = "#6675bd")

plot3D::scatter3D(x = centroids$mean_bill_length,
                  y = centroids$mean_bill_depth,
                  z = centroids$mean_flipper_length, colvar = as.integer(centroids$species),
                  xlab = "Bill length",
                  ylab = "Bill depth",
                  zlab = "Flipper length",
                  xlim = c(32, 60),
                  ylim = c(13, 22),
                  zlim = c(171, 232),
                  type = "h",
                  colkey = FALSE,
                  phi = 0, bty = "u", col = c("darkorange", "purple", "cyan4"),
                  pch = 19, cex = 2, col.axis = "#6675bd", col.panel = "#161b33", col.grid = "#6675bd")

mtext("My 'Title' in a strange place", side = 3, line = -2, outer = TRUE)

p <- ggtern(data = d1, aes(x = bill_length_ratio, y = bill_depth_ratio, z = flipper_length_ratio)) +
  geom_point(aes(colour = species),
             show.legend = FALSE, size = 1) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_zoom_center(0.55) +
  annotate("text", x = 0.3, y = 0.5, z = 0.4, label = "Adelie", colour = "darkorange", family = "Righteous", size = 10) +
  annotate("text", x = 0.5, y = 0.5, z = 0.4, label = "Chinstrap", colour = "purple", family = "Righteous", size = 10) +
  annotate("text", x = 0.5, y = 0.5, z = 0.8, label = "Gentoo", colour = "cyan4", family = "Righteous", size = 10) +
  labs(x = "Bill length", y = "Bill depth", z = "Flipper length") +
  ggtitle("How to differenciate penguins",
          subtitle = "Visualisation: Jonathan Kitt | Data source: Palmer Penguins | #30DayChartChallenge 2022 | Day 8: 3-dimensional") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#161b33", colour = "#161b33"),
        plot.background = element_rect(fill = "#161b33", colour = "#161b33"),
        axis.line = element_line(colour = "lightblue"),
        axis.title = element_text(family = "Righteous", colour = "lightblue", size = 22),
        plot.title = element_text(family = "Righteous", colour = "lightblue", size = 75, hjust = 0.5,
                                  margin = margin(t = 10)),
        plot.subtitle = element_text(family = "Righteous", colour = "lightblue", size = 20, hjust = 0.5)) +
  theme_hidelabels() +
  theme_hideticks() +
  theme_hidegrid()

# Save plot ----
  
ggsave("2022/plots/finished/14_3dimensional.png", p, dpi = 320, width = 12, height = 6)

# Test ----

# https://github.com/AckerDWM/gg3Ds

points <- penguins %>% 
  select(species, bill_length_mm:flipper_length_mm) %>% 
  mutate(data_type = "point",
         plot_type = "p")

centroids <- penguins %>% 
  group_by(species) %>% 
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            mean_bill_depth = mean(bill_depth_mm, na.rm = TRUE),
            mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE))

d1 <- rbind(points, centroids) %>% 
  filter(!is.na(bill_length_mm))

par(bg = "#161b33", fg = "white")
plot3D::scatter3D_fancy(x = d1$mean_bill_length,
                  y = d1$mean_bill_depth,
                  z = d1$mean_flipper_length, colvar = as.integer(d1$species),
                  xlab = "Bill length",
                  ylab = "Bill depth",
                  zlab = "Flipper length",
                  type = d1$plot_type,
                  phi = 0, theta = 45, bty = "u", col = c("darkorange", "purple", "cyan4"),
                  pch = 19, cex = 1.5, col.axis = "#6675bd", col.panel = "#161b33", col.grid = "#6675bd")

library(gg3D)

ggplot(iris, aes(x=Petal.Width, y=Sepal.Width, z=Petal.Length, color=Species)) + 
  theme_void() +
  axes_3D() +
  stat_3D()

ggplot(data = penguins,
       aes(x = bill_length_mm, y = flipper_length_mm, z = bill_depth_mm,
           color = species)) +
  theme_void() +
  axes_3D() +
  stat_3D()

par(bg = "#161b33", fg = "white")
plot3D::scatter3D(x = penguins$bill_length_mm,
                  y = penguins$flipper_length_mm,
                  z = penguins$bill_depth_mm, colvar = as.integer(penguins$species),
                  xlab = "Bill length",
                  ylab = "Flipper length",
                  zlab = "Bill depth",
                  type = "p",
                  phi = 0, bty = "u", col = c("darkorange", "purple", "cyan4"), fill = "#161b33",
                  pch = 21, cex = 1.5, col.axis = "#6675bd", col.panel = "#161b33", col.grid = "#6675bd")


ggsave("2022/plots/14_3dimensional_1.png", p, dpi = 320, width = 12, height = 6)



library(patchwork)
library(gg3D)
p1 + plot_spacer()
