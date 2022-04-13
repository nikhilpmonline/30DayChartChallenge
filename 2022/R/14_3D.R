# 30DayChartChallenge
# 2022
# Category : Relationships
# Day 14 : 3-dimensional
# Last updated 2022-04-14

# Load packages ----

library(palmerpenguins)
library(plot3D)
library(tidyverse)

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

scatter3D(x = points$bill_length_mm,
          y = points$bill_depth_mm,
          z = points$flipper_length_mm,
          xlim = c(32, 60),
          ylim = c(13, 22),
          zlim = c(171, 232),
          type = "p",
          pch = 19,
          colvar = as.integer(points$species),
          col = c("darkorange", "purple", "cyan4"),
          colkey = FALSE,
          phi = 0,
          bty = "u",
          xlab = "Bill length",
          ylab = "Bill depth",
          zlab = "Flipper length",
          cex.lab = 1.5,
          main = "Measurements for all penguins",
          col.main = "white",
          cex.main = 2,
          col.axis = "#6675bd",
          col.panel = "#161b33",
          col.grid = "#6675bd")

scatter3D(x = centroids$mean_bill_length,
          y = centroids$mean_bill_depth,
          z = centroids$mean_flipper_length,
          xlim = c(32, 60),
          ylim = c(13, 22),
          zlim = c(171, 232),
          type = "h",
          pch = 19,
          cex = 4,
          lwd = 3,
          colvar = as.integer(centroids$species),
          col = c("darkorange", "purple", "cyan4"),
          colkey = FALSE,
          phi = 0,
          bty = "u",
          xlab = "Bill length",
          ylab = "Bill depth",
          zlab = "Flipper length",
          cex.lab = 1.5,
          main = "Mean by species",
          col.main = "white",
          cex.main = 2,
          col.axis = "#6675bd",
          col.panel = "#161b33",
          col.grid = "#6675bd")
text3D(x = centroids$mean_bill_length + 1,
       y = centroids$mean_bill_depth,
       z = centroids$mean_flipper_length,
       labels = centroids$species,
       add = TRUE, colkey = TRUE, cex = 2.5,
       col = c("darkorange", "purple", "cyan4"))

mtext("Palmer penguins : three penguin species observed on three islands in the Palmer Archipelago, Antarctica",
      side = 1, line = -18, outer = TRUE, cex = 3, at = 0.5)
mtext("Gentoo penguins have longer flippers", col = "cyan4",
      side = 1, line = -14, outer = TRUE, cex = 3, at = 0.5)
mtext("Chinstrap penguins have longer bills", col = "purple",
      side = 1, line = -10, outer = TRUE, cex = 3, at = 0.5)
mtext("Adelie penguins have shorter bills and flippers", col = "darkorange",
      side = 1, line = -6, outer = TRUE, cex = 3, at = 0.5)
mtext("Visualisation : Jonathan Kitt | Data source : Palmer penguins | #30DayChartChallenge 2022 | Day 14 : 3-dimensional", col = "white",
      side = 1, line = -2, outer = TRUE, cex = 1.5, at = 0.5)
