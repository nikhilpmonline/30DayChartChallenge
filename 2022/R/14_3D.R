# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 14 : 3-dimensional
# Last updated 2022-03-18

# https://www.arcgis.com/apps/webappviewer3d/index.html?id=6292fde2362e44679d6359fb6d2bbefe
# https://www.tylermw.com/3d-ggplots-with-rayshader/
# https://github.com/lindbrook/cholera
# https://geodacenter.github.io/data-and-lab/snow/
# https://freakonometrics.hypotheses.org/19201
# https://gist.github.com/tylermorganwall/2f3ca112b9cd13972e02e1062670b735
# https://github.com/LKremer/ggpointdensity

# Palmer penguins PCA ----

# https://allisonhorst.github.io/palmerpenguins/articles/articles/pca.html

penguin_pca %>% broom::augment(penguins)

d1 <- penguins %>% 
  rowid_to_column()

pca_fit <- prcomp(penguins)

pca_fit <- prcomp(genotyping_data)

pca_fit %>% 
  tidy(matrix = "eigenvalues") %>% 
  filter(PC %in% 1:20) %>% 
  ggplot(mapping = aes(x = PC, y = percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  ggtitle("% of variance explained") +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0, 0.01))) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank())

d1 <- augment(pca_fit, data = genotyping_data)

ggplot(d1, mapping = aes(.fittedPC1, .fittedPC2)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Genetic diversity", x = "PC1", y = "PC2") +
  theme_minimal()

# Load packages ----

library(tidyverse)
library(showtext)
# library(ggwaffle)
# library(emojifont)
library(patchwork)
library(cholera)
library(HistData)
library(rayshader)
library(ggpointdensity)

# Testing plot3D package ----

# https://allisonhorst.github.io/palmerpenguins/articles/articles/pca.html

library(corrr)
library(recipes)
library(palmerpenguins)

d1 <- penguins %>% 
  rowid_to_column() %>% 
  rename(individual = rowid) %>% 
  select(individual, bill_length_mm:body_mass_g)

pca_fit <- PCA(d1)

pca_res <- tibble(
  individual = 1:nrow(penguins),
  species = penguins$species,
  pc1 = pca_fit$ind$coord[, 1],
  pc2 = pca_fit$ind$coord[, 2],
  pc3 = pca_fit$ind$coord[, 3]) %>% 
  mutate(species_code = case_when(species == "Adelie" ~ 1,
                                  species == "Gentoo" ~ 2,
                                  species == "Chinstrap" ~ 3))

plot(pca_res$pc1, pca_res$pc2)

plot3D::scatter3D(x = pca_res$pc1, y = pca_res$pc3, z = pca_res$pc2,
                  colvar = pca_res$species_code, bty = "g", colkey = FALSE,
                  col = c("darkorange", "cyan4", "darkorchid"),
                  phi = 0, pch = 18, cex = 0.5)

penguin_recipe <-
  recipe(~., data = penguins) %>% 
  update_role(species, island, sex, new_role = "id") %>% 
  step_naomit(all_predictors()) %>% 
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") %>% 
  prep()

penguin_pca <- 
  penguin_recipe %>% 
  tidy(id = "pca") 

pca_wider <- penguin_pca %>%
  tidyr::pivot_wider(names_from = component, id_cols = terms)

penguin_pca

penguins %>% 
  dplyr::select(where(is.numeric)) %>% 
  tidyr::drop_na() %>% 
  scale() %>% 
  prcomp() %>%  
  .$rotation

penguin_recipe %>% 
  tidy(id = "pca", type = "variance") %>% 
  dplyr::filter(terms == "percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  geom_col(fill = "#b6dfe2") + 
  xlim(c(0, 5)) + 
  ylab("% of total variance")

penguin_pca %>%
  mutate(terms = tidytext::reorder_within(terms, 
                                          abs(value), 
                                          component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  ) 

install.packages("plot3D")
library(plot3D)

x <- sep.l <- iris$Sepal.Length
y <- pet.l <- iris$Petal.Length
z <- sep.w <- iris$Petal.Width

scatter3D(x, y, z, clab = c("Sepal", "Width (cm)"))

# Load fonts ----

# font_add_google("Tangerine", "Tangerine")
# showtext_auto()

# Tests ----




## =======================================================================
## With a surface
## =======================================================================

par(mfrow = c(1, 1))

# surface = volcano
M <- mesh(1:nrow(volcano), 1:ncol(volcano))

# 100 points above volcano 
N  <- 100
xs <- runif(N) * 87
ys <- runif(N) * 61
zs <- runif(N)*50 + 154

# scatter + surface
scatter3D(xs, ys, zs, ticktype = "detailed", pch = 16, 
          bty = "f", xlim = c(1, 87), ylim = c(1,61), zlim = c(94, 215), 
          surf = list(x = M$x, y = M$y, z = volcano,  
                      NAcol = "grey", shade = 0.1))

mortirolo_pass <- plotKML::readGPX(gpx.file = "2022/data/Climb Alps - Mortirolo Pass from Mazzo di Valtellina.gpx")

# Data wrangling ----


deaths <- Snow.deaths %>% 
  as_tibble() %>% 
  mutate(z = 0.5) %>% 
  mutate(description = "case") %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(description, id, x, y, z)

pumps <- Snow.pumps %>% 
  as_tibble() %>% 
  mutate(z = 0.25) %>% 
  mutate(description = "pump") %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(description, id, x, y, z)

d1 <- rbind(deaths, pumps) 


p <- ggplot() +
  stat_density_2d(data = deaths,
               aes(x = x, y = y, fill = ..density..),
               geom = "raster", contour = FALSE) +
  geom_point(data = pumps, 
             aes(x = x, y = y))


plot_gg(p, zoom = 0.60, theta = -45, phi = 30)
render_snapshot(clear = TRUE)

ggplot(deaths) +
  stat_density2d(aes(x = x, y = y, fill = ..density..),
                 geom = "raster", contour = FALSE)

scatter3D(x = d1$x, y = d1$y, z = d1$z, 
          pch = 19, cex = 0.5,
          bty = "g", phi = 5, plot = T)

p <- ggplot(deaths) +
  geom_point(aes(x, y, colour = z))

p1 <- plot_gg(p, multicore = TRUE, sunangle = 225, zoom = 0.60, phi = 30, theta = 45)
p1 <- render_snapshot(clear = TRUE)

ggsave("2022/plots/14_3_dimensional.png", p, dpi = 320, width = 12, height = 6)

deaths

streets <- Snow.streets

themeval = theme(panel.border = element_blank(), 
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 axis.line = element_blank(), 
                 axis.ticks = element_blank(),
                 axis.text.x = element_blank(), 
                 axis.text.y = element_blank(), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank(),
                 legend.key = element_blank(),
                 plot.margin = unit(c(0.5, 0, 0, 0), "cm"))
js = ggplot() + 
  geom_path(data=streets,aes(x=x, y=y, group = street), color="grey50") +
  geom_pointdensity(data = deaths,aes(x=x, y=y), size=1,adjust=0.1) +
  coord_fixed() + 
  scale_color_viridis_c() +
  theme_bw() +
  themeval 

js2 = ggplot() + 
  geom_path(data = streets, aes(x=x, y=y, group = street), color="white") +
  geom_pointdensity(data = deaths, aes(x=x, y=y), size=1, adjust=0.1) +
  coord_fixed() +
  scale_color_viridis_c() +
  theme_bw() +
  themeval 

ggheight = plot_gg(list(js,js2), multicore = TRUE, raytrace=TRUE,
                   height_aes = "color", shadow_intensity = 0.3,
                   width=8,height=7, soliddepth = -100, save_height_matrix = TRUE,
                   background = "#f5e9dc", shadowcolor= "#4f463c",windowsize=c(1000,1000))

deaths <- Snow.deaths %>%
  as_tibble() %>% 
  mutate(description = "case") %>% 
  select(description, id = case, x, y)
  
  
pumps <- Snow.pumps %>%
  as_tibble() %>% 
  mutate(description = "pump") %>% 
  select(description, id = pump, x, y)
  
d1 <- rbind(deaths, pumps)

ggplot(data = d1,
       aes(x = x, y = y, colour = description)) +
  geom_point()

cholera_plot <- ggplot(data = d1,
                       aes(x = x, y = y, colour = description)) +
  geom_point()

plot_gg(cholera_plot, width = 3.5, multicore = TRUE, windowsize = c(1400, 866), sunangle = 225,
        zoom = 0.60, phi = 30, theta = 45)


head(Snow.deaths)

plot(Snow.deaths$x, Snow.deaths$y)
plot(Snow.pumps$x, Snow.pumps$y, col = "red")

# Import data ----

nuclear_weapons <- read_csv("2022/data/nuclear-warhead-stockpiles.csv")

d1 <- nuclear_weapons %>% 
  filter(!Entity %in% c("United States", "Russia", "United Kingdom", "France"))

d2 <- nuclear_weapons %>% 
  mutate(bin = cut_interval(nuclear_weapons_stockpile, length = 10000))

ggplot(data = d2,
       aes(x = Year, y = Entity, fill = bin)) +
  geom_tile()

boxplot(nuclear_weapons$Year ~ nuclear_weapons$Code)

head(nuclear_weapons)

ggplot(data = nuclear_weapons,
       mapping = aes(x = Year, y = nuclear_weapons_stockpile,
                     colour = Entity)) +
  geom_line()

# Data wrangling ----

d1 <- tibble(
  quartet_nb = rep(1:4, each = 11),
  x = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5,
        10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5,
        10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5,
        8, 8, 8, 8, 8, 8, 8, 19, 8, 8, 8),
  y = c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68,
        9.14, 8.14, 8.74, 8.77, 9.26, 8.1, 6.13, 3.1, 9.13, 7.26, 4.74,
        7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73,
        6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.5, 5.56, 7.91, 6.89))

# Create plot ----

p1 <- ggplot(data = d1 %>% filter(quartet_nb == 1)) +
  scale_x_continuous(limits = c(4, 19), breaks = seq(4, 18, 2)) +
  scale_y_continuous(limits = c(4, 14), breaks = seq(4, 12, 2)) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13,
               colour = "#000000", size = 0.05) +
  geom_point(aes(x = x, y = y),
             size = 2, colour = "#ffa500") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#f3ddc2", colour = "#f3ddc2"),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        axis.title = element_text(family = "Tangerine", size = 50),
        axis.text = element_text(family = "Tangerine", size = 40))
  
p2 <- ggplot(data = d1 %>% filter(quartet_nb == 2)) +
  scale_x_continuous(limits = c(4, 19), breaks = seq(4, 18, 2)) +
  scale_y_continuous(limits = c(4, 14), breaks = seq(4, 12, 2)) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13,
               colour = "#000000", size = 0.05) +
  geom_point(aes(x = x, y = y),
             size = 2, colour = "#0099ff") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#f3ddc2", colour = "#f3ddc2"),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        axis.title = element_text(family = "Tangerine", size = 50),
        axis.text = element_text(family = "Tangerine", size = 40))

p3 <- ggplot(data = d1 %>% filter(quartet_nb == 3)) +
  scale_x_continuous(limits = c(4, 19), breaks = seq(4, 18, 2)) +
  scale_y_continuous(limits = c(4, 14), breaks = seq(4, 12, 2)) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13,
               colour = "#000000", size = 0.05) +
  geom_point(aes(x = x, y = y),
             size = 2, colour = "#009e73") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#f3ddc2", colour = "#f3ddc2"),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        axis.title = element_text(family = "Tangerine", size = 50),
        axis.text = element_text(family = "Tangerine", size = 40))

p4 <- ggplot(data = d1 %>% filter(quartet_nb == 4)) +
  scale_x_continuous(limits = c(4, 19), breaks = seq(4, 18, 2)) +
  scale_y_continuous(limits = c(4, 14), breaks = seq(4, 12, 2)) +
  geom_segment(x = 0, xend = 20, y = 3, yend = 13,
               colour = "#000000", size = 0.05) +
  geom_point(aes(x = x, y = y),
             size = 2, colour = "#b32db5") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#f3ddc2", colour = "#f3ddc2"),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        axis.title = element_text(family = "Tangerine", size = 50),
        axis.text = element_text(family = "Tangerine", size = 40))

p <- p1 + p2 + p3 + p4 +
  plot_annotation(
    title = "Anscombe's quartet",
    subtitle = "Four datasets with nearly identical descriptive statistics but very different distributions",
    caption = "Visualisation: Jonathan Kitt | Data source: www.cyclinglocations.com | #30DayChartChallenge 2022 | Day 3: historical",
    theme = theme(plot.title = element_text(family = "Tangerine", colour = "black", size = 120, hjust = 0.5,
                                            margin = margin(t = 20, b = 10)),
                  plot.subtitle = element_text(family = "Tangerine", colour = "black", size = 75, hjust = 0.5,
                                               margin = margin(b = 20)),
                  plot.background = element_rect(fill = "#f3ddc2", colour = "#f3ddc2"),
                  plot.caption = element_text(colour = "black", hjust = 0.5, size = 25)))

ggsave("2022/plots/03_historical.png", p, dpi = 320, width = 12, height = 6)
