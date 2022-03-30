# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 14 : 3-dimensional
# Last updated 2022-03-18

# https://www.flerlagetwins.com/2019/08/ternary.html
# https://jserizay.com/blog/text_mining_and_sentiment_analysis_in_r/
# https://www.rayshader.com/reference/plot_gg.html

# Load packages ----

library(ggtern)
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
  mutate(bill_length_ratio = bill_length_mm / max(bill_length_mm, na.rm = TRUE),
         bill_depth_ratio = bill_depth_mm / max(bill_depth_mm, na.rm = TRUE),
         flipper_length_ratio = flipper_length_mm / max(flipper_length_mm, na.rm = TRUE)) %>% 
  select(species, island, bill_length_ratio, bill_depth_ratio, flipper_length_ratio) %>% 
  filter(!is.na(bill_depth_ratio))

# Create plot ----

ggtern(data = d1, aes(x = bill_length_ratio, y = bill_depth_ratio, z = flipper_length_ratio)) +
  geom_point(aes(colour = species),
             show.legend = FALSE) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_zoom_center(0.55) +
  annotate("text", x = 0.3, y = 0.5, z = 0.4, label = "Adelie", colour = "darkorange") +
  annotate("text", x = 0.5, y = 0.5, z = 0.4, label = "Chinstrap", colour = "purple") +
  annotate("text", x = 0.5, y = 0.5, z = 0.8, label = "Gentoo", colour = "cyan4") +
  ggtitle("Test") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#161b33", colour = "#161b33"),
        plot.background = element_rect(fill = "#161b33", colour = "#161b33"),
        axis.line = element_line(colour = "lightblue")) +
  theme_hidelabels() +
  theme_hideticks() +
  theme_hidegrid()
  # theme_hidelabels() +
  # theme_hideticks() +
  # theme_hidegrid() +



  theme(axis.line = element_line(colour = "black"))
  # theme_void() +
  theme(panel.background = element_rect(fill = "#161b33", colour = "#161b33"),
        plot.background = element_rect(fill = "#161b33", colour = "#161b33"),
        # panel.background = element_rect(fill = "#9ebfe0", colour = "#9ebfe0"),
        # plot.background = element_rect(fill = "#9ebfe0", colour = "#9ebfe0")
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.5),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(colour = "#041f32", size = 25, hjust = 0.5,
                                  family = "Righteous",
                                  margin = margin(t = 20, b = 10)),
        plot.subtitle = element_text(colour = "#041f32", size = 60, hjust = 0.5,
                                     margin = margin(t = 0, b = 40), family = "Righteous"),
        plot.caption = element_text(colour = "#041f32", hjust = 0.5,
                                    margin = margin(b = 15, t = 20), size = 25))




ggsave("2022/plots/work_in_progress/14_3dimensional_2.png", p, dpi = 320, width = 6, height = 6)

plot3d(x = penguins$bill_length_mm, y = penguins$bill_depth_mm, z = penguins$flipper_length_mm,
       type = "s", radius = 0.4)

# Testing John Snow cholera map ----

cases <- as_tibble(Snow.deaths) %>% 
  mutate(description = "case") %>% 
  select(description, id = case, x, y)

pumps <- as_tibble(Snow.pumps) %>% 
  mutate(description = "pump") %>% 
  select(description, id = pump, x, y)

p <- ggplot() +
  stat_density2d(data = cases,
                 aes(x = x, y = y, fill = ..density..),
                 geom = "raster", contour = FALSE,
                 show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "pink", high = "red") +
  geom_point(data = pumps, 
             aes(x = x, y = y))

p3d <- plot_gg(p)

render_label(p3d, text = "Broad street pump", x = 12.6, y = 11.7, z = 450)
render_snapshot()

pp <- ggplot(cases, aes(x = x, y = y)) +
  stat_density2d(aes(fill = ..density..),
                 geom = "raster", contour = FALSE,
                 show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#d0efff", high = "#03254c") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#d0efff", colour = "#d0efff"),
        plot.background = element_rect(fill = "#d0efff", colour = "#d0efff"))

p3d <- plot_gg(pp)

plot_gg(pp) %>% 
  save_png("2022/plots/14_3dimensional.png")

plot_gg(pp)
render_snapshot(title_text = "Cholera outbreak", "2022/plots/14_3dimensional.png")

plot_gg(p)

ggplot(cases, aes(x = x, y = y, fill = ..density..)) +
  stat_density2d(geom = "raster")
+
  geom_point(data = pumps, aes(x = 12.6, y = 11.7),
             colour = "red", size = 5)

plot_gg(cases_density) %>% 
  render_label(x = 12.6, y = 11.7, text = "Pump")

ggplot(cases, aes(x = x, y = y)) +
  stat_density2d(aes(fill = ..level..), geom = "polygon") +
  geom_point(data = pumps, aes(x = 12.6, y = 11.7),
             colour = "red", size = 5)

d1 <- rbind(cases, pumps)

ggplot() +
  stat_density_2d_filled(data = cases, aes(x = x, y = y)) +
  geom_point(data = pumps, aes(x = 12.6, y = 11.7),
             colour = "red", size = 5)


  # geom_point(data = cases, aes(x = x, y = y),
  #            colour = "grey80")

library(tidyverse)
library(tidytext)
library(showtext)
library(patchwork)
library(ggtern)

# Load fonts ----

font_add_google("MedievalSharp", "Medieval")
showtext_auto()

# Import data ----

fotr_raw <- read_tsv("2022/data/01 - The Fellowship Of The Ring.txt", col_names = FALSE)
tt_raw <- read_tsv("2022/data/02 - The Two Towers.txt", col_names = FALSE)
rotk_raw <- read_tsv("2022/data/03 - The Return Of The King.txt", col_names = FALSE)

# Data wrangling ----

fotr_raw <- fotr_raw %>% 
  rowid_to_column() %>% 
  select(line = rowid, text = X1) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  mutate(book = "The Fellowship of the Ring") %>% 
  select(book, everything())

tt_raw <- tt_raw%>% 
  rowid_to_column() %>% 
  select(line = rowid, text = X1) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  mutate(book = "The Two Towers") %>% 
  select(book, everything())

rotk_raw <- rotk_raw %>% 
  rowid_to_column() %>% 
  select(line = rowid, text = X1) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  mutate(book = "The Return of the King") %>% 
  select(book, everything())

word_count_fotr <- fotr_raw %>% 
  count(book, word, sort = TRUE)

word_count_tt <- tt_raw %>% 
  count(book, word, sort = TRUE)

word_count_rotk <- rotk_raw %>% 
  count(book, word, sort = TRUE)

word_count <- rbind(word_count_fotr, word_count_tt, word_count_rotk)

rm(fotr_raw, tt_raw, rotk_raw, word_count_fotr, word_count_tt, word_count_rotk)

characters <- word_count %>% 
  mutate(character = case_when(str_detect(word, "frodo") ~ "Frodo",
                               str_detect(word, "gandalf") ~ "Gandalf",
                               str_detect(word, "sam") ~ "Sam",
                               str_detect(word, "aragorn|strider|elessar") ~ "Aragorn",
                               str_detect(word, "legolas") ~ "Legolas",
                               str_detect(word, "gimli") ~ "Gimli",
                               str_detect(word, "peregrin|pippin") ~ "Peregrin",
                               str_detect(word, "meriadoc|merry") ~ "Merry",
                               str_detect(word, "boromir") ~ "Boromir")) %>% 
  filter(!word %in% c("gossamer", "sample", "flotsam", "jetsam", "sammath", "sample",
                      "merrymaking")) %>% 
  filter(!is.na(character)) %>% 
  group_by(character, book) %>% 
  summarise(total = sum(n)) %>% 
  pivot_wider(names_from = book, values_from = total) %>% 
  select(character, x = `The Fellowship of the Ring`,
         y = `The Two Towers`,
         z = `The Return of the King`) %>% 
  ungroup() %>% 
  mutate(total = rowSums(.[2:4])) %>% 
  rowid_to_column()


# Create plot ----


p <- ggtern(data = characters, aes(x, y, z, label = rowid)) + 
  # geom_text() +
  geom_mask() +
  geom_point(size = 8, colour = "#d9a404", alpha = 0.5) + 
  theme_void() +
  # theme_nomask() +
  theme_hideticks() +
  theme_hidelabels() +
  geom_text(aes(x, y, label = rowid)) +
  # theme_showarrows() +
  theme_clockwise() +
  labs(x = "The Fellowhip\nof the Ring",
       y = "The Two Towers",
       z = "The Return of\nthe King",
       title = "The Lord of the Rings",
       subtitle = "How frequently do members of the Fellowship appear in the books ?") +
  theme(axis.title = element_text(family = "Medieval", colour = "#d9a404"),
        axis.text = element_text(family = "Medieval", colour = "#d9a404"),
        axis.ticks = element_blank(),
        plot.title = element_text(family = "Medieval", hjust = 0.5, size = 100,
                                  margin = margin(t = 50), colour = "#d9a404"),
        plot.subtitle = element_text(family = "Medieval", hjust = 0.5, size = 75,
                                  margin = margin(t = 50), colour = "#d9a404"),
        plot.background = element_rect(fill = "#014023", colour = "#014023"),
        panel.background = element_rect(fill = "#014023", colour = "#014023"))


# ggsave("2022/plots/14_3dimensional.png", p, dpi = 320, width = 12, height = 6)
ggsave("2022/plots/14_3dimensional.png", p, dpi = 320, width = 6, height = 6)


+
  theme(panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"))
