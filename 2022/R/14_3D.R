# 30DayChartChallenge
# 2022
# Category : Distributions
# Day 14 : 3-dimensional
# Last updated 2022-03-18

# https://www.flerlagetwins.com/2019/08/ternary.html
# https://jserizay.com/blog/text_mining_and_sentiment_analysis_in_r/

# Testing ternary plot ----

library(tidyverse)
library(ggtern)

x  <- data.frame(
  x1 = c( 0, 0, 1, 0.1, 0.6, 0.2 ),
  x2 = c( 0, 1, 0, 0.3, 0.2, 0.8 ),
  x3 = c( 1, 0, 0, 0.6, 0.2, 0.0 )
)

ggtern(data=x,aes(x2,x1,x3)) + 
  geom_mask() +
  geom_point(fill="red",shape=21,size=4) + 
  theme_bw() +
  theme_showarrows() +
  theme_clockwise() +
  theme(panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"))

# Data wrangling ----

library(tidyverse)
library(tidytext)

fotr_raw <- read_tsv("2022/data/01 - The Fellowship Of The Ring.txt", col_names = FALSE) %>% 
  rowid_to_column() %>% 
  select(line = rowid, text = X1) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  mutate(book = "The Fellowship of the Ring") %>% 
  select(book, everything())

word_count_fotr <- fotr_raw %>% 
  count(book, word, sort = TRUE)

tt_raw <- read_tsv("2022/data/02 - The Two Towers.txt", col_names = FALSE) %>% 
  rowid_to_column() %>% 
  select(line = rowid, text = X1) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  mutate(book = "The Two Towers") %>% 
  select(book, everything())

word_count_tt <- tt_raw %>% 
  count(book, word, sort = TRUE)

rotk_raw <- read_tsv("2022/data/03 - The Return Of The King.txt", col_names = FALSE) %>% 
  rowid_to_column() %>% 
  select(line = rowid, text = X1) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  mutate(book = "The Return of the King") %>% 
  select(book, everything())

word_count_rotk <- rotk_raw %>% 
  count(book, word, sort = TRUE)

word_count <- rbind(word_count_fotr, word_count_tt, word_count_rotk)

rm(fotr_raw, tt_raw, rotk_raw, word_count_fotr, word_count_tt, word_count_rotk)
