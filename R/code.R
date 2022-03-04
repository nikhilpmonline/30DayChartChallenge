# 30DayChartChallenge

# Last updated 2022-03-04

# Load packages ----

library(tidyverse)
library(munro)

# Comparisons ----

# 1 - Part-to-whole

munros <- munro::munros %>% 
  mutate(unique_name = paste(1:nrow(.), name, sep = " - ")) %>% 
  select(unique_name, county) %>% 
  separate()

munros


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