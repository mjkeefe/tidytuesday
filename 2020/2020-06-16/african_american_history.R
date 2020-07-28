# Matthew J. Keefe
# June 16, 2020
# TidyTuesday exploring African American history


# Load Libraries ----------------------------------------------------------

library(tidyverse)


# Read in Data ------------------------------------------------------------

blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')
slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')


# Explore Census Data -----------------------------------------------------

census %>% 
  mutate(pct_black_slaves = 100*black_slaves/total) %>% 
  filter(region == 'USA Total') %>% 
  ggplot(aes(y=pct_black_slaves, x=year)) +
  geom_line()

census %>%
  ggplot() +
  geom_map(aes(fill = total, map = states_map))
