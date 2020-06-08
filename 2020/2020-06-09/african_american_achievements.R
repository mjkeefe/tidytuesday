# Matthew J. Keefe
# June 8, 2020
# TidyTuesday -- African American Achievements


# Load in Libraries and Data ----------------------------------------------
library(tidyverse)
library(skimr)

firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')


# Explore Data ------------------------------------------------------------
skim(firsts)
skim(science)

head(firsts)
head(science)



# How Many AA Accomplishments per Year? -----------------------------------

firsts %>%
  group_by(year) %>%
  count() %>%
  arrange(year) %>%
  ggplot(aes(x=year, y = n)) +
  geom_line() +
  geom_smooth()



# What types of occupations did they have? --------------------------------

science %>%
  count(occupation_s) %>%
  arrange(-n) %>%
  filter(n>1) %>% 
  ggplot(aes(x=reorder(occupation_s,-n),y=n)) +
  geom_col(aes(fill=occupation_s))



# Age vs Birth Year -------------------------------------------------------

science %>% 
  mutate(age = death - birth) %>% 
  ggplot(aes(x=birth, y = age)) +
  geom_point() +
  geom_smooth()


# Word Cloud of Accomplishments by African Americans ----------------------
library(ggwordcloud)
library(tidytext)
firsts %>%
  select(accomplishment) %>%
  unnest_tokens(word, accomplishment) %>%
  bind_rows(science %>% 
              select(inventions_accomplishments) %>% 
              select(inventions_accomplishments) %>%
              unnest_tokens(word, inventions_accomplishments)) %>% 
  anti_join(stop_words, by = c("word" = "word")) %>% 
  count(word) %>%
  arrange(-n) %>%
  filter(n > 5) %>%
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(eccentricity = .9) + 
  scale_size_area(max_size = 40) +
  theme_minimal() +
  scale_color_gradient(low = "dodgerblue", high = "black")
  

