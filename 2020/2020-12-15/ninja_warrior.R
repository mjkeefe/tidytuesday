# Matthew J. Keefe
# December 15, 2020
# Tidytuesday Week 50: Ninja Warrior


# Load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(modeest)
# Helper Functions --------------------------------------------------------
mypng <- function(x
                  , type='cairo-png'
                  , width=2.2*(40/3)/2
                  , height=2.2*(7.5)/2
                  , units='in'
                  , res=300){
  png(x
      , type='cairo-png'
      , width=width
      , height=height
      , units=units
      , res=res)
}

# Read in data ------------------------------------------------------------

tt_dat <- tt_load('2020-12-15')
nw_dat <- tt_dat$ninja_warrior



# Explore Data ------------------------------------------------------------

nw_dat %>% skimr::skim()

nw_dat %>% count(obstacle_name) %>% 
  arrange(-n)

nw_dat %>% 
  group_by(season, obstacle_name) %>% 
  summarize(count = n()) 

nw_dat %>% 
  group_by(obstacle_name) %>% 
  summarize(avg_order = mean(obstacle_order)) %>% 
  arrange(-avg_order) %>% 
  ggplot(aes(x=avg_order, y =0)) +
  geom_jitter(width = 0.1)

nw_dat %>% 
  group_by(obstacle_name, obstacle_order) %>% 
  summarize(count = n()) %>% 
  arrange(-count) %>% 
  filter(count > 5) %>% 
  ggplot(aes(x=obstacle_order, y =0)) +
  geom_jitter(width = 0.1) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  ggrepel::geom_label_repel(aes(label=obstacle_name))
