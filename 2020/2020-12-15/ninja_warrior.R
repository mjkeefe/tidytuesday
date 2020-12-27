# Matthew J. Keefe
# December 15, 2020
# Tidytuesday Week 50: Ninja Warrior


# Load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(modeest)
library(sentimentr)

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

mypng('ninja_warrior_sentiment.png')
nw_dat %>% 
  distinct(obstacle_name) %>% 
  mutate(obstacle_sentiment = sentiment(obstacle_name)$sentiment) %>% 
  filter(obstacle_sentiment != 0) %>% 
  mutate(sentiment_sign = ifelse(obstacle_sentiment>0, 'positive', 'negative')) %>% 
  ggplot(aes(y=fct_reorder(obstacle_name, obstacle_sentiment), x = obstacle_sentiment)) +
  geom_linerange(aes(xmin=0,xmax=obstacle_sentiment, color=sentiment_sign), lwd=1.5) +
  geom_point(aes(color=sentiment_sign), size = 4, alpha=0.7) +
  scale_color_manual(values=c('positive'='darkgreen', 'negative'='red')) +
  theme_minimal() +
  labs(x='obstacale name sentiment',
       y='',
       title='Sentiment of Ninja Warrior Obstacle Names',
       color='') +
  theme(plot.title.position = 'plot',
        text = element_text(size=16),
        panel.grid.minor = element_blank())
dev.off()
