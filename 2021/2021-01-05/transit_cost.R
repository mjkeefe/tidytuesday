# Matthew J. Keefe
# January 5, 2021
# Tidytuesday Week 2: Transit Cost Beijing


# Load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(gghighlight)
library(scales)


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
setwd('2021/2021-01-05')
tt_dat <- tt_load('2021-01-05')
transit_dat <- tt_dat$transit_cost


# Explore data ------------------------------------------------------------

skimr::skim(transit_dat)

#take complete cases only
transit_dat_clean <-transit_dat %>% 
  filter(complete.cases(.))


# cost per km averaged by country
transit_dat_clean %>% 
  group_by(country) %>% 
  summarize(avg_cost_per_km = mean(cost_km_millions)) %>% 
  arrange(-avg_cost_per_km)

mypng('transit_cost_gghighlight.png')
transit_dat_clean %>% 
  ggplot(aes(y=cost_km_millions,x=stations)) +
  geom_point(size=3) +
  gghighlight(cost_km_millions > 1000, label_key = city) +
  scale_y_continuous(labels = dollar) +
  labs(x='number of stations',
       y= 'cost per km ($ millions)',
       title = 'Transit cost per km by number of stations') +
  theme(plot.title.position = 'plot',
        text = element_text(size=16))
dev.off()
