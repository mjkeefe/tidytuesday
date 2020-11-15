# Matthew J. Keefe
# November 13, 2020
# Tidytuesday Week 45: IKEA Furniture


# Load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(gridExtra)
library(grid)
library(png)
library(scales)
library(gganimate)
library(gifski)

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

tt_dat <- tt_load('2020-11-10')
mobile_dat <- tt_dat$mobile
landline_dat <- tt_dat$landline


# Explore data ------------------------------------------------------------
skimr::skim(mobile_dat)

# strong relationship between log(gdp_per_cap) and mobile subscriptions
mobile_dat %>% 
  filter(year==max(year)) %>% 
  ggplot(aes(x=log(gdp_per_cap), y=mobile_subs)) +
  geom_point() +
  geom_smooth(method='lm')

mobile_dat %>% 
  filter(year==max(year)) %>%
  mutate(mobile_subs_per_gdp = mobile_subs/gdp_per_cap) %>% 
  arrange(-mobile_subs)


# Animated bar chart ------------------------------------------------------

# prep data
mobile_formatted <- mobile_dat %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-mobile_subs),
         Value_rel = mobile_subs/mobile_subs[rank==1],
         Value_lbl = paste0(" ",round(mobile_subs))) %>%
  group_by(entity) %>% 
  filter(rank <=10) %>%
  ungroup()

static_plot <- mobile_formatted %>% ggplot(aes(rank, group = entity, 
                                       fill = as.factor(entity), color = as.factor(entity))) +
  geom_tile(aes(y = mobile_subs/2,
                height = mobile_subs,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(entity, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=mobile_subs,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

animated_plot<- static_plot + transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Mobile subscriptions per year: {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = "")

animate(animated_plot, 200, fps = 5,  width = 1200, height = 1000, 
        renderer = gifski_renderer("mobile_subs.gif"))
