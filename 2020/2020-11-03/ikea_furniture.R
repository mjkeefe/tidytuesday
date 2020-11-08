# Matthew J. Keefe
# November 8, 2020
# Tidytuesday Week 45: IKEA Furniture


# Load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(gridExtra)
library(grid)
library(png)
library(ggridges)
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

tt_dat <- tt_load('2020-11-03')
ikea_dat <- tt_dat$ikea


# Explore data ------------------------------------------------------------

names(ikea_dat)
skimr::skim(ikea_dat)

mypng('')
ikea_dat_summary <- ikea_dat %>% 
  group_by(category) %>% 
  summarize(avg_price = mean(price),
            price_lower = mean(price) - 1.96*sd(price)/sqrt(n()),
            price_upper = mean(price) + 1.96*sd(price)/sqrt(n())) 

mypng('~/Repositories/tidytuesday/2020/2020-11-03/ikea_furniture.png')
ikea_dat_summary %>% 
  ggplot(aes(y=fct_reorder(category, avg_price, median), x=avg_price))  +
  geom_errorbar(aes(xmin=price_lower, xmax=price_upper), width=0.3, color = "#003399", lwd=1.2) +
  geom_point(fill = "#ffcc00", color ='#003399',size = 4, pch=21) +
  scale_x_continuous(labels = dollar, n.breaks=10) +
  labs(y = "",
       x = "price",
       title = 'Average price by furniture category',
       subtitle = "Mean with 95% confidence interval") +
  theme(text = element_text(size=16),
        plot.title.position = 'plot')
dev.off()
