# Matthew J. Keefe
# September 16, 2020
# Tidytuesday Week 38: Kids Spending


# Load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(ggrepel)

# Read in data ------------------------------------------------------------

tt_dat <- tt_load('2020-09-15')
kids_dat <- tt_dat$kids


# Explore data ------------------------------------------------------------

skimr::skim(kids_dat)
kids_dat %>% distinct(variable) %>% print(n=Inf)

kids_dat %>% 
  mutate(label = if_else(year == max(year), as.character(variable), NA_character_)) %>% 
  filter(state=='Florida') %>% 
  ggplot(aes(x=year,y=inf_adj_perchild, color = variable)) +
  geom_point(size=4, alpha = 0.5) +
  geom_line() +
  geom_label_repel(aes(label=label), nudge_x = 1, na.rm=TRUE) +
  scale_x_continuous(breaks = 1997:2016) +
  scale_y_continuous(label = scales::dollar)


kids_dat %>% 
  ungroup() %>% 
  filter(state=='Florida') %>% 
  group_by(variable,state) %>% 
  summarize(raw = sum(raw, na.rm=TRUE),
            inf_adj = sum(inf_adj, na.rm=TRUE),
            inf_adj_perchild = sum(inf_adj_perchild)) %>% 
  ungroup() %>% 
  ggplot(aes(y=fct_reorder(variable, raw),x=raw)) +
  geom_col(fill = 'orange') +
  labs(x='Total spending',
       y='',
       title = 'Florida total spending by category 1997-2016') +
  scale_x_continuous(label = scales::dollar)+
  theme_minimal() +
  theme(text = element_text(size=16),
        panel.grid.minor = element_blank(),
        plot.title.position = 'plot')

kids_dat %>% 
  mutate(label = if_else(year == max(year), as.character(variable), NA_character_)) %>% 
  filter(state=='Florida', variable =='PK12ed') %>% 
  ggplot(aes(x=year,y=inf_adj_perchild)) +
  geom_point(size=4, alpha = 0.5,color = 'orange') +
  geom_line(color = 'orange') +
  scale_x_continuous(breaks = seq(1997,2016,2)) +
  scale_y_continuous(label = scales::dollar) +
  labs(x='year',
       y='Spending per child adjusted for inflation',
       title = 'Florida PK12 education spending per child') +
  theme_minimal() +
  theme(text = element_text(size=16),
        panel.grid.minor = element_blank(),
        plot.title.position = 'plot')
