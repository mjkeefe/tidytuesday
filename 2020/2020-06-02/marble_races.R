# Matthew J. Keefe
# June 3, 2020
# TidyTuesday 2020-06-02: Marble Races

# variable        type          description   
# --------        ---------     ------------
# date            character	    date of race
# race            character	    race id
# site	          character	    site of race
# source	        character	    youtube url
# marble_name	    character	    name of marble
# team_name	      character	    team name
# time_s	        double	      Time in seconds
# pole	          character	    pole position
# points	        double	      Points gained
# track_length_m	double	      track length in meters
# number_laps	    double	      number of laps
# avg_time_lap	  double	      average lap time
# host	          character	    Host of race
# notes	          character	    Notes (very few, but some notes about potential errors)


# Load Libraries ----------------------------------------------------------
library(tidyverse)


# Read in Data ------------------------------------------------------------
marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')


# Explore Data ------------------------------------------------------------
skimr::skim(marbles)


# Total Points by Marble and Team -----------------------------------------
marbles %>%
  filter(!is.na(points)) %>%
  group_by(marble_name, team_name) %>%
  summarize(points = sum(points)) %>%
  ggplot(aes(y=points,x=reorder(marble_name, points), fill = team_name)) +
  geom_col()


# Distributions of Points, Time, Track Length -----------------------------
marbles %>%
  filter(!is.na(points)) %>%
  ggplot(aes(x=points)) +
  geom_histogram(binwidth = 5)

marbles %>%
  filter(!is.na(time_s)) %>%
  ggplot(aes(x=time_s)) +
  geom_histogram(binwidth = 5)

marbles %>%
  select(race, track_length_m) %>%
  unique() %>%
  filter(!is.na(track_length_m)) %>%
  arrange(track_length_m)


# Track Length vs. Avg Lap Time -------------------------------------------
marbles %>%
  filter(!is.na(time_s)) %>%
  ggplot() +
  geom_point(aes(x=track_length_m, y = avg_time_lap, color = race)) +
  geom_smooth(aes(x=track_length_m, y = avg_time_lap))


# Investigate Long Times for S1R8 -----------------------------------------
marbles %>%
  filter(time_s > 450) %>%
  select(race, track_length_m) %>%
  unique()

marbles %>%
  mutate(race_length_m = track_length_m*number_laps) %>%
  filter(!is.na(time_s)) %>%
  ggplot() +
  geom_point(aes(x=race_length_m, y = time_s, color = race)) +
  geom_smooth(aes(x=race_length_m, y = time_s))

# it looks like S1R8 had more laps (total longer race length)



# Cumulative Points As Season Progresses ----------------------------------

marbles %>%
  filter(!is.na(points)) %>%
  mutate(date = as.Date(date, format = '%d-%b-%y')) %>%
  group_by(team_name) %>%
  arrange(date) %>%
  mutate(cumulative_points = cumsum(points)) %>%
  select(date, race, team_name, points, cumulative_points) %>%
  arrange(team_name) %>% 
  ggplot(aes(x=reorder(race,date), y = cumulative_points, color= team_name)) +
  geom_line(aes(group=team_name), lwd=1.2) +
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14,face="bold"), 
        axis.text.x = element_text(angle = 0),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14)) +
  ylab("Cumulative Points") +
  xlab("Race") +
  labs(color = "Team Name") +
  ggtitle("Cumulative Points By Team Across Season 1")
  
