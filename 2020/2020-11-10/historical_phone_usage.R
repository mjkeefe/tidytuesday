# Matthew J. Keefe
# November 8, 2020
# Tidytuesday Week 45: IKEA Furniture


# Load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(gridExtra)
library(grid)
library(png)
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

tt_dat <- tt_load('2020-11-10')
mobile_dat <- tt_dat$mobile
landline_dat <- tt_dat$landline


# Explore data ------------------------------------------------------------
skimr::skim(mobile_dat)

