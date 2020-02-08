# Libraries 

# Google Trends API
library(gtrendsR)

# Data Tools
library(tidyverse)
library(lubridate)
library(tidyquant)

# File System
library(fs)

# Google Trends API

# Setting up search terms
search_terms <- c(
    "SQL",
    "python",
    "c++"
)

# Read Search Terms ----
gtrends_lst <- search_terms %>%
    gtrends(geo = "US", time = "all")

#Search Interest Over Time ----
gtrends_lst %>% 
    pluck("interest_over_time") %>% 
    mutate(hits = as.numeric(hits)) %>%
    as_tibble() %>%
    ggplot(aes(date, hits, color = keyword)) +
    geom_line() +
    geom_smooth(span = 0.3, se = FALSE) +
    theme_tq() +
    scale_color_tq() +
    labs(title = "Keyword Trends - US - Over Time")

