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

# 3.2 Trends by Geography ----
gtrends_lst %>% 
    pluck("interest_by_region") %>%
    as_tibble()

states_tbl <- map_data("state") %>%
    as_tibble() %>%
    mutate(region = str_to_title(region))
states_tbl

state_trends_tbl <- gtrends_lst %>%
    pluck("interest_by_region") %>%
    left_join(states_tbl, by = c("location" = "region")) %>%
    as_tibble()


state_trends_tbl %>%
    
    ggplot(aes(long, lat)) +
    geom_polygon(aes(group = group, fill = hits)) +
    coord_map("albers", at0 = 45.5, lat1 = 29.5) +
    scale_fill_viridis_c() +
    theme_tq() +
    facet_wrap(~ keyword, nrow = 1) +
    labs(title = "Keyword Trends - US")


gtrends_lst %>% names()
gtrends_lst %>% pluck("interest_by_dma") %>% as_tibble() %>% View()
gtrends_lst %>% pluck("related_queries") %>% as_tibble() %>% View()

