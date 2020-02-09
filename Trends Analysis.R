# Libraries 

# Google Trends API
library(gtrendsR)

# Data Tools
library(tidyverse)
library(lubridate)
library(tidyquant)


# Setting up search terms
search_terms <- c(
    "SQL",
    "python"
)

# Read Search Terms ----
gtrends_lst <- search_terms %>%
    gtrends(geo = "US", time = "all")

#Search Interest Over Time ----
gtrends_lst[["interest_over_time"]] %>% 
    mutate(hits = as.numeric(hits)) %>%
    as_tibble() %>%
    ggplot(aes(date, hits, color = keyword)) +
    geom_line() +
    geom_smooth(span = 0.3, se = FALSE) +
    theme_tq() +
    scale_color_tq() +
    labs(title = paste("Keyword Trends - ", as.character((gtrends_lst[[1]]$geo[1])), "- Over Time"))

# 3.2 Trends by Geography ----
gtrends_lst[["interest_by_region"]] %>%
    as_tibble()

states_tbl <- map_data("state") %>%
    as_tibble() %>%
    mutate(region = str_to_title(region))
states_tbl

state_trends_tbl <- gtrends_lst[["interest_by_region"]] %>%
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

# 3.3 Related Queries ----
gtrends_lst[["related_queries"]] %>% DataExplorer::plot_bar()

top_10_related_searches_tbl <- gtrends_lst[["related_queries"]] %>%
    as_tibble() %>%
    filter(related_queries == "top") %>%
    mutate(interest = as.numeric(subject)) %>%
    
    select(keyword, value, interest) %>%
    group_by(keyword) %>%
    arrange(desc(interest)) %>%
    slice(1:10) %>%
    ungroup() %>%
    
    mutate(value = as_factor(value) %>% fct_reorder(interest))

top_10_related_searches_tbl %>%
    ggplot(aes(value, interest, color = keyword)) +
    geom_segment(aes(xend = value, yend = 0)) +
    geom_point() +
    coord_flip() +
    facet_wrap(~ keyword, nrow = 1, scales = "free_y") +
    theme_tq() +
    scale_color_tq()
