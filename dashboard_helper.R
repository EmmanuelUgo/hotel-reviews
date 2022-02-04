
library(tidyverse)
library(tidytext)
library(lubridate)
library(wordcloud)
library(reshape2)
library(showtext)


font_add_google(name = "quicksand",  family = "quicksand")

tidy_reviews <- vroom::vroom("tidy_reviews.csv", show_col_types = FALSE)

all_location <- tidy_reviews %>%
  distinct(location) %>%
  arrange(location) %>%
  pull()

pick_hotel <- function(locations) {
  tidy_reviews %>% 
    filter(location %in% locations) %>% 
    distinct(hotel_restaurant_name) %>%
    arrange(hotel_restaurant_name) %>%
    pull()
}

theme_set(hrbrthemes::theme_tinyhand(base_family = "quicksand"))