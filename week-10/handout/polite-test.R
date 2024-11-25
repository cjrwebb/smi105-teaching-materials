install.packages("polite")

library(tidyverse)
library(lubridate)
library(janitor)

library(rvest)
library(httr)
library(polite)

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_tea_consumption_per_capita"
url_reddit <- "https://reddit.com"

url_bow <- polite::bow(url)
url_bow

url_bow_reddit <- polite::bow(url_reddit)
url_bow_reddit

#----

ind_html <- polite::scrape(url_bow)

ind_html <- ind_html %>% rvest::html_nodes("table.wikitable") %>%
  html_table(fill = TRUE)

ind_html <- ind_html[[1]] %>%
  clean_names()

ind_html <- ind_html %>%
  separate(tea_consumption, "kg", into = c("tea_consumption_kg", "tea_consumption_lb"))

# tidy up the numerics
ind_html <- ind_html %>%
  mutate(
    tea_consumption_kg = parse_number(tea_consumption_kg),
    tea_consumption_lb = parse_number(tea_consumption_lb)
  )

# Get rid of footnotes/references

ind_html <- ind_html %>%
  mutate(
    country_region = str_remove_all(country_region, "\\[[0-9]\\]")
  )


# per capita, so Tea-drinking Goerge cannot screw results
ind_html %>%
  ggplot() +
  aes(x = fct_reorder(country_region, tea_consumption_kg), 
      y = tea_consumption_kg) +
  geom_col() +
  coord_flip()


