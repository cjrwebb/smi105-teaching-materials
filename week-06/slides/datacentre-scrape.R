
library(tidyverse)
library(polite)
library(rvest)

# //*[@id="table"]

polite::bow("https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_poverty_rate")

uspov <- polite::scrape(polite::bow("https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_poverty_rate"))

us_poverty <- html_element(uspov, xpath = "/html/body/div[3]/div/div[3]/main/div[3]/div[2]/div[1]/section[2]/div[2]/table/tbody") %>%
  html_table() %>%
  janitor::clean_names() %>%
  mutate_at(vars(number:error_2), parse_number) %>%
  slice(-1)

write_csv(us_poverty, "data/us_poverty.csv")


area <- polite::scrape(polite::bow("https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_area"))

state_sizes <- html_element(area, xpath = '//*[@id="mwIQ"]') %>%
  html_table() %>%
  janitor::clean_names() %>%
  slice(-1) %>%
  select(
    state = state_territory,
    total_area_km = total_area
  ) %>%
  mutate(
    total_area_km = parse_number(total_area_km)
  )

write_csv(state_sizes, "data/state_sizes.csv")

gdp <- scrape(bow("https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_GDP"))

gdp <- html_element(gdp, xpath = '//*[@id="mwYA"]') %>%
  html_table() %>%
  select(state = 1, gdp_per_capita = 10) %>%
  slice(-1) %>%
  mutate(
    gdp_per_capita = parse_number(gdp_per_capita)
  )

write_csv(gdp, "data/gdp.csv")


