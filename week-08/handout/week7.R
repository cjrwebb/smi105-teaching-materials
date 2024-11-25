library(tidyverse)
install.packages("sf")
library(sf)

westminster <- read_sf(dsn = ".", layer = "westminster_const_region")
airports <- read_sf(dsn = ".", layer = "airports_gb")

housing <- read.csv("housing.csv")

h_and_w <- inner_join(westminster, housing, by = "CODE" )

ggplot() +
  geom_sf(data = westminster) +
  geom_sf(data = airports)

ggplot(data = westminster) +
  geom_sf()












