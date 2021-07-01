library(leaflet)
library(tidyverse)
library(ggmap)

# Print only the names of the map tiles in the providers list 
names(providers)

# Read in data containing location of zipatala

kasungu_hospitals <- read.csv(
  here::here("data/dry_season_malaria_2017_2020.csv"))

view(kasungu_hospitals)

# Remove hospitals with missing lat/lon values
kasungu_hospitals <- kasungu_hospitals[complete.cases(kasungu_hospitals),]

# Get coordinates for Kasungu
ggmap::geocode("Kasungu") # requires an API key. Zosayenda


# Map with CartoDB tile centered on Kasungu District Hospital with zoom of 8
leaflet() |>
  addProviderTiles("CartoDB.PositronNoLabels") |>
  setView(lng = 33.48041, lat = -13.03579, zoom = 8)

# Alternatively
leaflet(options = leafletOptions(
  # Set minZoom and dragging 
  minZoom = 8, dragging = FALSE)) |>
  addProviderTiles("CartoDB.PositronNoLabels") |>
  setView(lng = kasungu_hospitals$LONGITU[17], 
          lat = kasungu_hospitals$LATITUD[17], 
          zoom = 8) |>
  addMarkers(lng = kasungu_hospitals$LONGITU, 
             lat = kasungu_hospitals$LATITUD,
             popup = kasungu_hospitals$Names) |>
  addCircleMarkers(lng = kasungu_hospitals$LONGITU, 
                   lat = kasungu_hospitals$LATITUD,
                   label = kasungu_hospitals$Names, 
                   radius = 3)


