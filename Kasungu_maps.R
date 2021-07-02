library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(ggmap)
library(inlmisc)
# devtools::install_github('byzheng/leaflet')


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
  setView(lng = 33.48041, lat = -13.03579, zoom = 8) |>
  addMarkers(lng = kasungu_hospitals$LONGITU, 
             lat = kasungu_hospitals$LATITUD,
             popup = kasungu_hospitals$Names,
             group = 'circles') |>
  inlmisc::AddSearchButton(map, group = 'kasungu_hospitals', zoom = 8,
                           textPlaceholder = "Search here")

# Alternatively
map1 <- leaflet(options = leafletOptions(
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
                   popup = paste0("<b>",kasungu_hospitals$Names, "</b>", 
                                  "<br/>", kasungu_hospitals$dr_2017, "<br/>",
                                  kasungu_hospitals$dr_2018, "<br/>",
                                  kasungu_hospitals$dr_2019),
                   #color = "#2cb42c",
                   radius = 3) |>
  addSearchOSM() |> 
  addReverseSearchOSM() 

map1

  inlmisc::AddSearchButton(map1, group = '', zoom = 8,
                           textPlaceholder = "Search here")



cities <- read.csv(
  textConnection("City,Lat,Long,Pop
                    Boston,42.3601,-71.0589,645966
                    Hartford,41.7627,-72.6743,125017
                    New York City,40.7127,-74.0059,8406000
                    Philadelphia,39.9500,-75.1667,1553000
                    Pittsburgh,40.4397,-79.9764,305841
                    Providence,41.8236,-71.4222,177994
                    "))

# Function to map icon based on region
getColor <- function(hospitals) {
  sapply(quakes$district, function(mag) {
    if(region == "central") {
      "green"
    } else if(region == "south") {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(icon = 'ios-close',
                      iconColor = 'black',
                      library = 'ion',
                      markerColor = getColor(hospitals))

leaflet(kasungu_hospitals) |> 
  addProviderTiles(providers$CartoDB.Voyager) |>
  # addAwesomeMarkers(~LONGITU, 
  #                   ~LATITUD, 
  #                   icon = icons, 
  #                   label = ~as.character(region)) |>
  # these markers will appear on your map:
  addCircleMarkers(lng = ~LONGITU, 
                   lat = ~LATITUD,
                   weight = 1, 
                   fillOpacity = 0.5,
                   stroke = FALSE,
                   radius = 5, 
                   popup = ~Names, 
                   label = ~Names, 
                   clusterOptions = markerClusterOptions(),
                   group ='circles') |> # group needs to be different than addMarkers()
  addResetMapButton() |>
  # these markers will be "invisible" on the map:
  addMarkers(data = kasungu_hospitals, 
             lng = ~LONGITU, 
             lat = ~LATITUD, 
             label = kasungu_hospitals$Names,
             popup = ~Names,
             group = 'kasungu_hospitals', # this is the group to use in addSearchFeatures()
    # make custom icon that is so small you can't see it:
    icon = makeIcon(iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
                    iconWidth = 2, 
                    iconHeight = 2)) |>
  leaflet.extras::addSearchOSM(options = searchOptions(collapsed = FALSE)) |>
  addSearchFeatures(targetGroups = 'kasungu_hospitals', # group should match addMarkers() group
                    options = searchFeaturesOptions(zoom = 12, 
                                                    openPopup = TRUE, 
                                                    firstTipSubmit = TRUE,
                                                    autoCollapse = TRUE, 
                                                    hideMarkerOnCollapse = TRUE)) |>
  addControl("<P><B>Hint!</B> Search for ...<br/><ul><li>Kasungu..</li>
            <li>Hospital</li><li>Dispensary</li><li>Health Centre</li></P>",
            position = 'bottomleft') 

