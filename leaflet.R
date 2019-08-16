library(leaflet)
library(htmltools)
library(htmlwidgets)
library(dplyr)

heatPlugin <- htmlDependency("Leaflet.heat", "99.99.99",
                             src = c(href = "http://leaflet.github.io/Leaflet.heat/dist/"),
                             script = "leaflet-heat.js"
)

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

leaflet() %>% addTiles() %>%
  fitBounds(min(quakes$long), min(quakes$lat), max(quakes$long),     max(quakes$lat)) %>%
  registerPlugin(heatPlugin) %>%
  onRender("function(el, x, data) {
           data = HTMLWidgets.dataframeToD3(data);
           data = data.map(function(val) { return [val.lat, val.long, val.mag*100]; });
           L.heatLayer(data, {radius: 25}).addTo(this);
           }", data = quakes %>% select(lat, long, mag))






leaflet(quakes) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( 178, -20, 5 ) %>%
  addHeatmap(
    lng = ~long, lat = ~lat, intensity = ~mag,
    blur = 20, max = 0.05, radius = 15
  )

## for more examples see
# browseURL(system.file("examples/heatmaps.R", package = "leaflet.extras"))
kml <- readr::read_file(
  system.file("examples/data/kml/crimes.kml.zip", package = "leaflet.extras")
)

leaflet() %>%
  setView(-77.0369, 38.9072, 12) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addKMLHeatmap(kml, radius = 7) %>%
  addKML(
    kml,
    markerType = "circleMarker",
    stroke = FALSE, fillColor = "black", fillOpacity = 1,
    markerOptions = markerOptions(radius = 1))

## for more examples see
# browseURL(system.file("examples/KML.R", package = "leaflet.extras"))