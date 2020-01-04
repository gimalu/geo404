# GEO 404 - Assignment 1
# Gina Luk

# ------------------------------------------------------------------------------
# Environmental Settings
# ------------------------------------------------------------------------------
library("knitr")
library("sf")
library("stringr")
library("rgeos")
library("rgdal")
library("ggplot2")
library("ggmap")
library("leaflet")

# URI ------------------------------------------------------------------
URI.plz = "https://www.suche-postleitzahl.org/download_files/public/plz-gebiete.shp.zip"

# Destination Paths ----------------------------------------------------
dest.plz = tempfile("plz-gebiete", fileext = c(".zip"))  # Create a temporary file

# ------------------------------------------------------------------------------
# Postal Codes
# ------------------------------------------------------------------------------
# Download Postal Codes ------------------------------------------------
if (!file.exists(dest.plz)) {
  download.file(url=URI.plz, destfile= dest.plz)
}

unzip(dest.plz, exdir='data/plz')

# Read PLZ Shape Files -------------------------------------------------
plz.shp = read_sf("./data/plz/plz-gebiete.shp")
aug.shp = plz.shp[which((plz.shp$plz >= 86150 & plz.shp$plz <= 86199)), ]
aug.shp = as(aug.shp, "Spatial")

aug.center = gCentroid(aug.shp)  # Get center point
aug.map = leaflet() %>% setView(lng = aug.center$x, lat = aug.center$y, zoom = 11)

# Plot Postal Codes ----------------------------------------------------
aug.map %>% addTiles() %>% addPolygons(data=aug.shp, weight=1, col = 'red',
                                 smoothFactor = 0.5,
                                 opacity = 0.4, fillOpacity = 0.5,
                                 highlightOptions = highlightOptions(color = "white", 
                                                                     weight = 2,
                                                                     bringToFront = TRUE))

