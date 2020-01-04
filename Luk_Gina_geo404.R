# GEO 404 - Assignment 1
# Gina Lukaszczyk

# ------------------------------------------------------------------------------
# Environmental Settings
# ------------------------------------------------------------------------------
library("knitr")
library("sf")
library("stringr")
library("rgeos")
library("rgdal")
library("leaflet")
library("tspmeta")
library("osrm")
library("cartography")
library("rgrass7")
library("osmdata")


# URI ------------------------------------------------------------------
URI.plz = "https://www.suche-postleitzahl.org/download_files/public/plz-gebiete.shp.zip"
URI.grass = "C:/Program Files/GRASS GIS 7.8"

# Destination Paths ----------------------------------------------------
dest.plz = tempfile("plz-gebiete", fileext = c(".zip"))  # Create a temporary file
dest.dm = "./data/dm/dm_loc_json.rds"

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
rm(plz.shp)
rm(aug.shp)

# ------------------------------------------------------------------------------
# DM Location
# ------------------------------------------------------------------------------
# Scrab Locations ------------------------------------------------------
url = paste0(
  "https://www.dm.de/cms/restws/stores/find?requestingCountry=DE&", 
  "countryCodes=DE%2CAT%2CBA%2CBG%2CSK%2CRS%2CHR%2CCZ%2CRO%2CSI%", 
  "2CHU%2CMK&mandantId=100", 
  "&bounds=", aug.center$y - 0.05, 
  "%2C", aug.center$x - 0.05,
  "%7C", aug.center$y + 0.05,
  "%2C", aug.center$x + 0.05, 
  "&before=false&after=false&morningHour=9&eveningHour=18&_", 
  "=1479236790492")

dm.json = jsonlite::fromJSON(url)
saveRDS(dm.json, dest.dm)

dm.json = readRDS(dest.dm)

# Cleaning Data -------------------------------------------------------
temp.address = list()
temp.x = list()
temp.y = list()

for (i in 1:length(dm.json$address$street)){
  temp.address[[i]] = matrix(unlist(strsplit(dm.json$address$street[i], ",")), ncol=2, byrow=TRUE)[1]
  temp.y[[i]] = dm.json$location[[i]][1]
  temp.x[[i]] = dm.json$location[[i]][2]
}

temp.address = unlist(temp.address)
temp.x = unlist(temp.x)
temp.y = unlist(temp.y)

# Generate DM Data ----------------------------------------------------
dm.data = data.frame(city=dm.json$address$city, street=temp.address, 
                     plz=as.numeric(dm.json$address$plz), x=as.numeric(temp.x), 
                     y=as.numeric(temp.y),
                     parking=dm.json$parkingTooltip)

dm.sf = st_as_sf(x=dm.data, 
                 coords=c("x", "y"),
                 crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

dm.sf = as(dm.sf, "Spatial")

rm(i)
rm(temp.address)
rm(temp.x)
rm(temp.y)

# Plot DM Shops around Augsburg ---------------------------------------
dm.icon = makeIcon("./data/icon/dm_icon_20.png", iconWidth=20, iconHeight=20)

dm.map  =  leaflet(dm.data) %>%
  addProviderTiles(providers$OpenStreetMap) %>% 
  addMarkers(lng=~x, lat=~y, 
             popup=paste0("<b>Adresse: </b>" , "<br>", 
                          dm.data$street, "<br>",
                          dm.data$plz, " ", dm.data$city, "<br>",
                          "<b>Parkplatzsituation: </b>", dm.data$parking), 
             icon=dm.icon[1])%>% 
  addLegend("bottomright", colors= dm.icon[1], labels="DM'", title="DM Shops um Augsburg")
dm.map
