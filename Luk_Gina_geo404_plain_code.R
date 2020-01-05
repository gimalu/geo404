# GEO 404 - Assignment 1
# Gina Lukaszczyk

# -------------------------------------------------------------------------
# Environmental Settings
# -------------------------------------------------------------------------
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

# URI -------------------------------------------------------------
URI.plz = "https://www.suche-postleitzahl.org/download_files/public/plz-gebiete.shp.zip"
URI.grass = "C:/Program Files/GRASS GIS 7.8"
URI.icon = "./data/icon/dm_icon_20.png"

# Destination Paths -----------------------------------------------
dest.plz = tempfile("plz-gebiete", fileext = c(".zip"))  # Create a temporary file
dest.plz.shp = "./data/plz/plz-gebiete.shp"
dest.dm = "./data/dm/dm_loc_json.rds"
dest.route = "./data/grass_route.rds"

# -------------------------------------------------------------------------
# Postal Codes
# -------------------------------------------------------------------------
# Download Postal Codes -------------------------------------------
if (!file.exists(dest.plz.shp)) {
  if (!file.exists(dest.plz)) {
    download.file(url = URI.plz, destfile = dest.plz)
  }
  unzip(dest.plz, exdir = 'data/plz')
}

# Read PLZ Shape Files --------------------------------------------
plz.shp = read_sf(dest.plz.shp)
aug.shp = plz.shp[which((plz.shp$plz >= 86150 &
                           plz.shp$plz <= 86199)),]

aug.center.shp = plz.shp[which((plz.shp$plz == 86150)),]
aug.center.shp = as(aug.center.shp, "Spatial")

aug.center = gCentroid(aug.center.shp)  # Get center point

temp.dist = list()

for (i in 1:length(aug.shp$plz)) {
  plz = aug.shp$plz[i]
  center.x = gCentroid(as(plz.shp[which((plz.shp$plz == plz)),], "Spatial"))
  
  p = st_sfc(st_point(c(aug.center$x, aug.center$y)),
             st_point(c(center.x$x, center.x$y)))
  dist = st_distance(p, p)[1, 2]
  temp.dist[[i]] = dist
}

temp.dist = unlist(temp.dist)
aug.shp$distance = temp.dist
aug.shp = as(aug.shp, "Spatial")


# Plot Postal Codes -----------------------------------------------
qpal = colorQuantile("Blues", aug.shp$distance, n = 4)


aug.map = leaflet() %>% setView(lng = aug.center$x,
                                lat = aug.center$y,
                                zoom = 11)

aug.map %>% addTiles() %>% addPolygons(
  data = aug.shp,
  weight = 1,
  color = ~ qpal(aug.shp$distance),
  smoothFactor = 0.5,
  opacity = 0.4,
  fillOpacity = 0.5,
  popup = paste0("<b>Postleitzahl: </b>" , "<br>",
                 aug.shp$plz),
  highlightOptions = highlightOptions(
    color = "white",
    weight = 2,
    bringToFront = TRUE
  )
)  %>% addPolygons(
  data = aug.center.shp,
  weight = 0.1,
  color = "red",
  smoothFactor = 0.5,
  opacity = 0.05,
  fillOpacity = 0.05,
  popup = paste0("<b>Postleitzahl: </b>" , "<br>",
                 aug.shp$plz),
  highlightOptions = highlightOptions(
    color = "white",
    weight = 2,
    bringToFront = TRUE
  )
)

rm(plz.shp)
rm(temp.dist)
rm(center.x)
rm(p)
rm(dist)

# -------------------------------------------------------------------------
# DM Location
# -------------------------------------------------------------------------
# Scrab Locations -------------------------------------------------
url = paste0(
  "https://www.dm.de/cms/restws/stores/find?requestingCountry=DE&",
  "countryCodes=DE%2CAT%2CBA%2CBG%2CSK%2CRS%2CHR%2CCZ%2CRO%2CSI%",
  "2CHU%2CMK&mandantId=100",
  "&bounds=",
  aug.center$y - 0.08,
  "%2C",
  aug.center$x - 0.08,
  "%7C",
  aug.center$y + 0.08,
  "%2C",
  aug.center$x + 0.08,
  "&before=false&after=false&morningHour=9&eveningHour=18&_",
  "=1479236790492"
)

dm.json = jsonlite::fromJSON(url)
saveRDS(dm.json, dest.dm)

dm.json = readRDS(dest.dm)

# Cleaning Data --------------------------------------------------
temp.address = list()
temp.x = list()
temp.y = list()

for (i in 1:length(dm.json$address$street)) {
  temp.address[[i]] = matrix(unlist(strsplit(dm.json$address$street[i], ",")), ncol =
                               2, byrow = TRUE)[1]
  temp.y[[i]] = dm.json$location[[i]][1]
  temp.x[[i]] = dm.json$location[[i]][2]
}

temp.address = unlist(temp.address)
temp.x = unlist(temp.x)
temp.y = unlist(temp.y)

# Generate DM Data -----------------------------------------------
dm.data = data.frame(
  city = dm.json$address$city,
  street = temp.address,
  plz = as.numeric(dm.json$address$plz),
  x = as.numeric(temp.x),
  y = as.numeric(temp.y),
  parking = dm.json$parkingTooltip
)

dm.sf = st_as_sf(x = dm.data,
                 coords = c("x", "y"),
                 crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

dm.sf = as(dm.sf, "Spatial")

rm(i)
rm(temp.address)
rm(temp.x)
rm(temp.y)
rm(dm.json)

# Plot DM Shops around Augsburg ----------------------------------
dm.icon = makeIcon(URI.icon,
                   iconWidth = 20,
                   iconHeight = 20)

dm.map  =  leaflet(dm.data) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addMarkers(
    lng =  ~ x,
    lat =  ~ y,
    popup = paste0(
      "<b>Adresse: </b>" ,
      "<br>",
      dm.data$street,
      "<br>",
      dm.data$plz,
      " ",
      dm.data$city,
      "<br>",
      "<b>Parkplatzsituation: </b>",
      dm.data$parking
    ),
    icon = dm.icon[1]
  ) %>% addPolygons(
    data = aug.shp,
    weight = 1,
    color = ~ qpal(aug.shp$distance),
    smoothFactor = 0.5,
    opacity = 0.4,
    fillOpacity = 0.25,
    popup = paste0("<b>Postleitzahl: </b>" , "<br>",
                   aug.shp$plz),
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    )
  )
addLegend("bottomright",
          colors = dm.icon[1],
          labels = "DM'",
          title = "DM Shops um Augsburg")
dm.map

# -------------------------------------------------------------------------
# TSP with "GRASS"
# -------------------------------------------------------------------------
b_box = st_bbox(dm.sf)
aoi.streets = opq(b_box) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf() %>%
  `[[`("osm_lines")

aoi.streets = dplyr::select(aoi.streets, osm_id)

initGRASS(
  gisBase = URI.grass,
  home = "./data/grass",
  gisDbase = "./data/grass",
  location = "augsburg",
  mapset = "PERMANENT",
  override = TRUE
)

execGRASS(
  "g.proj",
  flags = c("c", "quiet"),
  proj4 = st_crs(aoi.streets)$proj4string
)

b_box = st_bbox(aoi.streets)
execGRASS(
  "g.region",
  flags = c("quiet"),
  n = as.character(b_box["ymax"]),
  s = as.character(b_box["ymin"]),
  e = as.character(b_box["xmax"]),
  w = as.character(b_box["xmin"]),
  res = "1"
)

writeVECT(SDF = as(aoi.streets, "Spatial"), vname = "aoi_streets")
writeVECT(SDF = dm.sf, vname = "dm")

# clean street network
execGRASS(
  cmd = "v.clean",
  input = "aoi_streets",
  output = "aoi_streets_clean",
  tool = "break",
  flags = "overwrite"
)

# connect points with street network
execGRASS(
  cmd = "v.net",
  input = "aoi_streets_clean",
  output = "aoi_points_connected",
  points = "dm",
  operation = "connect",
  threshold = 0.001,
  flags = c("overwrite", "c")
)

execGRASS(
  cmd = "v.net.salesman",
  input = "aoi_points_connected",
  output = "tsp_result",
  center_cats = paste0("1-", nrow(dm.sf)),
  flags = c("overwrite")
)

grass.route = readVECT("tsp_result") %>%
  st_as_sf() %>%
  st_geometry()

saveRDS(grass.route, dest.route)

grass.route.map  =  leaflet(dm.data) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addMarkers(
    lng =  ~ x,
    lat =  ~ y,
    popup = paste0(
      "<b>Adresse: </b>" ,
      "<br>",
      dm.data$street,
      "<br>",
      dm.data$plz,
      " ",
      dm.data$city,
      "<br>",
      "<b>Parkplatzsituation: </b>",
      dm.data$parking
    ),
    icon = dm.icon[1]
  ) %>% addPolygons(
    data = aug.shp,
    weight = 1,
    color = ~ qpal(aug.shp$distance),
    smoothFactor = 0.5,
    opacity = 0.4,
    fillOpacity = 0.25,
    popup = paste0("<b>Postleitzahl: </b>" , "<br>",
                   aug.shp$plz),
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    )
  ) %>%
  addPolylines(
    data = grass.route,
    color = "#ff2500",
    weight = 3,
    opacity = 3
  ) %>%
  
  addLegend("bottomright",
            colors = dm.icon[1],
            labels = "DM'",
            title = "DM Shops um Augsburg")
grass.route.map
