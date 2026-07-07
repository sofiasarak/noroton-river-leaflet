library(tidyverse)
library(leaflet)
library(sf)
library(here)

# create dataframe of Noroton sampling sites
noroton_sites <- tribble(
  ~site, ~lng, ~lat, ~bact, # bacteria concentrations from May 13, 2026
  "Noroton 1", -73.50735, 41.06093, 185,
  "Noroton 1.5", -73.5081, 41.06186, NA,
  "Noroton 1.75", -73.511, 41.07218, 185,
  "Noroton 2", -73.5155, 41.0753, 172, 
  "Noroton 3", -73.51443, 41.0953, 127, 
  "Noroton 4", -73.50982, 41.1029, 50,
  "Noroton 5", -73.5013, 41.11868, 48, 
  "Noroton 7", -73.51167, 41.14108, 52,
  "Noroton 8", -73.51421, 41.15925, 192
)

# read in noroton river geometry
noroton_river <- read_sf(here("data", "noroton.geojson")) 

# transform noroton river geo to leaflet projection
noroton_river <- st_transform(noroton_river, crs = '+proj=longlat +datum=WGS84')

# create color palette for bacteria concentrations
pal_num <- colorNumeric(
  palette = "viridis", 
  domain = noroton_sites$bact,
  reverse = TRUE
)


leaflet() %>%
  addTiles() %>%
  
  # set default zoom to Noroton 3 sampling site
  setView(lng = -73.51443, lat = 41.0953, zoom = 13) %>% 
  
  # add Noroton river geometry
  addPolygons(data = noroton_river, color = "darkblue", stroke = 1) %>% 
  
  # add Noroton sampling site markers
  addCircleMarkers(data = noroton_sites,
                   popup = ~site,
                   color = ~pal_num(bact),
                   fillColor = ~pal_num(bact),
                   fillOpacity = 0.9,
                   stroke = FALSE) %>%  
  
  # add legend
  addLegend(
    data = noroton_sites,
    pal = pal_num, 
    values = ~bact, 
    title = "Bacteria\nConcentration")
  
  