# load necessary libraries
library(tidyverse)
library(sf)
library(here)
library(plotly)

# read in data

# new creek geometry
new_creek_geo <- read_sf(here("data", "new_creek.geojson")) 

# transform new_creek geo to leaflet projection
new_creek_geo <- st_transform(new_creek_geo, crs = '+proj=longlat +datum=WGS84')

# read in summary data (yearly number of times ssm was exceeded and max)
ssm <- read_sf(here("data", "summaries", "max.geojson"))
# add long and lat coords back in (are currently stored as geometries)
ssm <- cbind(ssm, st_coordinates(ssm))

#                  Segmentize New Creek Geo                   ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create one single linestring feature from geometry
new_creek_seg <- st_segmentize(new_creek_geo, dfMaxLength = 50)

## break into individual 2-point segments
# creates coordinates from the linestring we created
coords <- st_coordinates(ssm)[, 1:2]

# takes all the coordinates and makes them into their own, little linestrings (line between each two coords)
segs <- lapply(1:(nrow(coords) - 1), function(i) st_linestring(coords[i:(i+1), ]))

# create an sf object from those little linestrings, using the same CRS a our original geo
segs <- st_sf(geometry = st_sfc(segs, crs = st_crs(new_creek_geo)))

# calculate midpoint of each tiny linestring (stored as a matrix, NOT an sf object yet)
mid_coords <- (coords[-nrow(coords), ] + coords[-1, ]) / 2

# use the midpoints, and create sf objects from them (using st_point)
mids <- st_sf(geometry = st_sfc(lapply(1:nrow(mid_coords), function(i)
  st_point(mid_coords[i, ])), crs = st_crs(new_creek_geo)))

# find the nearest sample to the midpoints
nearest <- st_nearest_feature(mids, ssm)

# add a column for bacteria concentration based on the index of the nearest sample site
segs$conc <- ssm$yearly_max[nearest]


#                    Prepare for plotting                     ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create list of years
years <- sort(unique(ssm$year))

# assign color palette
pal <- colorNumeric(rev(RColorBrewer::brewer.pal(10, "RdBu")), domain = range(ssm$yearly_max, na.rm = TRUE))

# identify the nearest
nearest_sites <- st_nearest_feature(mids, ssm[ssm$year == years[1], ])

#                            Plot                             ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# lines (unchanged, but keep result in `plot`)
plot <- plot_ly()
for (yr in years) {
  samples_yr <- ssm[ssm$year == yr, ]
  conc <- samples_yr$yearly_max[nearest]
  for (i in seq_len(nrow(coords) - 1)) {
    seg <- coords[i:(i + 1), ]
    plot <- plot |>
      add_trace(type = "scattermapbox", mode = "lines",
                lat = seg[, 2], lon = seg[, 1],
                line = list(color = rev(pal(conc[i])), width = 4),
                frame = yr, showlegend = FALSE, hoverinfo = "skip")
  }
}

# outline points — chain onto same `plot`
# plot <- plot |>
#   add_trace(data = ssm, lat = ~Y, lon = ~X, frame = ~year,
#             type = "scattermapbox", mode = "markers",
#             marker = list(size = 18, color = "DarkSlateGrey"),
#             showlegend = FALSE)

# colored points — per-row color, chain onto same `plot`
plot <- plot |>
  add_trace(data = ssm, lat = ~Y, lon = ~X, frame = ~year,
            type = "scattermapbox", mode = "markers",
            marker = list(size = 14, color = ~pal(yearly_max)),
            text = ~paste0("<b>", site, "</b><br>Times Exceeded SSM: ", yearly_max),
            hoverinfo = "text", showlegend = FALSE)

# # legend
# for (lvl in 1:10) {
#   plot <- plot |>
#     add_trace(lat = 0, lon = 0, type = "scattermapbox", mode = "markers",
#               marker = list(size = 14, color = pal(lvl)),
#               name = as.character(lvl), showlegend = TRUE, hoverinfo = "skip")
# }

plot <- plot %>% 
  layout(
    title = "New Creek: Yearly Max at Each Site (2023-2025)",
    
    # add plot spacing
    margin = list(
      t = 150,  
      b = 50,  
      l = 50,   
      r = 50   
    ),
    
    # set basemap, initial zoom
    mapbox = list(
      style = "open-street-map",
      center = list(lat = 41.12076, lon = -73.3151),
      zoom = 13
    ),
    
    # background color and font color
    paper_bgcolor = "white",
    font = list(color = "black")
  ) %>% 
  
  # changes speed of animation when "play" is hit
  animation_opts(frame = 1000)

plot

