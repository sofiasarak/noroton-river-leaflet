# load necessary libraries
library(tidyverse)
library(sf)
library(here)
library(plotly)

#                        Read in data                         ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# new creek geometry
new_creek_geo <- read_sf(here("data", "new_creek.geojson")) 

# read in summary data (yearly number of times ssm was exceeded and max)
ssm <- read_sf(here("data", "summaries", "ssm.geojson"))

# add long and lat coords back in as distinct columns (are currently stored as geometries)
ssm <- cbind(ssm, st_coordinates(ssm)) %>% 
  st_transform(crs = st_crs(new_creek_geo))


#                  Segmentize New Creek Geo                   ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create one single linestring feature from geometry
new_creek_seg <- st_segmentize(new_creek_geo, dfMaxLength = 50)

# extract the coordinates along the entire line segment
new_creek_coords <- sf::st_coordinates(new_creek_geo) %>% as.data.frame() %>% 
  
  # convert coordinates to an sf object
  st_as_sf(coords = c("X", "Y"), crs = st_crs(new_creek_geo)) 


#        Find nearest site for each segment/coordinate        ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# for loop based on year
years <- unique(ssm$year) # save list of years

# create empty list of dfs to combine later
dflist <- list()

for (i in 1:length(years)){
  
  # take only the rows of the df that correspond to that year
  filtered <- ssm %>% filter(year == years[i])
  
  # # find the nearest sample to the midpoints; for each mid, the nearest point in ssm
  # nearest now has indices of ssm that match up to new_creek_coords
  nearest <- st_nearest_feature(new_creek_coords, filtered)
  
  # create temporary coords df (used for appending)
  new_creek_coords_temp <- new_creek_coords
  
  # add a column for bacteria concentration based on the index of the nearest sample site
  new_creek_coords_temp$percent_exceeded <- filtered$percent_exceeded[nearest]
  
  # add column for which site it corresponds to
  new_creek_coords_temp$site <- filtered$site[nearest]
  
  # add coordinates as columns, alongside year
  new_creek_coords_temp <- cbind(new_creek_coords_temp, st_coordinates(new_creek_coords_temp), year = years[i])
  
  # append the temporary coord df to the empty list
  dflist[[i]] <- new_creek_coords_temp

}

# combine list of temporary data frames into one (long format)
new_creek_coords <- do.call(rbind, dflist)


#                            Plot                             ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# initialize plotly
plot <- plot_ly()

## RIVER GEO (LINE)

plot <- plot %>%
  add_trace(data = new_creek_coords,
            lat = ~Y, lon = ~X,
            split = ~site, # treats segments differently based on the site they correspond to
            frame = ~year,
            color = ~percent_exceeded, # color by perc site exceeded ssm
            type = "scattermapbox", mode = "lines",
            line = list(width = 3), # adjust width
            showlegend = FALSE, hoverinfo = "skip",
            showscale = FALSE) # tried to get rid of second "percent_exceeded" - might have to make static legend

## OUTLINE POINTS 
# add dark gray outline to markers

plot <- plot |>
  add_trace(data = ssm, lat = ~Y, lon = ~X, frame = ~year,
            type = "scattermapbox", mode = "markers",
            marker = list(size = 18, color = "DarkSlateGrey"),
            showlegend = FALSE)

## SAMPLING POINTS
# add markers that change color based on perc exceeded (similar to lines)

plot <- plot |>
  add_trace(data = ssm, lat = ~Y, lon = ~X, frame = ~year,
            type = "scattermapbox", mode = "markers",
            color = ~percent_exceeded,
            colors = colorRamp(c("white", "darkred")), # colormap does not work :(
            marker = list(size = 14),
            
            # add hover
            text = ~paste0("<b>", site, "</b><br>% Exceeded SSM: ", percent_exceeded),
            hoverinfo = "text", showlegend = FALSE)


# legend
# for (lvl in 1:10) {
#   plot <- plot |>
#     add_trace(lat = 0, lon = 0, type = "scattermapbox", mode = "markers",
#               marker = list(size = 14, color = ~percent_exceeded, colors = "Reds"),
#               name = as.character(lvl), showlegend = TRUE, hoverinfo = "skip")
# }

## BACKGROUND, BASEMAP

plot <- plot %>% 
  layout(
    title = "New Creek: % of Times SSM Exceeded",
    
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

# call plot
plot

