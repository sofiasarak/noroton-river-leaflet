
# load necessary libraries
library(tidyverse)
library(sf)
library(here)
library(plotly)

# read in summary data (yearly number of times ssm was exceeded and max)
ssm <- read_sf(here("data", "summaries", "ssm.geojson"))
max <- read_sf(here("data", "summaries", "max.geojson"))

# add long and lat coords back in (are currently stored as geometries)
ssm <- cbind(ssm, st_coordinates(ssm))
max <- cbind(max, st_coordinates(max))

# turn times exceeded into a factor variable, in order to keep consistency across plot frames
ssm$times_exceeded <- factor(ssm$times_exceeded, levels = 1:10)

# create color palette from factor levels
pal <- setNames(rev(RColorBrewer::brewer.pal(10, "RdBu")), 1:10)


#                            Plot                             ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot <- plot_ly() %>% 
  
  # first layer are all points plotted as gray and slightly larger as colored points
  # this creates an outline
  add_trace(
    data = ssm,
    lat = ~Y, lon = ~X,
    frame = ~year,
    type = "scattermapbox",
    mode = "markers",
    marker = list(
      size = 18,               # larger size to create a 2px "outline"
      color = "DarkSlateGrey"  # border color
    ),
    showlegend = FALSE
  )  %>%
  
  # plot sampling sites on top, colored by the number of times they exceeded ssm
  add_trace(
    data = ssm,
    lat = ~Y, lon = ~X,
    frame = ~year,
    type = "scattermapbox",
    mode = "markers",
    marker = list(size = 14,
                  color = ~pal[as.character(times_exceeded)],
                  line = list(width=10, color='DarkSlateGrey')),
    
    # adds text when hovering over marker
    text = ~paste0("<b>", site, "</b><br>Times Exceeded SSM: ", times_exceeded),
    hoverinfo = "text",
    showlegend = FALSE
  )

# create static legend
for (lvl in 1:10) {
  plot <- plot %>% 
    
    # add a new marker for each level of the palette
    add_trace(
      lat = 0, lon = 0,          # dummy coord, marker hidden via opacity
      type = "scattermapbox",
      mode = "markers",
      marker = list(size = 14, color = pal[[as.character(lvl)]]),
      name = as.character(lvl),
      showlegend = TRUE,
      hoverinfo = "skip"
    )
}

plot <- plot %>% 
  layout(
    title = "New Creek: Number of Times SSM Exceeded at Each Site (2023-2025)",
    
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
