##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                   CREATING COLUMNS FOR YEARLY COMPARISON                 ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load necessary libraries
library(tidyverse)
library(here)
library(janitor)
library(sf)

#                        Prepare data                         ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# read in new creek data (this time in long format)
new_creek_long <- read_csv(here("data", "new_creek.csv")) %>% 
  clean_names() %>% 
  
  # ensure all bacteria levels are numeric values
  mutate(value = as.numeric(value))

# create data frame with locations of sampling sites
new_creek_loc <- data.frame(
  # sampling site IDs and lat/long
  site = c("New 0.5", "New 1", "New 1.5", "New 2", "New 3"),
  lng = c(-73.3187, -73.3169, -73.3151, -73.3138, -73.3128),
  lat = c(41.11657, 41.11884, 41.12076, 41.12334, 41.13064))

# merge new creek bact data to the sampling sites
new_creek_long <- new_creek_long %>% 
  
  # join using site name column
  left_join(new_creek_loc, by = "site") %>% 
  
  # transform into sf (geospatial) object
  st_as_sf(coords = c("lng", "lat"))

#        Calculate # of times SSM exceeded at each site       ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# set SSM maximum to be 126 MPN/100mL for E.coli and 35 MPN/100mL for Enterococci, which is the value used in Harbor Watch reports
new_creek_long <- new_creek_long %>% 
  
  # create binary column for exceeded or not (1 = exceeded)
  mutate(exceed_ssm = case_when(
    indicator == "E. coli" & value > 126 ~ 1,
    indicator == "Enterococci" & value > 35 ~ 1,
    .default = 0
  ))

# summarize by summing the number of times SSM was exceeded for each site, for each year
ssm <- new_creek_long %>% 
  group_by(site, year) %>% 
  
  # create a times exceeded column as well as
  summarize(times_exceeded = sum(exceed_ssm),
            
   # percent of times exceeded         
            percent_exceeded = times_exceeded / n())

# in this case New Creek was sampled 10 times every year, so number and percent are similar

#            Calculate max bact level at each site            ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

max <- new_creek_long %>% 
  
  # create column with the maximum for each site, for each year
  group_by(site, year) %>% 
  
  summarize(yearly_max = max(value, na.rm = TRUE))

#                  Save both dfs as csv files                 ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

st_write(ssm, here("data", "summaries", "ssm.geojson"))
st_write(max, here("data", "summaries", "max.geojson"))





