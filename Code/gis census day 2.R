install.packages("tidycensus")
library(tidycensus)
library(tidyverse)
library(dplyr)
library(plyr)
census_api_key("e6edb0be0f3892def6b04a8e181ee49fef629bc4", install=TRUE)
m90 <- get_decennial(geography = "state", variables = "H043A001", year = 1990)

utils::View(head(m90))

m90 %>%
  ggplot(aes(x = value, y = reorder(NAME, value))) + 
  geom_point()
v15 <- load_variables(2016, "acs5", cache = TRUE)

utils::View(v15)
B24123_105E #number of environmental scientists and geologists
o1<-get_acs(geography ="tract",
            year = 2016,
            state = "OR",
            county = "Lane County",
            variables = "B24123_105E", 
            geometry = TRUE)
ggplot(o1)
o1 <- get_acs(geography = "tract", 
                                   year = 2016, # 2012-2016
                                   variables = "B00001_001E",  # Est Total
                                   state = "OR", 
                                   county = "Lane County",
                                   geometry = TRUE) # load geometry/gis info
ggplot(o1) + 
  geom_sf(aes(fill = estimate)) +
  coord_sf(datum = NA) + theme_minimal()
?geom_sf
## Install the mapview package if you haven't yet
install.packages("mapview")
library(sf)
library(mapview)

mapview(o1 %>% select(estimate), 
        col.regions = sf.colors(10), alpha = 0.1)
