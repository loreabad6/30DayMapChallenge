library(osmdata)
library(sf)
glac = opq(getbb("Ecuador", featuretype = "country")) %>% 
  add_osm_feature(key = "natural", value = "glacier") %>% 
  osmdata_sf() %>% 
  unique_osmdata()
