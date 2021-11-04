library(osmdata)
library(sf)

mangrove = opq(getbb("Ecuador", featuretype = "country")) %>% 
  # add_osm_feature(key = "natural", value = "wetland") %>% 
  add_osm_feature(key = "wetland", value = "mangrove") %>% 
  osmdata_sf() %>% 
  unique_osmdata()
