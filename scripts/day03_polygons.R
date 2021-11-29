library(sf)
library(rnaturalearth)
library(dplyr)
library(tmap)
library(here)
library(osmdata)
extrafont::loadfonts(device = "win")

ec = ne_countries(scale = "medium", country = "Ecuador", returnclass = "sf")

bbox_cont = ec %>% 
  st_cast("POLYGON") %>% 
  mutate(area = st_area(geometry)) %>% 
  top_n(1) %>% 
  st_bbox()

reserves = opq(bbox = bbox_cont) %>% 
  add_osm_feature(key = 'leisure', value = "nature_reserve", value_exact = FALSE) %>%
  osmdata_sf() 

reserves_uq = reserves %>% 
  unique_osmdata() %>% 
  unname_osmdata_sf()

reserves_multi = reserves_uq$osm_multipolygons %>% 
  st_cast("POLYGON")
reserves_poly = reserves_uq$osm_polygons %>% 
  bind_rows(reserves_multi) %>% 
  filter(leisure == "nature_reserve")
  
tm_shape(ec, bbox = bbox_cont) +
  tm_fill(col = "#090909") +
  tm_shape(reserves_multi, bbox = bbox_cont) +
  tm_fill(
    col = "white",
    alpha = 0.2
  ) +
  tm_layout(bg.color = "black") +
  tm_credits(
    "SNAP: National System of Protected Areas \n #30DayMapChallenge | Day 3: Polygons \n Data: OpenStreetMap | Created by @loreabad6",
    col = "white", align = "right", position = "right", fontfamily = "Futura LT"
  )

tmap_save(filename = "maps/day03.png", bg = "black")
