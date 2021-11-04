library(osmdata)
library(sf)

ec = ne_countries(scale = "large", country = "Ecuador", returnclass = "sf")

mangrove = opq(getbb("Ecuador", featuretype = "country")) %>% 
  add_osm_feature(key = "wetland", value = "mangrove") %>% 
  osmdata_sf() %>% 
  unique_osmdata()

shrimp = opq(getbb("Ecuador", featuretype = "country")) %>% 
  add_osm_feature(key = "aquaculture", value = "shrimp") %>% 
  osmdata_sf() %>% 
  unique_osmdata()

mangrove_col = bind_rows(
  mangrove$osm_lines, mangrove$osm_polygons,
  mangrove$osm_multilines, mangrove$osm_multipolygons
)

shrimp_col = bind_rows(
  shrimp$osm_polygons, shrimp$osm_multipolygons,
  shrimp$osm_lines
)

ggplot() +
  geom_sf(data = ec, fill = "white", col = NA) +
  geom_sf(data = mangrove_col, color = NA, fill = "green") +
  geom_sf(data = shrimp_col, color = NA, fill = "blue") +
  coord_sf(
    xlim = c(-80.962793, -78.774443)
  ) +
  theme_void() +
  theme(panel.background = element_rect(fill = "grey60", color = NA))
