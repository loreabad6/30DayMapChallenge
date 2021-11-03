library(sf)
library(rnaturalearth)
library(tidyverse)
vol = read_sf("data/sni/PELIGRO VOLCANICO.shp") 

ec = ne_countries(scale = "medium", country = "Ecuador", returnclass = "sf")
eccont = ec %>% 
  st_cast("POLYGON") %>% 
  mutate(area = st_area(geometry)) %>% 
  top_n(1) %>% 
  st_transform(st_crs(vol))
echex = st_make_grid(eccont, square = F, n = c(50,50))

ec_grid = st_intersection(echex, eccont)

volunion = vol %>% 
  filter(Posibilida == "ALTA") %>% 
  st_union() %>% 
  st_cast("MULTILINESTRING")

dist = st_distance(ec_grid, volunion)

ec_vol = ec_grid %>% 
  st_as_sf() %>% 
  mutate(dist = dist)


