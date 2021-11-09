library(geodata)
library(here)
library(tmap)
library(stars)
library(rnaturalearth)
ec = ne_countries(scale = "large", country = "Ecuador", returnclass = "sf")

bbox_cont = ec %>% 
  st_cast("POLYGON") %>% 
  mutate(area = st_area(geometry)) %>% 
  top_n(1) %>% 
  st_buffer(100000) %>% 
  st_bbox()

tavg = worldclim_country(
  "Ecuador", "tavg",
  path = here("data/wordclim/")
)

tavg_stars = st_as_stars(tavg)

tavg_ec = tavg_stars[ec]

tm_shape(tavg_ec, bbox = bbox_cont) +
  tm_raster()
