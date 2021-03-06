library(sf)
library(rnaturalearth)
library(dplyr)
library(tmap)
library(here)
extrafont::loadfonts(device = "win")

ec = ne_countries(scale = "medium", country = "Ecuador", returnclass = "sf")
rail = read_sf(here("data/igm/ferrocarril.shp"))
road = read_sf(here("data/igm/vias.shp"))

bbox_cont = ec %>% 
  st_cast("POLYGON") %>% 
  mutate(area = st_area(geometry)) %>% 
  top_n(1) %>% 
  st_bbox()

tm_shape(road, bbox = bbox_cont) +
  tm_lines(
    col = "grey80",
    alpha = 0.5, 
    lwd = 0.4
  ) +
  tm_shape(rail) +
  tm_lines(
    col = "white",
    alpha = 0.8,
    lwd = 1.8
  ) +
  tm_layout(bg.color = "black") +
  tm_credits(
    "Ferrocarril al Cielo: Ecuadorian Railway \n #30DayMapChallenge | Day 2: Lines \n Data from IGM | Created by @loreabad6",
    col = "white", align = "right", position = "right", fontfamily = "Futura LT"
  )

tmap_save(filename = "maps/day02.png", bg = "black")
