library(sf)
library(rnaturalearth)
library(tidyverse)
library(tmap)
library(here)
extrafont::loadfonts(device = "win")

ec = ne_countries(scale = "medium", country = "Ecuador", returnclass = "sf")
city = read_sf(here("data/igm/poblados.shp"))

bbox_cont = ec %>% 
  st_cast("POLYGON") %>% 
  mutate(area = st_area(geometry)) %>% 
  top_n(1) %>% 
  st_bbox()

tm_shape(city, bbox = bbox_cont) +
  tm_dots(
    col = "white",
    size = 0.01, 
    alpha = 0.75
  ) +
  tm_layout(bg.color = "black") +
  tm_credits(
    "Populated areas in continental Ecuador \n #30DayMapChallenge | Day 1: Points \n Data from IGM | Created by @loreabad6",
    col = "white", align = "right", position = "right", fontfamily = "Futura LT"
  )

tmap_save(filename = "maps/day01.png", bg = "black")
