library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stars)
library(raster)
library(terra)
extrafont::loadfonts(device = "win")

## Messy lulc data
withdesc = read_sf("data/sni/uso_cobertura_tierra.shp")
nodesc = read_sf("data/sni/USO_SUELO_SIERRA.shp")
dictionary = withdesc %>% 
  st_drop_geometry() %>% 
  count(codigo, descripcio) %>% 
  separate(codigo, sep = "[[:punct:]]+",
           into = c('cod1', 'cod2'), remove = FALSE) 

single_codes = dictionary %>% 
  group_by(cod1) %>% 
  summarise(desc = first(descripcio)) %>% 
  mutate(desc = case_when(
    cod1 == 'Bi' ~ 'BOSQUE INTERVENIDO',
    cod1 == 'Cd' ~ 'CEBADA',
    TRUE ~ desc
  ))


## If split with a '-' then it is 50-50% 
## If split with a '/' then it is 70-30%

nodesc %>% 
  pull(USO) %>% 
  unique()
bbox = st_bbox(
  c(
    xmin = -78.5371, xmax = -78.3796,
    ymin = -1.5404, ymax = -1.3794
  ), crs = 4326
) %>% 
  st_as_sfc() %>% 
  st_transform(st_crs(24817)) 

lulc_bbox = st_crop(nodesc, bbox)

usos = lulc_bbox %>% 
  select(USO) %>% 
  separate(USO, sep = "[[:punct:]]+",
           into = c('cod1', 'cod2'), remove = FALSE) %>% 
  left_join(single_codes) %>% 
  separate(desc, sep = "[[:blank:]]+",
           into = c('desc1', 'desc2'), remove = FALSE) %>% 
  st_transform(24817)

## DEMS to create hillshade
dem_1 = read_stars("data/dem/DEM_30_0000768000_0009840000.asc")
dem_2 = read_stars("data/dem/DEM_30_0000768000_0009816000.asc")
dem = st_mosaic(dem_1, dem_2) %>% 
  st_set_crs(24817)

alt = as(dem, "Raster") %>%
  rast() %>% 
  crop(extent(st_bbox(bbox)))

slope = terrain(alt, "slope", unit="radians")
aspect = terrain(alt, "aspect", unit="radians")
hill = shade(slope, aspect, 40, 270)
hillstars = st_as_stars(hill)

## Plot
palette = tribble(
  ~landcover, ~hex,
  "Urban area", "#e86fc6", 
  "Sandbank", "#ded08c", 
  "Forest", "#6a8241", 
  "Waterbody", "#48a9cf", 
  "Short cycle crop", "#cf7c17", 
  "Fruits", "#f2a463", 
  "Corn", "#e6c629", 
  "Snow & ice", "#d3e8eb", 
  "Paramo", "#b6c961", 
  "Pasture", "#cbd49d", 
  "Bushes", "#586901"
)

ggplot() +
  geom_sf(data = usos, aes(fill = desc1), col = NA, na.rm = TRUE) +
  scale_fill_manual(
    "Tungurahua Volcano - LULC",
    values = palette$hex, labels = palette$landcover, 
    guide = guide_legend(direction = "horizontal",
                 title.position = "top", title.hjust = 0.5)
  ) +
  ggnewscale::new_scale_fill() +
  geom_stars(data = hillstars, show.legend = F, alpha = 0.4) +
  scico::scale_fill_scico(
    palette = 'grayC', direction = -1
  ) +
  theme_void() +
  labs(
    caption = "#30DayMapChallenge | Day 17: Land | Data SNI, SavGIS | Created by @loreabad6"
  ) +
  theme(
    plot.caption = element_text(
      hjust = 0.5, 
      size = 9
    ),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 13),
    legend.position = "bottom",
    legend.box = 'vertical',
    legend.box.margin = margin(-15,3,5,3),
    # legend.direction = "horizontal", 
    panel.background = element_rect(fill = "#5c5858", color = NA),
    plot.background = element_rect(fill = "#5c5858", color = NA),
    text = element_text(family = "Rockwell", color = "white")
  )

ggsave(filename = "maps/day17.png",
       height = 20, width = 17, units = "cm")

