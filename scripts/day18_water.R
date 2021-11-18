library(osmdata)
library(sf)
library(tidyverse)
library(ggfx)
library(scico)
library(terra)
library(raster)
library(stars)
library(ggtext)
extrafont::loadfonts(device = "win")

elcajas = getbb("Parque Nacional Cajas",
                format_out = 'sf_polygon',
                featuretype = "national_park")

water = opq(st_bbox(elcajas)) %>% 
  add_osm_feature("natural", "water") %>% 
  osmdata_sf() %>% 
  unique_osmdata() %>% 
  trim_osmdata(elcajas)

waterpoly = bind_rows(
  water$osm_polygons,
  st_cast(water$osm_multipolygons, 'POLYGON')
)

# Hillshade
## DEMS to create hillshade
dem_1 = read_stars("data/dem/DEM_30_0000672000_0009672000.asc")
dem_2 = read_stars("data/dem/DEM_30_0000672000_0009696000.asc")
dem_3 = read_stars("data/dem/DEM_30_0000696000_0009672000.asc")
dem_4 = read_stars("data/dem/DEM_30_0000696000_0009696000.asc")
dem = st_mosaic(dem_1, dem_2, dem_3, dem_4) %>% 
  st_set_crs(24817)

alt = as(dem, "Raster") %>%
  rast() %>% 
  crop(extent(st_bbox(st_transform(elcajas, 24817))))

slope = terrain(alt, "slope", unit="radians")
aspect = terrain(alt, "aspect", unit="radians")
hill = shade(slope, aspect, 40, 270)
hillstars = st_as_stars(hill)
hillcajas = hillstars[st_transform(elcajas, st_crs(hillstars))]
demstars = st_as_stars(alt)
demcajas = demstars[st_transform(elcajas, st_crs(hillstars))] 
cajasproj = st_transform(elcajas, st_crs(hillstars))
waterproj = st_transform(waterpoly, st_crs(hillstars))

# Nice fonts on number of lakes
ggplot() +
  with_outer_glow(
    geom_stars(data = demcajas, show.legend = F), 
    colour = "#4d3400", sigma = 20
  ) +
  scico::scale_fill_scico(
    palette = 'oleron', begin = 0.5, na.value = NA
  ) +
  with_outer_glow(
    geom_sf(data = waterproj,
            fill = 'deepskyblue3', color = NA),
    colour = "white"
  ) +
  ggnewscale::new_scale_fill() +
  geom_stars(data = hillcajas, show.legend = F, alpha = 0.3) +
  scico::scale_fill_scico(
    palette = 'grayC', direction = -1, na.value = NA
  ) +
  ggspatial::annotation_scale(
    pad_y = unit(0.3, "cm"),
    style = "ticks", text_family = "Poor Richard", text_cex = 0.9,
    line_col = "#4d3400", text_col = "#4d3400", line_width = 1.5
  ) +
  annotate(
    "RichText", x = 685650, y = -304610,
    fill = NA, label.color = NA, color = "#4d3400",
    family = "Poor Richard", size = 6,
    label = "<span style='font-size:30pt'>El Cajas National Park</span><br>& its more than <span style='font-size:24pt; color:deepskyblue3'>230</span> lakes and lagoons"
  ) +
  labs(
    caption = "#30DayMapChallenge | Day 18: Water | Data © OpenStreetMap contributors & SavGIS | Created by @loreabad6"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "#9FA586",
      color = "#4d3400",
      size = 3 
    ) ,
    text = element_text(family = "Poor Richard"),
    plot.caption = element_text(
      hjust = 0.9, size = 10, color = "#4d3400",
      margin = margin(t = -15, b = 10)
    )
  )

ggsave(filename = "maps/day18.png",
       height = 20, width = 24, units = "cm")

knitr::plot_crop("maps/day18.png")
