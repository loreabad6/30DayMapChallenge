library(dplyr)
library(forcats)
library(sf)
library(tmap)

prov = read_sf(
  "data/humdata/ecu_adm_inec_20190724_shp/ecu_admbnda_adm1_inec_20190724.shp"
) %>% 
  st_transform(32717) %>% 
  mutate(
    area = units::set_units(st_area(geometry), "km^2"),
    name = fct_reorder(
      as.factor(ADM1_ES), area)
    ) %>% 
  filter(ADM1_ES != "Zona No Delimitada")

tm = tm_shape(prov) +
  tm_fill(
    col = "area", palette = "viridis",
    legend.show = TRUE, style = "cont",
    legend.is.portrait = FALSE,
    title = "",
    legend.format = list(suffix = " km²")
  ) +
  tm_facets("name", ncol = 24) +
  tm_layout(
    panel.show = FALSE, 
    title =  "#30DayMapChallenge | Day 22: Boundaries | Data: INEC | Created by @loreabad6",
    title.size = 0.5,
    title.position = c("right", "top"),
    legend.outside.position = "top",
    legend.outside.size = 0.2,
    legend.text.size = 1,
    frame = FALSE,
    bg.color = "#fdf2ff"
  ) 

tmap_save(
  tm, 
  filename = "maps/day22.png",
  width = 35, height = 5, units = "cm",
  bg = "#fdf2ff"
)

