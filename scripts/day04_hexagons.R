library(sf)
library(osmdata)
library(tidyverse)
library(ggfx)
extrafont::loadfonts(device = "win")

ec = getbb("Ecuador", featuretype = "country",
           format_out = "sf_polygon")

morona = getbb("Morona Santiago", featuretype = "state",
               format_out = "sf_polygon")

bbox = st_bbox(morona)

hex = st_make_grid(morona, square = F, n = c(100, 100))

grid = st_intersection(hex, morona)

wf = opq(st_bbox(morona)) %>% 
  add_osm_feature(key = "waterway", value = "waterfall") %>% 
  osmdata_sf() %>% 
  unique_osmdata()
rivers = opq(st_bbox(morona)) %>% 
  add_osm_feature(key = "waterway", value = "river") %>% 
  osmdata_sf() %>% 
  unique_osmdata()

rivers_lines = rivers$osm_multilines %>% 
  st_cast("LINESTRING") %>% 
  bind_rows(rivers$osm_lines)

wf_points = wf$osm_points %>% 
  st_transform(st_crs(grid))

wf_join = wf_points %>% 
  st_union()

dist = st_distance(grid, wf_join) %>% 
  units::set_units("km")

q = classInt::classIntervals(dist, n = 9, style = "quantile")

morona_wf = grid %>% 
  st_as_sf() %>% 
  mutate(
    dist = dist,
    distq = cut(
      dist,
      round(q$brks),
      include.lowest = TRUE, right = T
    )
  )

ggplot() +
  geom_sf(
    data = ec$multipolygon,
    fill = 'grey90', color = NA
  ) +
  with_inner_glow(
    geom_sf(
      data = morona_wf,
      aes(fill = distq),
      color = NA, alpha = 0.9
    ),
    colour = "white",
    sigma = 15
  ) +
  geom_sf(
    data = rivers_lines, alpha = 0.6,
    color = "white", size = 0.3
  ) +
  # scale_fill_brewer(
  scico::scale_fill_scico_d(
    "Distance to the nearest waterfall (km)",
    palette = "davos", direction = 1, end = 0.8,
    guide = guide_bins(
      title.position = "top",
      axis = FALSE,
      show.limits = TRUE
    )
  ) +
  coord_sf(
    xlim = c(bbox['xmin'], bbox['xmax']),
    ylim = c(bbox['ymin'], bbox['ymax'])
  ) +
  labs(
    title = "Cascadas de\nMorona Santiago",
    caption = "#30DayMapChallenge | Day 4: Hexagons | Data: OpenStreetMap | Created by @loreabad6"
  ) +
  theme_void() +
  theme( 
    plot.title = element_text(
      hjust = 0.9, size = 35,
      margin = margin(b = -100)
    ),
    plot.caption = element_text(
      hjust = 0.5,  margin = margin(t = -10),
      family = "sans", size = 7
    ),
    legend.position = c(0.8, 0.1),
    legend.direction = "horizontal", 
    panel.background = element_rect(fill = "grey80", color = NA),
    text = element_text(family = "Dancing Script", color = "#2C194C")
  )

ggsave(filename = "maps/day04.png",
       height = 20, width = 20, units = "cm")

knitr::plot_crop("maps/day04.png")
