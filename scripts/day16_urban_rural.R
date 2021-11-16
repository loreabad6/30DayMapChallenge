library(osmextract)
library(sf)
library(here)
library(rnaturalearth)
library(tidyverse)
extrafont::loadfonts(device = "win")

ec = ne_countries(scale = "large",
                  country = "Ecuador",
                  returnclass = "sf")

bbox_gal = c(
  xmin = -93.22,
  xmax = -87.638,
  ymin = -1.58,
  ymax = 1.77
) %>% st_bbox()

bbox_cont = ec %>% 
  st_cast("POLYGON") %>% 
  mutate(area = st_area(geometry)) %>% 
  top_n(1) %>% 
  st_buffer(100000) %>% 
  st_bbox()

grid_ = st_make_grid(ec, square = TRUE, n = c(500, 500))
grid = st_intersection(grid_, ec) %>% 
  st_make_valid()

amenities_ec = oe_get(
  "Ecuador",
  download_directory = here("data/osm"),
  quiet = FALSE,
  query = "SELECT *, hstore_get_value(other_tags, 'amenity') AS amenity FROM points"
)

hosp = amenities_ec %>% 
  filter(amenity == 'hospital')

hosp_union = st_union(hosp)

dist = st_distance(grid, hosp_union) %>% 
  units::set_units("km")

q = classInt::classIntervals(dist, n = 30, style = "quantile")

ec_hosp = grid %>% 
  st_as_sf() %>% 
  mutate(
    dist = dist,
    distq = cut(
      dist,
      round(q$brks),
      include.lowest = TRUE,
      right = TRUE
    )
  )

main = ggplot() +
  geom_sf(data = ec_hosp, aes(fill = distq), color = NA) +
  scico::scale_fill_scico_d(
    "Distance to the nearest hospital [km]",
    palette = "bamako", direction = -1,
    labels = c(
      as.character(round(q$brks)[1]),
      rep("", 7),
      as.character(round(q$brks)[9]),
      rep("", 7),
      as.character(round(q$brks)[17]),
      rep("", 6),
      as.character(round(q$brks)[25]),
      rep("", 6),
      as.character(round(q$brks)[31])
    ),
    guide = guide_bins(
      title.position = "top",
      axis = FALSE,
      show.limits = TRUE,
      keywidth = grid::unit(4, "mm"),
      keyheight = grid::unit(3, "mm")
    )
  ) +
  labs(
    caption = "#30DayMapChallenge | Day 16: Urban/Rural | Data: (c) OpenStreetMap contributors | Created by @loreabad6"
  ) +
  coord_sf(
    xlim = c(bbox_cont["xmin"], bbox_cont["xmax"]),
    ylim = c(bbox_cont["ymin"], bbox_cont["ymax"]),
  ) +
  theme_void() +
  theme(
    plot.caption = element_text(
      hjust = 0.5,  margin = margin(t = -20, b = 10),
      size = 9
    ),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = c(0.4, 0.90),
    legend.direction = "horizontal", 
    panel.background = element_rect(fill = "#F0ea9f", color = NA),
    text = element_text(family = "Lucida Sans", color = "#003F4C")
  )

inset = ggplot() +
  geom_sf(data = ec_hosp, show.legend = FALSE,
          aes(fill = distq), color = NA) +
  scico::scale_fill_scico_d(
    "Distance to the nearest hospital",
    palette = "bamako", direction = -1
  ) +
  coord_sf(
    xlim = c(bbox_gal["xmin"], bbox_gal["xmax"]),
    ylim = c(bbox_gal["ymin"], bbox_gal["ymax"]),
  ) +
  theme_void()+
  theme(
    panel.background = element_rect(fill = "#ede589", color = NA)
  )

main +
  annotation_custom(
    grob = ggplotGrob(inset), 
    ymin = bbox_cont['ymin'],
    ymax = -2.5,
    xmin = -77.5,
    xmax = bbox_cont['xmax']
  )

ggsave(filename = "maps/day16.png",
       height = 20, width = 20, units = "cm")

knitr::plot_crop("maps/day16.png")
