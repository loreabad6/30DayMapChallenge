library(osmdata)
library(sf)
library(sfnetworks)
library(tidygraph)
library(dplyr)
library(ggplot2)

bus = opq("Quito, Ecuador") %>% 
  add_osm_feature(key = "route",
                  value = c("bus", "subway", "trolleybus")) %>% 
  osmdata_sf() %>% 
  unique_osmdata()

bus = bus %>% 
  unname_osmdata_sf()

buslines = bind_rows(
  bus$osm_lines,
  st_cast(bus$osm_multilines, "LINESTRING")
) %>% 
  select(name)

bussfn = as_sfnetwork(buslines, directed = FALSE) 

center = c(-78.51436,-0.22308) %>% 
  st_point() %>% 
  st_sfc(crs = 4326) 

sfn = bussfn %>% 
  activate("edges") %>%
  mutate(weight = edge_length()) %>% 
  mutate(speed = units::set_units(11, "m/s")) %>%
  mutate(time = weight / speed) %>% 
  activate("nodes") %>% 
  filter(group_components() == 1) 

# Define the threshold values (in meters).
# Define also the colors to plot the neighborhoods in.
thresholds = seq(3600, 600, -600)

nbh = sfn %>% 
  activate("edges") %>% 
  mutate(neigh = units::set_units(NA, 'min')) %>% 
  activate("nodes") %>% 
  mutate(neigh = units::set_units(NA, 'min'))

for (i in c(1:length(thresholds))) {
  nbh = nbh %>% 
    activate('edges') %>% 
    morph(to_spatial_neighborhood, center, thresholds[i],
          weights = 'time') %>% 
    mutate(neigh = units::as_units(thresholds[i]/60, 'min')) %>% 
    activate("nodes") %>% 
    mutate(neigh = units::as_units(thresholds[i]/60, 'min')) %>% 
    unmorph()
}

ggplot() +
  geom_sf(
    data = st_as_sf(nbh, 'edges'), 
    aes(
      color = as.numeric(neigh)
    )
  ) +
  scale_color_distiller(
    "Travel time (min)",
    palette = "Greys",
    guide = guide_colorbar(
      title.position = "top",
      barheight = grid::unit(3, 'mm'), 
      ticks = F
    )
  ) +
  labs(
    title = "\nQuito public\ntransport network",
    caption = "#30DayMapChallenge | Day 10: Monochrome\nData: © OpenStreetMap contributors | Created by @loreabad6\n"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Futura LT"),
    plot.title = element_text(
      hjust = 0.5, size = 16
    ),
    plot.caption = element_text(
      hjust = 0.5, size = 7
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.background = element_rect(color = "black", fill = "white", size = 5)
  )

ggsave(filename = "maps/day09.png",
       height = 20, width = 10, units = "cm")

knitr::plot_crop("maps/day09.png")
