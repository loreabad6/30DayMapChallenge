library(sf)
library(here)
library(dplyr)
library(ggplot2)
library(tmap)
library(ggfx)
extrafont::loadfonts(device = "win")

contour = read_sf(here("data/igm/curva_nivel_l.shp"))
center = st_sfc(st_point(c(-78.81661,-1.468424)), crs = 4386)

buffer = st_buffer(
  st_transform(center, crs = st_crs(contour)),
  7500,
  endCapStyle = "SQUARE",
  joinStyle = "MITRE",
  mitreLimit = 2
)

crop = st_intersection(contour, buffer) 

chimborazo = crop %>% 
  mutate(linewidth = case_when(
    crv %in% seq(3800, 6200, 200) ~ 0.8,
    TRUE ~ 0.2
  ))

font = "Lydian"

ggplot() +
  geom_sf(
    data = chimborazo,
    aes(color = crv, size = linewidth),
    show.legend = FALSE
  ) +
  scale_color_viridis_c(direction = -1, option = "magma") +
  scale_size_identity() +
  labs(
    caption = "#30DayMapChallenge | Day 21: Elevation | Data: IGM | Created by @loreabad6"
  ) +
  coord_sf(expand = FALSE) +
  with_outer_glow(
    annotate(
      "text",
      y = 9832500.0, x = 742936.5,
      label = "Chimborazo",
      size = 26, 
      color = "white",
      family = font
    ),
    colour = "#FD9567FF", sigma = 5, expand = 5
  ) +
  with_outer_glow(
    annotate(
      "text",
      y = 9831000.0, x = 742936.5,
      label = "6,263 m a.s.l.",
      size = 16, 
      color = "white",
      family = font
    ),
    colour = "#FD9567FF", sigma = 3, expand = 5
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "#000004FF",
                                family = font)
  )


ggsave(filename = "maps/day21.png",
       height = 20.5, width = 20, units = "cm")

