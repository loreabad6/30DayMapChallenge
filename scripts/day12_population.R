library(sf)
library(here)
library(tidyverse)
library(ggfx)
library(rcartocolor)
library(rnaturalearth)

extrafont::loadfonts(device = "win")

ec = ne_countries(scale = "medium",
                  country = "Ecuador",
                  returnclass = "sf")
pe = ne_countries(scale = "medium",
                  country = "Peru",
                  returnclass = "sf")
co = ne_countries(scale = "medium",
                  country = "Colombia",
                  returnclass = "sf")

borders = bind_rows(ec, pe, co)

## https://www.amazoniasocioambiental.org/es/mapas/#descargas
tind = read_sf(here("data/raisg/Tis_2020/Tis_TerritoriosIndigenas.shp"))
tindec = tind %>% 
  filter(pais == "Ecuador", categoria == "Nacionalidad")

bbox = tindec %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_buffer(1000) %>% 
  st_bbox()

ggplot() + 
  geom_sf(data = co, col = NA, fill = "grey80") +
  geom_sf(data = pe, col = NA, fill = "grey90") +
  with_outer_glow(
    geom_sf(data = ec, col = NA, fill = "white"),
    colour = "grey30", sigma = 30
  ) +
  with_inner_glow(
    geom_sf(data = tindec, aes(fill = etnias), 
            color = "white", size = 0.2), 
    colour = "white", sigma = 10
  ) +
  scale_fill_carto_d(
    "Ethnicity", palette = "Bold"
  ) +
  labs(
    title = "Ecuadorian\nAmazon\nIndigenous\nPopulation",
    caption = "#30DayMapChallenge | Day 12: Population | Data: RAISG | Created by @loreabad6"
  ) +
  guides(fill = guide_legend(title.hjust = 0.5, ncol = 2)) +
  coord_sf(
    xlim = c(bbox['xmin'], bbox['xmax']),
    ylim = c(bbox['ymin'], bbox['ymax'])
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.05, size = 20,
      margin = margin(t = 40, b = -100)
    ),
    plot.caption = element_text(
      hjust = 0.5, margin = margin(t = -30, b = 10),
      size = 11
    ),
    text = element_text(family = 'Dubai', size = 15),
    legend.position = c(0.8, 0.2)
  )

ggsave(filename = "maps/day12.png",
       height = 20, width = 16.5, units = "cm")

knitr::plot_crop("maps/day12.png")
