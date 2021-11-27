#' Joshelyn Intriago, & David Morocho. (2021). 
#' Ecuador seismic dataset 2013-2021 (0.1) [Data set]. 
#' Zenodo. https://doi.org/10.5281/zenodo.4641566
#' 
#' Richard Styron. (2019). GEMScienceTools/gem-global-active-faults: 
#' First release of 2019 (2019.0). Zenodo. 
#' https://doi.org/10.5281/zenodo.3376300

library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggfx)

eq = read_csv('data/ig_epn/Informes_sismicos_Ecuador_ 2013_2021.csv') |> 
  drop_na(Long) |> 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) 

faults = geojsonsf::geojson_sf("https://github.com/cossatot/gem-global-active-faults/raw/master/geojson/gem_active_faults_harmonized.geojson")

countries = ne_countries(scale = "large",
                  # country = "Ecuador",
                  returnclass = "sf")
ec = countries |> 
  filter(admin == "Ecuador")
bbox = st_bbox(ec)

## Create fun title with ggfx
title = ggplot() + 
  as_reference(
    geom_polygon(aes(c(0, 1, 1), c(0, 0, 1)),
                 colour = NA, fill = 'magenta'), 
    id = "displace_map"
  ) + 
  with_displacement(
    geom_text(aes(0.5, 0.5, label = 'Seismic\nHeatmap'),
              size = 11, fontface = 'bold'), 
    x_map = ch_red("displace_map"), 
    y_map = ch_blue("displace_map"),
    x_scale = unit(0.015, 'npc'),
    id = "text"
  ) +
  with_blend(
    geom_density_2d_filled(
      aes(rnorm(1e4, 0.5, 0.3), rnorm(1e4, 0.5, 0.3)), 
      show.legend = FALSE,
    ),
    bg_layer = "text",
    blend_type = "in",
    id = "blended"
  ) + 
  scale_fill_viridis_d(option = "turbo") +
  with_shadow("blended", sigma = 3) + 
  coord_equal(xlim = c(0, 1), ylim = c(0, 1), clip = 'off') + 
  labs(x = NULL, y = NULL) +
  theme_void()


ggplot() +
  geom_sf(
    data = countries,
    fill = "grey20",  alpha = 0.5,
    color = "grey80", size = 0.1
  ) +
  geom_sf(data = eq, size = 0.01, alpha = 0.6, color = "white") +
  scale_size(range = c(0.01, 2)) +
  stat_density_2d(
    data = eq,
    mapping = aes(x = purrr::map_dbl(geometry, ~.[1]),
                         y = purrr::map_dbl(geometry, ~.[2]),
                         fill = stat(density),
                         alpha = stat(density)
                         ),
  geom = 'raster', show.legend = FALSE,
  contour = FALSE, n = 400, adjust = 0.85
) +
  scale_fill_viridis_c(option = "turbo") +
  scale_alpha(range = c(0, 1)) +
  geom_sf(
    data = faults,
    color = "grey30",
    size = 0.5,
    alpha = 0.7
  ) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    plot.caption = element_text(
      color = "white", hjust = 0, size = 8,
      margin = margin(t = -20)
    )
  ) +
  annotation_custom(
    grob = ggplotGrob(title), 
    ymin = -5,
    ymax = -2,
    xmin = -92,
    xmax = -85
  ) +
  labs(
    caption = glue::glue(
      "#30DayMapChallenge | Day 20: Heatmap | Created by @loreabad6\n",
      "Joshelyn Intriago, & David Morocho. (2021). Ecuador seismic dataset 2013-2021 (0.1) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.4641566\n",
      "Richard Styron. (2019). GEMScienceTools/gem-global-active-faults: First release of 2019 (2019.0). Zenodo. https://doi.org/10.5281/zenodo.3376300"
    )
  ) 

ggsave(filename = "maps/day27.png",
       height = 14.2, width = 35, units = "cm")
