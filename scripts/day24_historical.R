# David Kidd, Kingston University London (2019)
# https://storymaps.arcgis.com/stories/79eeffa9f54d429687c17fa8267d3ba2

# Thinkquest historical boundaries 1815
#https://web.archive.org/web/20080328104539/http://library.thinkquest.org:80/C006628/download.html
library(tidyverse)
library(sf)
library(ggpomological)
library(ggimage)
library(MapColoring)
library(ggspatial)
extrafont::loadfonts(device = "win")

humboldt = read_sf("data/kingstonUniLondon/humboldt_route.geojson") |> 
  filter(Journey == "America") |> 
  mutate(
    angle = case_when(
      Date == "June-July 1799" ~ 0,
      Date == "Dec. 1800" ~ 340,
      Date == "Mar 1804" ~ 25,
      Date == "May 1804" ~ 50,
      Date == "July 1804" ~ 10,
      is.na(Date) ~ NA_real_
    ),
    hjust = case_when(
      Date == "June-July 1799" ~ 1.1,
      Date == "Dec. 1800" ~ -0.3,
      Date == "Mar 1804" ~ 0.5,
      Date == "May 1804" ~ 0.5,
      Date == "July 1804" ~0.5,
      is.na(Date) ~ NA_real_
    ),
    vjust = case_when(
      Date == "June-July 1799" ~ 3.5,
      Date == "Dec. 1800" ~ 0.5,
      Date == "Mar 1804" ~ 0.6,
      Date == "May 1804" ~ 0.5,
      Date == "July 1804" ~0.5,
      is.na(Date) ~ NA_real_
    )
  )

countries = read_sf("data/thinkquest/1815/cntry1815.shp") |> 
  st_set_crs(4326) |> 
  mutate(fillcol = as.factor(getColoring(as_Spatial(countries)))) |> 
  st_transform(st_crs(humboldt))

bbox = humboldt |> st_bbox()

g = ggplot() +
  geom_sf(data = countries,
          aes(fill = fillcol),
          col = NA, alpha = 0.6,
          show.legend = FALSE
        ) +
  geom_sf(
    data = humboldt,
    linetype = "longdash",
    color = "#2b323f",
    size = 0.5
  ) +
  geom_sf_text(
    data = humboldt,
    aes(label = Date, angle = angle, hjust = hjust, vjust = vjust),
    family = "Homemade Apple", 
    nudge_y = 250000
  ) +
  annotation_north_arrow(
    style = north_arrow_nautical(
      line_col = "#a89985",
      text_family = "Homemade Apple",
      text_col = "#a89985",
      fill = c("#a89985", "white"),
    )
  ) +
  annotate(
    "text",
    x = -1500000, y = -900000, color = "#6b452b",
    family = "Homemade Apple", size = 5,
    label = "Alexander von Humboldt\ntravels to the Americas\n1799-1804"
  ) +
  scale_fill_pomological() +
  labs(
    caption = glue::glue(
      "#30DayMapChallenge | Day 24: Historical | ",
      "Data: David Kidd, Kingston University London (2019), ThinkQuest | Created by @loreabad6",
    )
  ) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])
  ) +
  labs(x = NULL, y = NULL) +
  theme_pomological("Homemade Apple", 16) +
  theme(
    plot.caption = element_text(
      family = "mono", size = 9, hjust = 0.5,
      color = "#6b452b", face = "bold"
    )
  )

ggbackground(g, ggpomological:::pomological_images("background"))

ggsave(filename = "maps/day24.png",
       height = 20, width = 28, units = "cm")
