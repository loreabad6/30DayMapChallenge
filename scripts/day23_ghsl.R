library(stars)
library(osmdata)
library(tidyverse)
library(ggfx)
library(ggtext)
extrafont::loadfonts(device = "win")

builtup = read_stars('data/ghsl/GHS_BUILT_LDSMT_GLOBE_R2018A_3857_30_V2_0_7_11/GHS_BUILT_LDSMT_GLOBE_R2018A_3857_30_V2_0_7_11.tif')

guayaquil = getbb("Guayaquil, Ecuador", format_out = 'sf_polygon')
bbox = guayaquil %>% 
  mutate(area = st_area(geometry)) %>% 
  arrange(area) %>% 
  slice(3) %>% 
  st_buffer(15000) %>% 
  st_transform(st_crs(builtup)) %>% 
  st_bbox()

builtup_gye = builtup %>% 
  st_crop(bbox) %>% 
  mutate(across(everything(), as.factor))

builtup_pal = viridis::rocket(3, begin = 0.5, direction = -1)
ggplot() +
  with_inner_glow(
    geom_stars(data = builtup_gye, show.legend = FALSE),
    colour = "black", expand = 10, sigma = 2
  ) +
  scale_fill_manual(
    "",
    values = c(
      "grey30", "grey10",
      builtup_pal
    ),
    labels = c(
      "water", "no builtup",
      "built-up from 2000 to 2014 epochs",
      "built-up from 1990 to 2000 epochs",
      "built-up up to 1990 epoch"
    )
  ) +
  annotate(
    "RichText", x = -8895304, y = -220000,
    fill = NA, label.color = NA, color = "grey90",
    family = "Geograph", size = 8,
    label = glue::glue(
      "<span style='font-size:30pt'>Guayaquil built-up area</span><br>",
      "<span style='font-size:20pt; color:{builtup_pal[3]}'>up to 1990, </span>",
      "<span style='font-size:20pt; color:{builtup_pal[2]}'>from 1990 to 2000</span>\n<br>& ",
      "<span style='font-size:20pt; color:{builtup_pal[1]}'>from 2000 to 2014</span>"
    )
  ) +
  labs(
    caption = glue::glue(
      "#30DayMapChallenge | Day 23: GHSL data challenge | Created by @loreabad6\n\n",
      "Corbane, Christina; Politis, Panagiotis; Syrris, Vasileios; Pesaresi, Martino (2018): GHS built-up grid, derived from
Sentinel-1 (2016), R2018A. European Commission, Joint Research Centre (JRC) doi:10.2905/jrc-ghsl-10008"
    )
  ) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    plot.caption = element_text(
      size = 9, color = "white", family = "Geograph", hjust = 0.5,
      margin = margin(t = -15, b = 10)
    )
  )

ggsave(filename = "maps/day23.png",
       height = 25, width = 18, units = "cm")

knitr::plot_crop("maps/day23.png")