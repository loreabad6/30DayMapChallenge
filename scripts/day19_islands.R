library(rnaturalearth)
library(tidyverse)
library(sf)
library(ggfx)
library(haven)
library(gggibbous)
library(ggspatial)
library(ggtext)
extrafont::loadfonts(device = "win")

ec = ne_countries(scale = "large", country = "Ecuador",
                  returnclass = "sf")
bbox_gal = c(
  xmin = -93.22,
  xmax = -87.638,
  ymin = -1.58,
  ymax = 1.77
)  |>  st_bbox()

gal = ec |> 
  st_crop(bbox_gal)

center = st_sfc(st_point(c(-90.429, 0)), crs = 4326)
buffer = st_buffer(center, 300000)

# Entradas y Salidas internacionales
esi = read_dta(
  "data/inec/Datos_abiertos_ ESI_2020/esi_2020.dta"
)

gal_esi = esi |> 
  filter(pro_jefm == 20) 

gal_esi |> 
  count(tip_movi, jef_migr, ocu_migr) |> 
  arrange(desc(n)) 

gal_esi |> 
  count(ocu_migr) |> 
  arrange(desc(n))

coords = tribble(
  ~codePort, ~port, ~lat, ~long,
  7, "Puerto Maritimo Ayora", -0.7477778, -90.31278,
  8, "Puerto Maritimo Baquerizo Moreno", -0.89913,	-89.61211,
  28, "Puerto Maritimo Seymour", -0.43599, -90.28314,
  30, "Puerto Maritimo Isabela", -0.96340,	-90.96474,
  27, "Aeropuerto Seymour de Baltra", -0.44566, -90.2697
) 

gal_esi_sf = gal_esi |> 
  left_join(coords, by = c("jef_migr" = "codePort")) |> 
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
  filter(str_detect(port, "Puerto"))

gal_esi_sum = gal_esi_sf |>
  count(port, tip_movi) |>
  group_by(port) |>
  mutate(n_port = sum(n), frac = n/n_port) |> 
  mutate(
    x = st_coordinates(geometry)[,1],
    y = st_coordinates(geometry)[,2],
    tip_movi = as_factor(tip_movi),
    tip_movi_moon = as.logical(as.integer(tip_movi)-1),
    nudgey = case_when(
      port == "Puerto Maritimo Ayora" ~ -0.15,
      port == "Puerto Maritimo Baquerizo Moreno" ~ -0.15,
      port == "Puerto Maritimo Isabela" ~  -0.15,
      port == "Puerto Maritimo Seymour" ~ 0.15,
      TRUE ~ NA_real_
    ),
    label = glue::glue(
      "{str_replace_all(port, 'Puerto Maritimo', 'P.M.')}",
      "\n{n_port} registros"
    )
  ) 

font = "Dubai"
ggplot() +
  with_outer_glow(
    geom_sf(data = gal, fill = "lightgreen", color = NA),
    colour = "white", sigma = 5, expand = 10
  ) +
  geom_sf_text(
    data = gal_esi_sum,
    aes(label = label),
    family = 'Vivaldi',
    nudge_y = gal_esi_sum$nudgey,
    nudge_x = 0.11,
    check_overlap = TRUE,
    color = "black",
    size = 3.3
  ) +
  geom_moon(
    data = gal_esi_sum,
    aes(
      x = x, y = y,
      ratio = frac, size = n_port,
      fill = tip_movi, 
      right = tip_movi_moon
    ),
    color = NA, alpha = 0.9,
    key_glyph = draw_key_full_moon,
    show.legend = FALSE
  ) +
  annotation_scale(style = "ticks", text_family = font) +
  scale_size(range = c(5, 10), guide = NULL) +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  annotate(
    "RichText", x = -90.3, y = 1.3,
    fill = NA, label.color = NA, color = "black",
    family = font, size = 6,
    label = glue::glue(
      "<span style='font-size:20pt; color:#D95F02'>**Entradas**</span>",
      " & <span style='font-size:20pt; color:#1B9E77'>**Salidas**</span>",
      "<br>internacionales 2020<br>",
      "<span style='font-size:15pt'>en puertos marítimos",
      "<br>de las Islas Galápagos</span>"
    )
  ) +
  labs(
    caption = "#30DayMapChallenge | Day 19: Island(s)\nData INEC - ESI 2020 | Created by @loreabad6"
  ) +
  theme_void() +
  theme(
    text = element_text(family = font),
    plot.caption = element_text(
      hjust = 0.95, size = 9, color = "black",
      margin = margin(t = -10, b = 5)
    ),
    plot.background = element_rect(
      fill = "lightblue", color = "black", size = 3
      )
  )
ggsave(filename = "maps/day19.png",
       height = 17, width = 15, units = "cm")

knitr::plot_crop("maps/day19.png")
