library(renv)
library(tidyverse)
pkgs = dependencies()
pkgs_sum = pkgs |> 
  count(Package) |> 
  mutate(type = case_when(
    str_starts(Package, "tidy") ~ "Wrangle",
    str_starts(Package, "forcats") ~ "Wrangle",
    str_starts(Package, "dplyr") ~ "Wrangle",
    str_starts(Package, "string") ~ "Wrangle",
    str_starts(Package, "purrr") ~ "Wrangle",
    str_starts(Package, "glue") ~ "Wrangle",
    str_starts(Package, "lubridate") ~ "Wrangle",
    str_starts(Package, "classInt") ~ "Wrangle",
    str_starts(Package, "units") ~ "Wrangle",
    str_starts(Package, "sf") ~ "Spatial",
    str_starts(Package, "stars") ~ "Spatial",
    str_starts(Package, "terra") ~ "Spatial",
    str_starts(Package, "raster") ~ "Spatial",
    str_starts(Package, "rgee") ~ "Read/Get",
    str_starts(Package, "read") ~ "Read/Get",
    str_starts(Package, "geodata") ~ "Read/Get",
    str_starts(Package, "osmdata") ~ "Read/Get",
    str_starts(Package, "osmextract") ~ "Read/Get",
    str_starts(Package, "overpass") ~ "Read/Get",
    str_starts(Package, "rnaturalearth") ~ "Read/Get",
    str_starts(Package, "geoj") ~ "Read/Get",
    str_starts(Package, "jsonl") ~ "Read/Get",
    str_starts(Package, "httr") ~ "Read/Get",
    str_starts(Package, "haven") ~ "Read/Get",
    str_starts(Package, "here") ~ "Read/Get",
    str_starts(Package, "gg") ~ "Plot",
    str_starts(Package, "grid") ~ "Plot",
    str_starts(Package, "roughsf") ~ "Plot",
    str_starts(Package, "rayshader") ~ "Plot",
    str_starts(Package, "tmap") ~ "Plot",
    str_starts(Package, "png") ~ "Plot",
    str_starts(Package, "knitr") ~ "Plot",
    str_starts(Package, "biscale") ~ "Plot",
    str_starts(Package, "extraf") ~ "Plot",
    str_starts(Package, "html") ~ "Interactive",
    str_starts(Package, "leaf") ~ "Interactive",
    str_starts(Package, "scico") ~ "Palettes",
    str_starts(Package, "viridis") ~ "Palettes",
    str_starts(Package, "rcarto") ~ "Palettes",
    str_starts(Package, "random") ~ "Palettes",
    str_starts(Package, "Map") ~ "Palettes"
  ))

ggplot(pkgs_sum) +
  geom_col(
    aes(n, fct_reorder(Package, n, min), fill = n),
    show.legend = FALSE
  ) +
  scale_fill_viridis_c() +
  labs(x = "Times used", y = "Package") +
  facet_wrap(~type, scales = "free_y") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA))

ggsave("figs/day30.png", width = 25, height = 15, units = "cm")
