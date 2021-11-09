library(sf)
library(here)
library(tidyverse)
library(ggfx)
extrafont::loadfonts(device = "win")

## https://www.amazoniasocioambiental.org/es/mapas/#descargas
tind = read_sf(here("data/raisg/Tis_2020/Tis_TerritoriosIndigenas.shp"))
tindec = tind %>% 
  filter(pais == "Ecuador", categoria == "Nacionalidad")

ggplot(tindec) +
  geom_sf(aes(fill = etnias), color = NA) +
  scale_fill_brewer(palette = "Dark2") +
  theme_void()
