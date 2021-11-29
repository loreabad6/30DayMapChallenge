library(roughsf)
library(dplyr)
library(randomcoloR)
library(sf)
extrafont::loadfonts(device = "win")

# https://data.humdata.org/dataset/ecuador-admin-level-2-boundaries
cantones = read_sf(
  "data/humdata/ecu_adm_inec_20190724_shp/ecu_admbnda_adm2_inec_20190724.shp"
)

cantones_rough = cantones %>% 
  filter(ADM1_ES == "Guayas") %>% 
  arrange(Shape_Area) %>% 
  mutate(
    fill = randomColor(25),
    fillweight = 0.7,
    stroke = 0.5,
    hachureangle = runif(25, min = 0, max = 360)
  ) %>% 
  st_cast("POLYGON")

map = roughsf(
  cantones_rough, simplification = 2,
  title = "Guayas Province", 
  caption = "#30DayMapChallenge | Day 14: Map with a new tool | Data: INEC | Created by @loreabad6",
  title_font = "50px Dancing Script",
  caption_font = "25px Dancing Script",
  width = 1000, height = 1000
)

save_roughsf(map, file = "maps/day14.png")
