library(tidyverse)
library(readxl)
library(sf)
library(biscale)
library(grid)
library(rnaturalearth)
extrafont::loadfonts(device = "win")

## For bbox 
ec = ne_countries(scale = "small",
                  country = "Ecuador",
                  returnclass = "sf")

bbox_gal = c(
  xmin = -93.22,
  xmax = -87.638,
  ymin = -1.58,
  ymax = 1.77
) %>% st_bbox()

bbox_cont = ec %>% 
  st_cast("POLYGON") %>% 
  mutate(area = st_area(geometry)) %>% 
  top_n(1) %>% 
  st_buffer(100000) %>% 
  st_bbox()

popdens = read_xlsx(
  "data/inec/CPV2010/2_Densidad_Pobla_Nac_Prov_Cant_Parr.XLSX",
  sheet = 1, skip = 8, 
) |> 
  select(
    code = `Código`,
    pop = `Población`,
    popdens = `Densidad Poblacional`,
    area = `Superficie de la parroquia (km2)`
  ) 

pob = read_xls(
  "data/inec/CPV2010/32_NBI_POBLA_PROV_CANT_PARRO_AREA.xls",
  sheet = 1, skip = 12
)
pob_clean = pob |>
  drop_na(`Codigo de parroquia`) |>
  filter(Canton != "Total", `Codigo de parroquia` != "Total") |>
  select(canton = Canton, parroquia = `Codigo de parroquia`, poorperc = `...9`) |>
  distinct() |>
  mutate(
    parroquia = str_replace(str_to_lower(parroquia), " \\s*\\([^\\)]+\\)", ""),
    canton = str_to_lower(canton)
  )

par = read_sf("data/humdata/ecu_adm_inec_20190724_shp/ecu_admbnda_adm3_inec_20190724.shp")

datasf = par |> 
  select(parroquia = ADM3_ES, parcode = ADM3_PCODE, canton = ADM2_ES) |> 
  mutate(parcode = str_remove(parcode, "EC")) |> 
  mutate(
    parroquia = str_replace(str_to_lower(parroquia), " \\s*\\([^\\)]+\\)", ""),
    canton = str_to_lower(canton)
  ) |> 
  left_join(popdens, by = c("parcode" = "code")) |> 
  left_join(pob_clean) |> 
  drop_na(pop) |> 
  mutate(poorperc = as.numeric(poorperc)) |> 
  st_as_sf()

data = bi_class(
  datasf,
  x = "popdens",
  y = "poorperc",
  style = "quantile",
  dim = 3
)

main = ggplot() +
  geom_sf(
    data = data,
    aes(fill = bi_class), 
    color = "white", size = 0.01,
    show.legend = FALSE
  ) +
  bi_scale_fill(
    pal = "DkViolet",
    dim = 3,
    na.value = 'grey90') +
  bi_theme() +
  coord_sf(
    xlim = c(bbox_cont["xmin"], bbox_cont["xmax"]),
    ylim = c(bbox_cont["ymin"], bbox_cont["ymax"]),
  ) +
  labs(
    title = "Densidad poblacional y porcentaje de pobreza por parroquia",
    caption = "#30DayMapChallenge | Day 26: Choropleth | Data: INEC - Censo de Población y Vivienca 2010 | Created by @loreabad6"
  ) +
  theme(
    text = element_text(family = 'Leelawadee'),
    plot.title = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 8)
  )

inset = ggplot() +
  geom_sf(
    data = data,
    aes(fill = bi_class), 
    color = "white", size = 0.01,
    show.legend = FALSE
  ) +
  bi_scale_fill(
    pal = "DkViolet",
    dim = 3,
    na.value = 'grey90') +
  bi_theme() +
  coord_sf(
    xlim = c(bbox_gal["xmin"], bbox_gal["xmax"]),
    ylim = c(bbox_gal["ymin"], bbox_gal["ymax"]),
  ) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

map = main +
  annotation_custom(
    grob = ggplotGrob(inset), 
    ymin = bbox_cont['ymin'],
    ymax = -2,
    xmin = -78.5,
    xmax = bbox_cont['xmax']
  )

legend = bi_legend(
  pal = "DkViolet",
  dim = 3,
  xlab = "Higher Population Density",
  ylab = "Higher Poverty",
  size = 7
) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

# Open PNG device
png(filename = "maps/day26.png", width = 20, height = 14, units = "cm", res = 300)
pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 5)))
# Arrange the plots
print(
  map,
  vp = grid::viewport(
    layout.pos.row = 1:3,
    layout.pos.col = 2:5,
    width = unit(10, "cm"),
    height = unit(10, "cm"))
)
print(
  legend,
  vp = grid::viewport(
    layout.pos.row = 1,
    layout.pos.col = 1,
    width = unit(5, "cm"),
    height = unit(5, "cm"),
    angle = 315)
)
dev.off()
