library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(readxl)
library(sf)
library(biscale)
library(grid)
library(rnaturalearth)
library(fuzzyjoin)
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
  st_buffer(10000) %>% 
  st_bbox()

popdens = read_xlsx(
  "data/inec/CPV2010/2_Densidad_Pobla_Nac_Prov_Cant_Parr.XLSX",
  sheet = 1, skip = 8, 
) |> 
  select(
    code = `Código`,
    provincia = `Nombre de provincia`,
    canton = `Nombre de canton`,
    parroquia = `Nombre de parroquia`,
    pop = `Población`,
    popdens = `Densidad Poblacional`,
    area = `Superficie de la parroquia (km2)`
  ) |>
  mutate(
    parroquia = str_trim(
      str_replace(
        str_to_lower(parroquia),
        "\\(.*",
        ""
      )),
    canton = str_to_lower(canton),
    provincia = str_to_lower(provincia)
  ) |>
  drop_na()

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
    parroquia = str_trim(
      str_replace(
      str_to_lower(parroquia),
      "\\(.*",
      ""
    )),
    canton = str_to_lower(canton)
  ) |>
  mutate(
    parroquia = case_when(
      canton == "el piedrero" ~ "el piedrero",
      canton == "manga del cura" ~ "manga del cura",
      TRUE ~ parroquia
    )
  )

par = read_sf("data/humdata/ecu_adm_inec_20190724_shp/ecu_admbnda_adm3_inec_20190724.shp")

par_clean = par |> 
  select(parcode = ADM3_PCODE, parroquia = ADM3_ES, canton = ADM2_ES) |> 
  mutate(parcode = str_remove(parcode, "EC")) |> 
  mutate(
    parroquia = str_trim(
      str_replace(
        str_to_lower(parroquia),
        "\\(.*",
        ""
      )),
    canton = str_to_lower(canton)
  )

datasf =  par_clean |> 
  left_join(popdens, by = c("parcode" = "code")) |>
  mutate(
    parroquia = case_when(
      !is.na(parroquia.y) ~ parroquia.y,
      TRUE ~ parroquia.x
    ),
    canton = case_when(
      !is.na(canton.y) ~ canton.y,
      TRUE ~ canton.x
    )
  ) |>
  mutate(
    canton = case_when(
      canton == "santo domingo de los tsachilas" ~ "santo domingo",
      canton == "san jose de chimbo" ~ "chimbo",
      TRUE ~ canton
    )
  ) |>
  select(parcode, provincia,
         canton, parroquia,
         pop, popdens, area) |>
  stringdist_join(pob_clean, mode = "left",
                  by = c("canton", "parroquia"), max_dist = 2) |> 
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
    caption = "#30DayMapChallenge | Day 26: Choropleth | Data: INEC - Censo de Población y Vivienda 2010 | Created by @loreabad6"
  ) +
  theme(
    text = element_text(family = 'Leelawadee'),
    plot.title = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 0.5)
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
    ymax = -3,
    xmin = -78.5,
    xmax = bbox_cont['xmax']
  )

legend = bi_legend(
  pal = "DkViolet",
  dim = 3,
  xlab = "Mayor\ndensidad\npoblacional",
  ylab = "Mayor\nporcentaje\nprobreza",
  size = 7
) +
  theme(
    axis.title.y = element_text(margin = margin(r = -7)),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

# Open PNG device
png(filename = "maps/day26_revised.png",
    width = 20, height = 17, units = "cm", res = 300)
pushViewport(viewport(layout = grid.layout(nrow = 10, ncol = 14)))
# Arrange the plots
print(
  map,
  vp = grid::viewport(
    layout.pos.row = 1:10,
    layout.pos.col = 3:14,
    width = unit(14, "cm"),
    height = unit(14, "cm"))
)
print(
  legend,
  vp = grid::viewport(
    layout.pos.row = 1:2,
    layout.pos.col = 1:4,
    width = unit(7, "cm"),
    height = unit(7, "cm"),
    angle = 315)
)
dev.off()

