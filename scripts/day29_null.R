library(httr)
library(ggplot2)
library(dplyr)
library(purrr)
library(jsonlite)
library(sf)
library(ggtext)
library(rnaturalearth)

url1 = "http://epsg.io/?q=ecuador&format=json&page=1"
url2 = "http://epsg.io/?q=ecuador&format=json&page=2"
url3 = "http://epsg.io/?q=ecuador&format=json&page=3"
res1 = GET(url1)
res2 = GET(url2)
res3 = GET(url3)
content1 = fromJSON(rawToChar(res1$content))
content2 = fromJSON(rawToChar(res2$content))
content3 = fromJSON(rawToChar(res3$content))
epsg_codes_ec = bind_rows(
  content1$results,
  content2$results,
  content3$results
) |> 
  mutate(
    datum = str_trim(str_extract(name, '[^/]+')),
    only_datum = case_when(name == datum ~ TRUE, TRUE ~ FALSE)
  )


get_null_loc = function(epsg, datum) {
  geom = st_point(c(0,0)) |> 
    st_sfc(crs = as.numeric(epsg))
    
  st_sf(geom, epsg = epsg, datum = datum) |> 
    st_transform(4326)
} 

datums_ec = epsg_codes_ec |> 
  filter(only_datum) |> 
  select(code, datum)
p = map2(epsg_codes_ec$code, epsg_codes_ec$datum, get_null_loc) |> 
  bind_rows()


countries = ne_countries(returnclass = "sf")
ggplot() +
  geom_sf(data = countries, fill = "black", color = NA) +
  geom_sf(
    data = p, color = "white", fill = "red",
    shape = 21, stroke = 1, size = 2.5
  ) +
  labs(
    caption = "#30DayMapChallenge | Day 29: NULL | Data: epsg.io | Created by @loreabad6",
  ) +
  annotate(
    "RichText", x = -130, y = -25,
    fill = NA, label.color = NA, color = "black",
    family = font, size = 5,
    label = glue::glue(
      "<span style='font-size:20pt; color:red'>**NULL archipelago**</span>",
      "<br>Coordinate 0,0<br>for Ecuador's projected CRSs<br>",
      "reprojected to WGS84 (EPSG:4326)"
    )
  ) +
  coord_sf(crs = 4326, expand = F) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = 'grey20'),
    plot.caption = element_text(
      color = "white", hjust = 0.5, size = 9, 
      margin = margin(t = -10, b = 10)
    )
  )

ggsave(filename = "maps/day29.png",
       height = 16, width = 30, units = "cm")
knitr::plot_crop("maps/day29.png")
