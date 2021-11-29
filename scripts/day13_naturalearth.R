library(haven)
library(here)
library(ggrepel)
library(rnaturalearth)
library(sf)
library(dplyr)
library(forcats)
library(ggplot2)
library(stringr)
library(tidygeocoder)
extrafont::loadfonts(device = "win")

path = here("data/cne/registro electoral a nivel parroquial.sav")
data = read_sav(path)

pop_abroad = data %>% 
  filter(PROVINCIA_CODIGO %in% c(26, 27, 28)) %>% 
  mutate(PARROQUIA_NOMBRE = as_factor(PARROQUIA_NOMBRE)) %>% 
  droplevels() %>% 
  mutate(city = str_remove(PARROQUIA_NOMBRE, "C. E. EN ")) %>% 
  group_by(city) %>%
  summarise(n_pop = sum(ELECTORES)) %>% 
  arrange(desc(n_pop)) %>% 
  ungroup()

pop_abroad_geo = pop_abroad  %>% 
  geocode(city, method = "osm", limit = 1)

pop_abroad_sf = pop_abroad_geo %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) 
crs = "+proj=natearth2 +datum=WGS84 +no_defs +over"
ec = ne_countries(scale = "large",
                  country = "Ecuador",
                  returnclass = "sf") %>% 
  st_transform(crs = crs)
countries = ne_countries(returnclass = 'sf') %>% 
  st_transform(crs = crs)

tz = ne_download(scale = 10, category = 'cultural',
                 destdir = here('data/naturalearth'),
                 type = 'time_zones', returnclass = 'sf')
tz_pop = tz %>% 
  select(objectid, time_zone, map_color8) %>% 
  mutate(rowid = row_number()) %>% 
  st_join(pop_abroad_sf) %>% 
  st_drop_geometry() %>% 
  group_by(rowid) %>% 
  summarise(n_pop_tz = sum(n_pop))

tzproj = tz %>% 
  mutate(rowid = row_number()) %>%
  left_join(tz_pop) %>% 
  st_transform(crs =crs) %>% 
  mutate(whatis8am = 13 + as.numeric(name)) %>% 
  mutate(whatis8am = case_when(
    whatis8am < 0 ~ whatis8am + 24, 
    TRUE ~ whatis8am)
  ) %>% 
  mutate(hour = case_when(whatis8am == 8.5 ~ "8h30", 
                          TRUE ~ paste0(whatis8am, "h00")))

pop_abroad_proj = pop_abroad_sf %>% 
  st_transform(crs = crs)

ggplot() +
  geom_sf(data = tzproj, aes(fill = map_color8),
          alpha = 0.6, color = NA, size = 0.2,
          show.legend = FALSE) +
  geom_sf(data = countries, fill = NA, 
          col = 'black', size = 0.2) +
  geom_sf(data = ec, col = NA, fill = 'white') +
  geom_sf(
    data = pop_abroad_proj, stroke = 1, alpha = 0.7,
    aes(size = n_pop), shape = 21, col = 'black', 
    fill = "white") +
  geom_label_repel(data = filter(tzproj, !is.na(n_pop_tz)),
            aes(geometry = geometry,
                stat(X), stat(Y),
                label = hour),
            family = "Futura LT", 
            size = 2.5, alpha = 0.85,
            label.r = unit(2, 'mm'),
            force_pull = 0, force = 80,
            min.segment.length = 0.01,
            stat = StatSfCoordinates,
            fun.geometry = sf::st_point_on_surface,
            color = "grey10") +
  scico::scale_fill_scico(
    direction = 1, end = 0.8,
    palette = 'batlow'
  ) +
  scale_size(
    "Ecuadorians",
    breaks = c(200, 1000, 20000, 40000, 60000)
  ) +
  labs(
    title = "Is it a good time to call?",
    subtitle = "If it is 8h00 in Ecuador, what time is it in the cities where Ecuadorian migrants live?",
    caption = "#30DayMapChallenge | Day 13: NaturalEarth Data Challenge | Data: NaturalEarth, CNE | Created by @loreabad6"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5, size = 14
    ),
    plot.subtitle = element_text(
      hjust = 0.5, size = 11
    ),
    plot.caption = element_text(
      hjust = 0.5, size = 8, family = "Futura LT"
    ),
    text = element_text(family = "Ink Free"),
    legend.position = "bottom",
    plot.background = element_rect(
      fill = "white", color = NA
    )
  )

ggsave(filename = "maps/day13.png",
       height = 15, width = 22, units = "cm")
 
