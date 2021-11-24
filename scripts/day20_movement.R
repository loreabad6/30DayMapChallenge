#' Halpin, P.N., A.J. Read, E. Fujioka, B.D. Best, 
#' B. Donnelly, L.J. Hazen, C. Kot, K. Urian, 
#' E. LaBrecque, A. Dimatteo, J. Cleary, C. Good, 
#' L.B. Crowder, and K.D. Hyrenbach. 2009. OBIS-SEAMAP: 
#' The world data center for marine mammal, sea bird, 
#' and sea turtle distributions. Oceanography 22(2):104-115
#' 
#' GBIF.org (21 November 2021) GBIF Occurrence Download  https://doi.org/10.15468/dl.s5jzav
#' 
#' https://www.gbif.org/dataset/2144434e-9e31-4558-8616-a2a301d02dc8#description

library(tidyverse)
library(sf)
library(gganimate)
library(rnaturalearth)
library(ggimage)
extrafont::loadfonts(device = "win")

countries = ne_countries(scale = "large", returnclass = "sf")

bbox = countries %>% 
  filter(continent == "South America") %>% 
  st_bbox()

data = read_tsv("data/gbif/0061779-210914110416597.csv")
datasf = data %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

data_america = datasf %>% 
  filter(countryCode %in% c("EC", "PE", "AR", "CL")) %>% 
  mutate(month = factor(month, labels = month.name))

plot = ggplot() +
  geom_sf(data = countries, fill = "black", alpha = 0.5,
          color = NA, size = 0.2) +
  geom_sf(data = data_america, 
          shape = 21, col = "grey90", fill = "grey10", stroke = 1.5,
          alpha = 0.9, size = 2, show.legend = FALSE) +
  annotation_custom(
    grid::rasterGrob(png::readPNG("figs/humpback-whale.png")),
    xmin = -109.5, xmax = -80, ymin = -55.8, ymax = -40
  ) +
  annotate(
    "text",
    x = -90, y = -30, color = "white",
    family = "Pompiere ", size = 8,
    label = "Humpback Whale\nobservations in\nthe South Pacific Ocean"
  ) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])
  ) +
  labs(
    caption = glue::glue(
      "#30DayMapChallenge | Day 20: Movement | Created by @loreabad6\n",
      "Happywhale. 2021. Happywhale - Humpback Whale in South Pacific Ocean.\n",
      "Accessed via GBIF.org on 2021-11-20"
    )
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Pompiere ", colour = "white"),
    plot.caption = element_text(
      size = 10, hjust = 0.5, family = 'sans',
      margin = margin(t = 10, b = 10)
    ),
    plot.title = element_text(
      hjust = 0.9, size = 35,
      margin = margin(t = 10, b = -50)
    ),
    plot.background = element_rect(fill = "deepskyblue3", color = NA)
  ) +
  labs(title = "{closest_state}") +
  transition_states(month, transition_length = 2, state_length = 3) +
  ease_aes('cubic-in-out')

anim_save(filename = "maps/day20.gif", animation = plot,
          width = 20, height = 20.5, res = 300, units = "cm")
