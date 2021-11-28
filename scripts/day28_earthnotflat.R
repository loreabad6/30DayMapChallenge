library(tidyverse)
library(sf)
library(rnaturalearth)

crs = "+proj=geos +h=15785831.0 +lon_0=-60 +sweep=y" #Geostationary satellite view

world = ne_coastline(scale = 10, returnclass = "sf") 
ec = ne_countries(scale = 10, country = "Ecuador", returnclass = "sf")

# Draw a sphere to plot countries on top
# It will represent the oceans
sphere = st_graticule(
  x = c(-130, -90, 10, 90), crs = 4326,
  lon = seq(10, -130, by = -0.1),
  ndiscr = 1000, margin = 10e-6
) |> 
  st_transform(crs = crs) |> 
  st_convex_hull()

equator = st_linestring(
  c(st_point(c(-130,0)),  st_point(c(10,0)))
) |> 
  st_sfc(crs = 4326)

g = ggplot() +
  geom_sf(
    data = filter(sphere, degree < 50, degree != -50),
    aes(fill = degree),
    color = NA,
    alpha = 0.8,
    show.legend = FALSE
  ) +
  scale_fill_gradient(
    low = "black", high = "gold"
  ) +
  geom_sf(data = world, color = "white", size = 0.2) +
  geom_sf(data = ec, fill = "white", color = NA) +
  geom_sf(data = equator, color = "red", size = 0.8) +
  coord_sf(crs = crs, clip = "off") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "transparent",
                                   color = "transparent"), 
    panel.background = element_rect(fill = "transparent",
                                    color = "transparent"),
    plot.margin = margin(rep(1, 4)),
    plot.caption = element_text(color = "white", hjust = 0.7, size = 20)
  ) +
  labs(
    caption = "#30DayMapChallenge | Day 28: Earth is not round\nData: Natural Earth | Created by @loreabad6"
  ) 

# Open PNG device
png(filename = "maps/day28.png", bg = "black", family = "sans",
    width = 20, height = 20, units = "cm", res = 200)

grid::grid.newpage()
print(
  g, 
  vp = grid::viewport(
    clip = "off",
    width = unit(1, "npc"),
    height = unit(1, "npc"),
    angle = 23.5)
)
dev.off()

