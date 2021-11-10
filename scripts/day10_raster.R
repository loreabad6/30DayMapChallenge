## https://www.blopig.com/blog/2019/08/combining-inset-plots-with-facets-using-ggplot2/

library(geodata)
library(here)
library(tmap)
library(stars)
library(rnaturalearth)
library(tidyverse)
ec = ne_countries(scale = "large",
                  country = "Ecuador",
                  returnclass = "sf")

bbox_cont = ec %>% 
  st_cast("POLYGON") %>% 
  mutate(area = st_area(geometry)) %>% 
  top_n(1) %>% 
  st_buffer(100000) %>% 
  st_bbox()

bbox_gal = c(
  xmin = -92.5,
  xmax = -89.5,
  ymin = -1.3,
  ymax = 1.2
) %>% st_bbox()

tavg = worldclim_country(
  "Ecuador", "tavg",
  path = here("data/wordclim/")
)

tavg_stars = st_as_stars(tavg)

tavg_ec = tavg_stars[ec]

main = ggplot() +
  geom_stars(data = tavg_ec, downsample = c(10,10,0)) +
  scico::scale_fill_scico(
    "Average temperature (°C) \n 1970-2000",
    palette = "berlin", 
    n.breaks = 7,
    na.value = "transparent",
    guide = guide_colorbar(
      title.position = "top", title.hjust = 0.5, 
      barheight = grid::unit(3, 'mm'), 
      barwidth = grid::unit(100, 'mm'), 
      ticks = FALSE
    )
  ) +
  coord_equal(
    xlim = c(bbox_cont["xmin"], bbox_cont["xmax"]),
    ylim = c(bbox_cont["ymin"], bbox_cont["ymax"]),
  ) +
  facet_wrap(~band) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

## A function to plot the inset 
get_inset = function(stars, month){
  p = ggplot() +
    geom_stars(
      data = stars[,,,month],
      downsample = c(10,10,0),
      show.legend = FALSE
    ) +
    scico::scale_fill_scico(
      "Average temperature (°C) \n 1970-2000",
      palette = "berlin", 
      n.breaks = 7,
      na.value = "transparent"
    ) +
    coord_equal(
      xlim = c(bbox_gal["xmin"], bbox_gal["xmax"]),
      ylim = c(bbox_gal["ymin"], bbox_gal["ymax"]),
    ) +
    # facet_wrap(~band) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal", 
      plot.background = element_rect(color = 'grey90', fill = "transparent")
    )
  
  p
}
inset_plot = get_inset(tavg_ec) 

main +
  annotation_custom(
    grob = ggplotGrob(inset_plot), 
    ymin = bbox_cont['ymin'],
    ymax = -2.5,
    xmin = -77.5,
    xmax = bbox_cont['xmax']
  )

main_plot + insets