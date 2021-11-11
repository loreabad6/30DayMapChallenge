## https://www.blopig.com/blog/2019/08/combining-inset-plots-with-facets-using-ggplot2/

library(geodata)
library(here)
library(tmap)
library(stars)
library(rnaturalearth)
library(tidyverse)
extrafont::loadfonts(device = "win")

ec = ne_countries(scale = "medium",
                  country = "Ecuador",
                  returnclass = "sf")

bbox_cont = ec %>% 
  st_cast("POLYGON") %>% 
  mutate(area = st_area(geometry)) %>% 
  top_n(1) %>% 
  st_buffer(100000) %>% 
  st_bbox()

bbox_gal = c(
  xmin = -91.7,
  xmax = -89.2,
  ymin = -1.4,
  ymax = 0.3
) %>% st_bbox()

tavg = worldclim_country(
  "Ecuador", "tavg",
  path = here("data/wordclim/")
)

tavg_stars = st_as_stars(tavg)

tavg_ec = tavg_stars[ec] 

tavg_ec_named = tavg_ec %>% 
  st_set_dimensions(which = 'band',
                    values = c("JAN", "FEB", "MAR", "APR", "MAY",
                                 "JUN", "JUL", "AUG", "SEP", "OCT",
                                 "NOV", "DEC"))
main = ggplot() +
  geom_stars(data = tavg_ec_named, downsample = c(0,0,0)) +
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
  facet_wrap(
    ~band, strip.position = "bottom"
  ) +
  labs(
    caption = "#30DayMapChallenge | Day 10: Raster | Data: WorldClim | Created by @loreabad6"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = element_text(size = 13, family = "Roboto", color = "#501802"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.caption = element_text(
      hjust = 0.5, size = 9
    )
  )

## A function to plot the inset 
get_inset = function(stars, month){
  p = ggplot() +
    geom_stars(
      data = stars[,,,month],
      downsample = c(0,0,0),
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


## This function allows us to specify which facet to annotate
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

insets = 1:12 %>% 
  purrr::map( ~annotation_custom2(
    grob = ggplotGrob(get_inset(tavg_ec_named, .)),
    data = data.frame(month = .),
    ymin = bbox_cont['ymin'],
    ymax = -2.5,
    xmin = -77.5,
    xmax = bbox_cont['xmax']
  )
)

final = main + insets

ggsave(final, filename = "maps/day10.png",
       height = 22, width = 25, units = "cm")

knitr::plot_crop("maps/day10.png")