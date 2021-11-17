library(raster)
library(rayshader)
library(stars)
library(tidyverse)
library(osmdata)

dem_1 = read_stars("data/dem/DEM_30_0000768000_0009792000.asc")
dem_2 = read_stars("data/dem/DEM_30_0000768000_0009816000.asc")
dem = st_mosaic(dem_1, dem_2)

extent = extent(772980, 789140,-192880,-175120)

bbox = extent |>
  st_bbox() |>
  st_as_sfc() |>
  st_set_crs(24817) |>
  st_transform(4326) |>
  st_bbox()

trails = opq(bbox) |> 
  add_osm_feature("highway", "path") |> 
  osmdata_sf() |>
  unique_osmdata()

water = opq(bbox) |> 
  add_osm_feature("natural", "water") |>
  osmdata_sf() |>
  unique_osmdata()

wateraltar = water$osm_polygons |> st_transform(24817) |>
  filter(name == "Laguna Amarilla")
trailsaltar = trails$osm_lines |> st_transform(24817) |>
  filter(name == "Camino al Altar")

altar = dem |> st_set_crs(24817) |> as("Raster") |>
  crop(extent) |> raster_to_matrix()

altar |> 
  sphere_shade(sunangle = 45, texture = "imhof4") |>
  plot_3d(altar, theta = 270, phi = 30, fov = 100,
          zscale = 20, zoom = 0.25, background = "aliceblue",
          windowsize = c(1000, 800))

render_polygons(wateraltar,
                extent = extent,
                bottom = 210, top = 212,
                clear_previous = TRUE,
                color="#5cb285",
                parallel=TRUE)

render_path(extent = extent,
            lat = trailsaltar, heightmap = altar,
            altitude = 3900, zscale = 20,
            color = "red", antialias = TRUE,
            clear_previous = TRUE)

render_depth(focus = 0.9, focallength = 500,
             filename = "maps/day11.png",
             title_text = "#30DayMapChallenge | Day 11: 3D | Data: SavGIS & © OpenStreetMap contributors | Created by @loreabad6",
             title_size = 18, title_font = "sans",
             title_position = "southeast")
