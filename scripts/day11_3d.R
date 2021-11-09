library(raster)
library(rayshader)
library(stars)

dem_1 = read_stars("data/dem/DEM_30_0000768000_0009792000.asc")
dem_2 = read_stars("data/dem/DEM_30_0000768000_0009816000.asc")
dem = st_mosaic(dem_1, dem_2)
extent = extent(768860, 791260,-193000,-172000)
altar = dem |> st_set_crs(24877) |> as("Raster") |>
  crop(extent) |> raster_to_matrix()
altar |> 
  sphere_shade(texture = "imhof1") |>
  plot_3d(altar, zscale = 20)
