library(rgee)
ee_Initialize(user = "loreabad6")
library(sf)
library(tmap)
extrafont::loadfonts(device = "win")

center = st_sfc(st_point(c(-80.0881, -2.4721)), crs = 4386)
circle = st_buffer(center, 5000) 
circle_bbox = circle %>% 
  st_bbox() 

## Get thumbnail from Google Earth Engine
circle_gee = ee$Geometry$Rectangle(
  coords = as.vector(circle_bbox),
  proj = "EPSG:4326",
  geodesic = FALSE
)

getQABits = function(image, qa) {
  # Convert decimal (character) to decimal (little endian)
  qa = sum(2^(which(rev(unlist(strsplit(as.character(qa), "")) == 1))-1))
  # Return a single band image of the extracted QA bits, giving the qa value.
  image$bitwiseAnd(qa)$lt(1)
}

s2_clean = function(img) {
  
  # quality band
  ndvi_qa = img$select("QA60")
  
  # Select pixels to mask
  quality_mask = getQABits(ndvi_qa, "110000000000")
  
  # Mask pixels with value zero.
  img$updateMask(quality_mask)
}

bg = ee$ImageCollection("COPERNICUS/S2_SR")$
  filterDate(ee$Date('2020-01-01'), ee$Date('2021-11-01'))$
  filterBounds(circle_gee)$
  filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 20))$
  map(s2_clean)$
  select(c("B12", "B11", "B8", "B7", "B3", "B2"))$
  median()


bg_tc = ee_as_thumbnail(
  bg$select(c("B8", "B7", "B3")), 
  region = circle_gee, dimensions = 1024, 
  vizparams = list(min = 0, max = 2000, gamma = 1.55)
) %>% st_set_crs(4326)


tm_shape(bg_tc, raster.downsample = F) +
  tm_rgb(max.value = 1) +
  tm_credits(
    "#30DayMapChallenge | Day 8: Blue | Data: Sentinel-2 RGB:8∙7∙3 | Created by @loreabad6",
    col = "#FFFF6A", size = 0.5, align = "right",
    position = "right", fontfamily = "Segoe Print"
  ) +
  tm_layout(frame = F, asp = 0, outer.margins = 0, inner.margins = 0) 

tmap_save(filename = "maps/day08.png")

