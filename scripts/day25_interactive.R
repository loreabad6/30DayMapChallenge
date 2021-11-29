library(leaflet)
library(leaftime)
library(leafem)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(sf)
library(stringi)
library(htmlwidgets)
library(htmltools)

Sys.setlocale("LC_TIME", "Spanish")

# Get data from INEC
feminicides = read_xlsx(
  "data/inec/Justicia_Crimen/092021_Tabulados Seguridad.xlsx",
  sheet = 3, skip = 4
) |> 
  rename(date = ...1)

# Wrangle
femdf = feminicides |> 
  select(-NACIONAL) |> 
  mutate(start = as.Date(date), end = as.Date('2021-10-01')) |> 
  pivot_longer(
    -c(date, start, end),
    names_to = "province", 
    values_to = "deaths"
  ) |> 
  mutate(province = stri_trans_general(str = province, 
                                       id = "Latin-ASCII")) |> 
  mutate(deaths = replace_na(deaths, 0)) |> 
  mutate(year = year(date), month = month(date)) |> 
  group_by(province) |> 
  mutate(deathscum = cumsum(deaths)) |> 
  ungroup()

# Get provinces
prov = read_sf("data/humdata/ecu_adm_inec_20190724_shp/ecu_admbnda_adm1_inec_20190724.shp")

# Generate a XY random location within each provicne to represent a single death
femsf = prov |> 
  mutate(province = stri_trans_general(str = ADM1_ES, 
                                       id = "Latin-ASCII")) |> 
  select(province) |> 
  left_join(femdf, by = "province") |> 
  filter(province != "Zona No Delimitada") |> 
  rowwise() |> 
  mutate(samples = st_combine(st_sample(geometry, deaths))) |> 
  ungroup() |> 
  st_set_geometry("samples") |>
  select(-geometry) |>
  filter(!st_is_empty(samples)) |> 
  st_cast('POINT') |> 
  mutate(
    x = st_coordinates(samples)[,1],
    y = st_coordinates(samples)[,2]
  ) |>
  mutate(start = as.character(start), end = as.character(end))

# Convert to geojson for timeline, include label text with html
femjson = femsf |> select(-samples) |> 
  group_by(province) |> 
  mutate(deathprov = sum(deaths)) |> 
  ungroup() |> 
  mutate(label = glue::glue(
    "<span style='font-size:12pt'>",
    "Soy una de las ", 
    "<span style='font-size:20pt; color:purple' font-face:bold>",
    "{deathscum} ",
    "</span>",
    "víctimas<br>de feminicidio registradas <br>", 
    "desde agosto, 2014 hasta<br>",
    "{format(date, '%B, %Y')} en ", 
    "<span style='font-size:13pt; color:purple' font-face:bold>",
    "{province}.<br>",
    "</span>",
    "Hasta septiembre 2021 somos ",
    "<span style='font-size:20pt; color:purple' font-face:bold>",
    "{deathprov}.",
    "</span>",
    "</span>"
  )) |> 
  geojsonio::geojson_json(lat = "x",lon = "y")

# Map text box
tag.map.title = tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    color: #FFB000;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(250,250,250,0);
    font-weight: bold;
    font-size: 28px;
  }
"))

title = tags$div(
  tag.map.title, HTML("Feminicidios en Ecuador")
)  

tag.map.cred = tags$style(HTML("
  .leaflet-control.map-cred { 
    transform: translate(-105%,-100%);
    position: fixed !important;
    left: 50%;
    color: rgba(250,250,250,1);
    text-align: left;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(250,250,250,0);
    font-size: 12px;
  }
"))

cred = tags$div(
  tag.map.cred, HTML("#30DayMapChallenge | Day 25: Interactive | Data: Fiscalia General del Estado via INEC | Created by @loreabad6")
)  

img = "https://raw.githubusercontent.com/loreabad6/30DayMapChallenge/main/figs/end-violence-against-women_transparent.png"

# Map 
m = leaflet(femsf) |>  
  addProviderTiles(
    providers$Stamen.Toner, 
    options = providerTileOptions(opacity = 0.65)
  ) |> 
  setView(lat = -1, lng = -83, zoom = 6) |> 
  addControl(title, position = "topleft", className="map-title") |> 
  addControl(cred, position = "bottomleft", className="map-cred") |> 
  addTimeline(
    data = femjson,
    width = "45%",
    sliderOpts = sliderOptions(
      steps = 86,
      position = "bottomleft",
      formatOutput = htmlwidgets::JS(
             "function(date) {return new Date(date).getFullYear()}")
    ),
    timelineOpts = timelineOptions(
      pointToLayer = htmlwidgets::JS(
        "
function(data, latlng) {
  return L.circleMarker(latlng, {
    radius: 5,
    stroke: false,
    fillColor: 'darkorange',
    fillOpacity: 0.9
  }).bindTooltip(data.properties.label)
}
"
      ),
    styleOptions = NULL
    )
  ) |> 
  addLogo(
    img, 
    width = 160,
    height = 160,
    url = "https://www.unwomen.org/en/what-we-do/ending-violence-against-women/take-action/16-days-of-activism")


saveWidget(m, file="web/day25.html", title = "Feminicidios en Ecuador")
