library(overpass)
library(osmdata)
library(dplyr)
library(forcats)
library(ggplot2)
library(readr)
library(sf)
library(glue)
extrafont::loadfonts(device = "win")

# With overpass package
## Define a bounding box
bbox = "-2.9121,-79.0239,-2.8859,-78.9803"
 
## Create query to obtain all contributors in bbox
osmcsv = glue('[out:csv(::user)];
(node({bbox}););
out meta;')

## Run query, read in results, get top 5 contributors
opq = overpass_query(osmcsv)
opq_l = read_delim(opq, delim = "\t")
top3_cont = opq_l %>% 
  count(`@user`) %>% 
  top_n(3, n) %>%
  arrange(desc(n))
top3 = top3_cont %>% pull(`@user`)

user01 = glue('[out:csv(::id)];
(node(user:{top5[1]})({bbox}););
out body;')

## Create queries to extract osm id features they contributed
create_query = function(userid) {
  glue('[out:csv(::id)];
(node(user:{userid})({bbox}););
out body;')
}
queries = lapply(top3, create_query)

## Run the query, don't run on a loop, it will exhaust the API!
opq_01 = overpass_query(queries[[1]])
opq_02 = overpass_query(queries[[2]])
opq_03 = overpass_query(queries[[3]])

## Read in data, adding corresponding userid
opq_df1 = read_delim(opq_01, delim = "\t")
opq_df2 = read_delim(opq_02, delim = "\t")
opq_df3 = read_delim(opq_03, delim = "\t")

## Get node ids with osmdata package
dat01 = opq_osm_id (type = "node", id = opq_df1$`@id`) %>%
  opq_string () %>%
  osmdata_sf ()
dat02 = opq_osm_id (type = "node", id = opq_df2$`@id`) %>%
  opq_string () %>%
  osmdata_sf ()
dat03 = opq_osm_id (type = "node", id = opq_df3$`@id`) %>%
  opq_string () %>%
  osmdata_sf ()

## Merge all points into one df with userid identifier
dat = bind_rows(
  mutate(dat01$osm_points, userid = top3[1], rank = 1),
  mutate(dat02$osm_points, userid = top3[2], rank = 2),
  mutate(dat03$osm_points, userid = top3[3], rank = 3)
) 
# save(dat, file = "tmp/userids_nodes.Rdata")
# load(file = "tmp/userids_nodes.Rdata")

dat = dat %>% 
  mutate(userid = fct_reorder(as.factor(userid), rank))

## Get some bg data
bbox_osmdata = c(-79.0239,-2.9121,-78.9803,-2.8859)
hw = opq(bbox_osmdata) %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf() %>% 
  unique_osmdata()

roads = hw$osm_lines

## plot!
ggplot() +
  geom_sf(data = roads, col = "white", alpha = 0.6) +
  geom_sf(data = dat,
          size = 2, shape = 21, color = "white", stroke = 0.1,
          aes(fill = userid),
          alpha = 0.9) +
  scale_fill_manual(
    "", values = viridis::viridis(3),
    guide = guide_legend(override.aes = aes(size = 4))
  ) +
  coord_sf(
    xlim = c(bbox_osmdata[1]+0.0012, bbox_osmdata[3]-0.001),
    ylim = c(bbox_osmdata[2], bbox_osmdata[4])
  ) +
  labs(
    title = "OpenStreetMap contributors",
    subtitle = "Top 3 OSM contributors and their mapped nodes in Cuenca city center", 
    caption = "#30DayMapChallenge | Day 5: OpenStreetMap Data Challenge | Created by @loreabad6"
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#002233", color = NA),
    plot.background = element_rect(fill = "#002233", color = NA),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    text = element_text(
      color = "white", family = "Ubuntu Mono"
    )
  )
ggsave(filename = "maps/day05.png",
       height = 15, width = 20, units = "cm")

