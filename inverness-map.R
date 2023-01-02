library(sf)
library(osmdata)
library(raster)
library(tidyverse)
library(lwgeom)
library(ggdark)


place <- "Inverness Scotland"

highway_sizes <- tibble::tribble(
  ~highway, ~highway_group, ~size,
  "motorway",        "large",   0.5,
  "motorway_link",        "large",   0.3,
  "primary",        "large",   0.5,
  "primary_link",        "large",   0.3,
  "secondary",       "medium",   0.3,
  "secondary_link",       "medium",   0.3,
  "tertiary",       "medium",   0.3,
  "tertiary_link",       "medium",   0.3,
  "residential",        "small",   0.2,
  "living_street",        "small",   0.2,
  "unclassified",        "small",   0.2,
  "service",        "small",   0.2,
  "footway",        "small",   0.2
)


streets_osm <- opq(place) %>%
  add_osm_feature(key = "highway",
                  value = highway_sizes$highway) %>%
  osmdata_sf()

streets <- streets_osm$osm_lines %>%
  select(osm_id, name, name.en, highway, maxspeed, oneway, surface) %>%
  mutate(length = as.numeric(st_length(.))) %>%
  left_join(highway_sizes, by = "highway") %>%
  filter(highway_group != "small" | length >= quantile(length, probs = 0.25))

railways_osm <- opq(place) %>%
  add_osm_feature(key = "railway", value ="rail") %>%
  osmdata_sf()

railways <- railways_osm$osm_lines %>%
  dplyr::select()


water_osm <- opq(place) %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf() %>%
  unname_osmdata_sf()

river_osm <- opq(place) %>%
  add_osm_feature(key = "waterway", value = c("river")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()

water <- c(water_osm, river_osm) %>%
  .$osm_multipolygons %>%
  select(osm_id, name) %>%
  mutate(area = st_area(.))# %>%
  # this filter gets rid of tiny isolated lakes et cetera
  #filter(area >= quantile(area, probs = 0.75))

all_boundaries <- opq(place) %>%
  add_osm_feature(key = "boundary",
                  value = c("administrative")) %>%
  osmdata_sf() %>%
  #This next step is optional, depending on what the call returns
  unname_osmdata_sf() %>%
  .$osm_multipolygons

#ggplot(data = all_boundaries) +
  #geom_sf()


boundary <- all_boundaries %>%
  #filter(osm_id =='1020106') %>% # just plot everything
  select(.)

streets_cropped <- streets %>% st_intersection(boundary)
water_cropped <- water %>% st_intersection(boundary)
railways_cropped <- railways %>% st_intersection(boundary)


blankbg <- theme(axis.line = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                legend.position = "none",
                #plot.background=element_blank(),
                panel.grid.minor = element_blank(),
                #panel.background=element_blank(),
                panel.grid.major = element_blank(),
                plot.margin = unit(c(t = 1,r = 2,b = 1,l = 2), "cm"),
                plot.caption = element_text(color = "grey20", size = 42,
                                            hjust = .5, face = "plain",
                                            family = "Trebuchet MS"),
                panel.border = element_blank()
)



p <- ggplot() +

  geom_sf(data = water %>%
            filter(name  != "Loch Ness"), # add back in if you don't want the loch
          fill = "steelblue",
          # size = .8,
          lwd = 0,
          alpha = .3) +
  geom_sf(data = railways,
          color = "grey30",
          size = .2,
          linetype ="dotdash",
          alpha = .5) +
  geom_sf(data = streets %>%
            filter(highway_group == "small"),
          size = .1,
          color = "grey40") +
  geom_sf(data = streets %>%
            filter(highway_group == "medium"),
          size = .3,
          color = "grey35") +
  geom_sf(data = streets %>%
            filter(highway_group == "large"),
          size = .5,
          color = "darkslategray4") +
   labs(caption = 'Inverness') +
  dark_theme_minimal() +
  blankbg +
  coord_sf(
           expand = FALSE)


ggsave("inverness.png", plot = p, width = 297, height = 420, units = "mm", dpi = "retina")
ggsave("map.svg", plot = p)
