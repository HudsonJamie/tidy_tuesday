# afrilearndata.R
# Jamie Hudson
# Created: 09 November 2021
# Edited: 09 November 2021
# Data: afrilearndata & afrihealthsites

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(afrilearndata)
library(afrihealthsites)
library(osmdata)
library(sf)
library(ggfx)
library(showtext)
font_add_google("Old Standard TT", "oldtt")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# load dataset ------------------------------------------------------------

# load hex file from https://cartogrid.vercel.app/
sudan_hex <- readOGR(dsn = "./data/Sudan.geojson")
sudan_hex_sf <- as(sudan_hex, 'sf')

# wrangle data ------------------------------------------------------------

sudan_bb <- getbb("Sudan", featuretype = "country",
                  format_out = "sf_polygon")

sudan_hospital <- afrihealthsites("sudan", plot='sf', datasource = "who",
                             returnclass = "dataframe") %>% 
  janitor::clean_names() %>% 
  mutate(
    facility = case_when(
      str_detect(facility_type, "Hospital") ~ "hospital",
      TRUE ~ as.character(facility_type))) %>% 
  filter(facility == "hospital") %>% 
  drop_na(long)

sudan_hospital_sf <- st_as_sf(sudan_hospital,
                       coords = c("long", "lat"),
                       crs = projcrs)

sudan_hospital_join = sudan_hospital_sf %>% 
  st_union()

sf::sf_use_s2(FALSE)

distance <-  st_distance(sudan_hex_sf, sudan_test_join) %>% 
  units::set_units("km")

sudan_wf <- sudan_hex %>% 
  st_as_sf() %>% 
  mutate(
    dist = distance) %>% 
  drop_na()

# plot ------------------------------------------------------------

ggplot() +
  with_outer_glow(with_inner_glow(
    geom_sf(
      data = sudan_wf,
      aes(fill = as.numeric(dist)),
      color = "NA", alpha = 0.95
    ),
    colour = "white",
    sigma = 2
  ),
  colour = "black",
  sigma = 10,
  expand = 4) +
  coord_sf(xlim = c(st_bbox(sudan_bb)[1], st_bbox(sudan_bb)[3]),
           ylim = c(st_bbox(sudan_bb)[2], st_bbox(sudan_bb)[4])) +
  scale_fill_gradientn(colours = magma(10),
                       labels = c("0km", "100km", "200km", "300km", "400km", "500km"),
                       guide = guide_colourbar(
                         title.position = "top",
                         direction = "horizontal",
                         barwidth = 17,
                         barheight = 0.5,
                         frame.colour = "black"
                       )) +
  geom_point(data = sudan_hos, 
             aes(x = long, y = lat), 
             colour = "white", size = 0.3,
             alpha = 0.3) +
  labs(title = "Distance to Hospitals in Sudan",
       fill = "Distance to nearest Sudanese hospital (white dots)",
       caption = "@jamie_bio | source = {afrihealthsites} & {afrilearndata}") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#e9e6eb", colour = "#e9e6eb"),
        plot.background = element_rect(fill = "#e9e6eb", colour = "#e9e6eb"),
        plot.title = element_text(hjust = 0.5, family = "oldtt",
                                  colour = "black",
                                  size = 20,
                                  margin = margin(20, 0, -5, 0)),
        plot.caption = element_text(family = "oldtt",
                                    size = 4,
                                    colour = "black"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_text(family = "oldtt", hjust = 0.5,
                                    colour = "black", size = 8),
        legend.text = element_text(colour = "black",
                                   size = 5))

ggsave(paste0("sudan_hospitals_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 5,
       height = 5)
