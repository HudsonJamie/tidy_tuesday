# aniamal_rescues.R
# Jamie Hudson
# Created: 02 Jul 2021
# Edited: 02 Jul 2021
# Data: Data is Plural and Georgios Karamanis


# load libraries ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(sf)
library(rgdal)
library(osmdata)
library(showtext)
library(ggtext)
library(magick)

showtext_auto()
font_add_google("Lato", "lato")

tuesdata <- tidytuesdayR::tt_load(2021, week = 27)

animal_rescues <- tuesdata$animal_rescues

# Much of the script is adapted from:
# https://taraskaduk.com/posts/2021-01-18-print-street-maps/#ref-burkhart
# https://ggplot2tutor.com/tutorials/streetmaps
# https://joshuamccrain.com/tutorials/maps/streets_tutorial.html

# long and lat are characters, and "NULL" needs to be converted to an NA and removed
# Also seems as is northing and easting has fewer NULL data, so convert these to lat long

wgs84 = "+init=epsg:4326"
bng = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 
+ellps=airy +datum=OSGB36 +units=m +no_defs'

ConvertCoordinates <- function(easting,northing) {
  out = cbind(easting,northing)
  mask = !is.na(easting)
  sp <-  sp::spTransform(sp::SpatialPoints(list(easting[mask],northing[mask]),proj4string=sp::CRS(bng)),sp::CRS(wgs84))
  out[mask,]=sp@coords
  out <- as.data.frame(out)
  out %>% 
    rename(lat = northing, long = easting)
}

animal_rescues_1 <- animal_rescues %>% 
  mutate(easting = na_if(easting_m, "NULL"),
         easting = as.numeric(easting),
         northing = na_if(northing_m, "NULL"),
         northing = as.numeric(northing),) %>% 
  mutate(easting = case_when(is.na(easting) ~ easting_rounded,
                             TRUE ~ easting),
         northing = case_when(is.na(northing) ~ northing_rounded,
                              TRUE ~ northing))

animal_rescues_2 <- animal_rescues_1 %>% 
  add_column(ConvertCoordinates(animal_rescues_1$easting, animal_rescues_1$northing))

#Extract osm data

x <- c(-0.3, -0.1)
y <- c(51.35, 51.55)

custom_wandsworth <- rbind(x,y) 
colnames(custom_wandsworth) <- c("min", "max")

streets <- custom_wandsworth %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary",
                            "trunk")) %>%
  osmdata_sf()

medium_streets <- custom_wandsworth %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_streets <- custom_wandsworth %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

railway <- custom_wandsworth %>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

nat_water <- custom_wandsworth %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf() %>% 
  unname_osmdata_sf()

river <- custom_wandsworth %>%
  opq() %>%
  add_osm_feature(key = "waterway", value = c("river", "riverbank")) %>%
  osmdata_sf() %>% 
  unname_osmdata_sf()

water <- c(nat_water, river) %>% 
  .$osm_multipolygons %>% 
  select(osm_id, name) %>% 
  mutate(area = st_area(.)) %>% 
  # this filter gets rid of tiny isolated lakes etc
  filter(area >= quantile(area, probs = 0.3))

# Circle map

crs2 <- 6384 #For some reason st_buffer won't work using the crs of London (4326) - maybe related to this? (https://gis.stackexchange.com/questions/303126/how-to-buffer-a-pair-of-longitude-and-latitude-coordinates-in-r)

center <- c(long = -0.17,
            lat = 51.465)

dist <-  3000
circle <- tibble(lat = center["lat"], long = center["long"]) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs2) %>% 
  st_buffer(dist = dist) %>% 
  st_transform(crs = 4326)

# Crop data

streets_cropped <- streets$osm_lines %>% st_intersection(circle)
medium_cropped <- medium_streets$osm_lines %>% st_intersection(circle)
small_cropped <- small_streets$osm_lines %>% st_intersection(circle)
rail_cropped <- railway$osm_lines %>% st_intersection(circle)
water_cropped <- water %>% st_intersection(circle)
data_cropped <- st_as_sf(animal_rescues_2, coords = c("long", "lat"), 
                         crs = 4326) %>% st_intersection(circle)

## Fox

data_cropped %>% 
  filter(animal_group_parent == "Fox") %>% 
  ggplot() +
  geom_sf(data = water_cropped,
          inherit.aes = FALSE,
          fill = "steelblue",
          lwd = 0,
          alpha = .3) +
  geom_sf(data = streets_cropped,
          inherit.aes = FALSE,
          color = "grey20",
          size = .2,
          alpha = .8) +
  geom_sf(data = medium_cropped,
          inherit.aes = FALSE,
          color = "grey50",
          size = .2,
          alpha = .8) +
  geom_sf(data = rail_cropped,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          alpha = .4) +
  geom_sf(data = small_cropped,
          inherit.aes = FALSE,
          color = "grey60",
          size = .2,
          alpha = .8) +
  geom_sf(aes(geometry = geometry),
          colour = "#802F1B",
          size = 3) +
  theme_void() +
  labs(title = "BATTERSEA",
       subtitle = "51.465ºN | 0.170ºW  \n <span style = 'color:#802F1B; font-size:13pt'><b>Fox rescues</b></span>",
       caption = "@jamiebio | source: Data is Plural and Georgios Karamanis") +
  theme(plot.title = element_text(size = 38, family = "lato", face="bold", hjust= 0.5),
        plot.subtitle = element_markdown(family = "lato", size = 15, hjust= 0.5,
                                         margin = margin(0,0,-15,0),
                                         lineheight = 1.1),
        plot.caption = element_text(family = "lato", hjust = 0.9))

ggsave(paste0("fox_rescues_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 7,
       height = 8)

## Dog

data_cropped %>% 
  filter(animal_group_parent == "Dog") %>% 
  ggplot() +
  geom_sf(data = water_cropped,
          inherit.aes = FALSE,
          fill = "steelblue",
          lwd = 0,
          alpha = .3) +
  geom_sf(data = streets_cropped,
          inherit.aes = FALSE,
          color = "grey20",
          size = .2,
          alpha = .8) +
  geom_sf(data = medium_cropped,
          inherit.aes = FALSE,
          color = "grey50",
          size = .2,
          alpha = .8) +
  geom_sf(data = rail_cropped,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          alpha = .4) +
  geom_sf(data = small_cropped,
          inherit.aes = FALSE,
          color = "grey60",
          size = .2,
          alpha = .8) +
  geom_sf(aes(geometry = geometry),
          colour = "#BC916F",
          size = 3) +
  theme_void() +
  labs(title = "BATTERSEA",
       subtitle = "51.465ºN | 0.170ºW  \n <span style = 'color:#BC916F; font-size:13pt'><b>Dog rescues</b></span>",
       caption = "@jamiebio | source: Data is Plural and Georgios Karamanis") +
  theme(plot.title = element_text(size = 38, family = "lato", face="bold", hjust= 0.5),
        plot.subtitle = element_markdown(family = "lato", size = 15, hjust= 0.5,
                                         margin = margin(0,0,-15,0),
                                         lineheight = 1.1),
        plot.caption = element_text(family = "lato", hjust = 0.9))

ggsave(paste0("dog_rescues_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 7,
       height = 8)

## Cat

data_cropped %>% 
  filter(animal_group_parent == "Cat") %>% 
  ggplot() +
  geom_sf(data = water_cropped,
          inherit.aes = FALSE,
          fill = "steelblue",
          lwd = 0,
          alpha = .3) +
  geom_sf(data = streets_cropped,
          inherit.aes = FALSE,
          color = "grey20",
          size = .2,
          alpha = .8) +
  geom_sf(data = medium_cropped,
          inherit.aes = FALSE,
          color = "grey50",
          size = .2,
          alpha = .8) +
  geom_sf(data = rail_cropped,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          alpha = .4) +
  geom_sf(data = small_cropped,
          inherit.aes = FALSE,
          color = "grey60",
          size = .2,
          alpha = .8) +
  geom_sf(aes(geometry = geometry),
          colour = "#E6BD5D",
          size = 3) +
  theme_void() +
  labs(title = "BATTERSEA",
       subtitle = "51.465ºN | 0.170ºW  \n <span style = 'color:#E6BD5D; font-size:13pt'><b>Cat rescues</b></span>",
       caption = "@jamiebio | source: Data is Plural and Georgios Karamanis") +
  theme(plot.title = element_text(size = 38, family = "lato", face="bold", hjust= 0.5),
        plot.subtitle = element_markdown(family = "lato", size = 15, hjust= 0.5,
                                         margin = margin(0,0,-15,0),
                                         lineheight = 1.1),
        plot.caption = element_text(family = "lato", hjust = 0.9))

ggsave(paste0("cat_rescues_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 7,
       height = 8)


## Bird

data_cropped %>% 
  filter(animal_group_parent == "Bird") %>% 
  ggplot() +
  geom_sf(data = water_cropped,
          inherit.aes = FALSE,
          fill = "steelblue",
          lwd = 0,
          alpha = .3) +
  geom_sf(data = streets_cropped,
          inherit.aes = FALSE,
          color = "grey20",
          size = .2,
          alpha = .8) +
  geom_sf(data = medium_cropped,
          inherit.aes = FALSE,
          color = "grey50",
          size = .2,
          alpha = .8) +
  geom_sf(data = rail_cropped,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          alpha = .4) +
  geom_sf(data = small_cropped,
          inherit.aes = FALSE,
          color = "grey60",
          size = .2,
          alpha = .8) +
  geom_sf(aes(geometry = geometry),
          colour = "#A4A73B",
          size = 3) +
  theme_void() +
  labs(title = "BATTERSEA",
       subtitle = "51.465ºN | 0.170ºW  \n <span style = 'color:#A4A73B; font-size:13pt'><b>Bird rescues</b></span>",
       caption = "@jamiebio | source: Data is Plural and Georgios Karamanis") +
  theme(plot.title = element_text(size = 38, family = "lato", face="bold", hjust= 0.5),
        plot.subtitle = element_markdown(family = "lato", size = 15, hjust= 0.5,
                                         margin = margin(0,0,-15,0),
                                         lineheight = 1.1),
        plot.caption = element_text(family = "lato", hjust = 0.9))

ggsave(paste0("bird_rescues_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 7,
       height = 8)

# Create into gif (from http://www.nagraj.net/notes/gifs-in-r/)

imgs <- list.files(pattern = "*.png", full.names = TRUE)

img_joined <- lapply(imgs, image_read) %>% 
  image_join()

img_animated <- image_animate(img_joined, fps = 0.5)

image_write(image = img_animated,
            path = "animal_rescues.gif",
            width = 7,
            height = 8)
