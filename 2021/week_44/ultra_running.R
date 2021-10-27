# ultra_running.R
# Jamie Hudson
# Created: Date
# Edited: Date
# Data: Data

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(ggmap)
library(XML)
library(gganimate)
library(showtext)
library(colorspace)
library(ggtext)
library(shadowtext)
font_add_google("Cabin")
font_add_google("Shrikhand")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# load dataset ------------------------------------------------------------

ultra_rankings <- readr::read_csv('https://t.co/JNJTpFTKqI?amp=1') %>% 
  clean_names()

race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

# read in GTX data for UTMB course 
# code from Sascha Wolfer https://www.r-bloggers.com/2014/09/stay-on-track-plotting-gps-tracks-with-r/
# data from https://www.plotaroute.com/search?keyword=utmb

options(digits=10)
# Parse the GPX file
pfile <- htmlTreeParse(file = "data/UTMB.gpx", error = function(...) {
}, useInternalNodes = T)

elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
lats <- as.numeric(coords["lat",])
lons <- as.numeric(coords["lon",])

# wrangle data ------------------------------------------------------------

race_df <- full_join(ultra_rankings, race)

utmb_21 <- race_df %>% 
  filter(race_year_id == 72496)

geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)

lat <- c(min(geodf$lat) -0.06, max(geodf$lat) + 0.1)
lon <- c(min(geodf$lon) - 0.1, max(geodf$lon) + 0.1)

bbox <- make_bbox(lon,lat)

geodf <- geodf %>% 
  slice(which(row_number() %% 5 == 1))

utmb_times <- utmb_21 %>% 
  dplyr::select(time_in_seconds) %>% 
  mutate(pos = row_number()) %>% 
  pivot_wider(names_from = pos, values_from = time_in_seconds) %>% 
  slice(rep(1, each = nrow(geodf)))

# function to repeat times n times
rep.x <- function(x, na.rm=FALSE) (x / nrow(geodf) * row_number())

geodf_times <- cbind(geodf, utmb_times) %>% 
  dplyr::select(-c("time")) %>% 
  mutate_at(.vars = vars("1":"1526"), 
            rep.x) %>% 
  pivot_longer(-c(lon, lat, ele), names_to = "id", 
               values_to = "time") %>% 
  mutate(time = as.numeric(time))

# D'Haene and Dauwalter
two_runners <- geodf_times %>%
  filter(id %in% c(1,7))

# dataframe of route
route <- geodf %>% 
  dplyr::select(lon, lat, ele)

# download stamenmap for background
map_background <- get_stamenmap(bbox,
                    zoom = 12,
                    source="stamen",
                    maptype = "terrain-background",
                    color="bw")

# plot ------------------------------------------------------------

plot <- ggmap(map_background) +
  geom_path(data = route, mapping = aes(x = lon, y = lat, color = ele, group = 1),
            size = 4, lineend = "round") +
  scale_color_viridis_c(option = "magma") +
  geom_rect(xmin = 6.5, xmax = 7.3, ymin = 46.09, ymax = 46.2, 
            fill = "grey", alpha = 0.4) +
  geom_jitter(geodf_times, mapping = aes(x = lon, y = lat, group = id),
              colour = "white", fill = "white", pch = 25,
              size = 0.4, width = 0.002, height = 0.002) +
  geom_jitter(two_runners, mapping = aes(x = lon, y = lat, group = id),
              colour = "white", fill = "black",
              pch = rep(c(21, 22), 2858), size = 2.5, width = 0.002, height = 0.002) +
  geom_segment(aes(x = route$lon[1] - 0.01, 
                   y = route$lat[1] + 0.01,
                   xend = route$lon[1] + 0.01, 
                   yend = route$lat[1] - 0.01),
               colour = "yellow") +
  geom_shadowtext(label = "Start/End",
                  x = route$lon[1] + 0.014, y = route$lat[1] - 0.014, 
                  size = 3, hjust = 0, family = "Cabin", check_overlap = TRUE,
                  colour = "black", bg.colour = "white", bg.r = 0.2) +
  geom_shadowtext(label = "Elevation (m)",
                  x = 6.917, y = 45.623, 
                  size = 3, hjust = 0.5, family = "Cabin", check_overlap = TRUE,
                  colour = "black", bg.colour = "white", bg.r = 0.2) +
  geom_shadowtext(label = "1000",
                  x = 6.785, y = 45.635, 
                  size = 2.5, hjust = 0.5, family = "Cabin", check_overlap = TRUE,
                  colour = "black", bg.colour = "white", bg.r = 0.2) +
  geom_shadowtext(label = "2500",
                  x = 7.07, y = 45.635, 
                  size = 2.5, hjust = 0.5, family = "Cabin", check_overlap = TRUE,
                  colour = "black", bg.colour = "white", bg.r = 0.2) +
  guides(colour = guide_colorbar(title.position = 'bottom', title.hjust = 0.5,
                                 barwidth = unit(15, 'lines'), barheight = unit(0.8, 'lines'))) +
  annotate(geom = "text", label = "Ultra-Trail du Mont-Blanc 2021",
           x = 6.917, y = 46.162, size = 8, hjust = 0.5, family = "Shrikhand") +
  annotate(geom = "richtext", label = "A total of 1526 runners completed the 2021 edition of the UTMB® race in Chamonix, France. Starting at 17:00 the course  \nundulates over 170km, and was eventually won by Francois D'Haene in a time of 20 hours 45 minutes and 59 seconds.  \nCourtney Dauwalter was the first female runner to finish in 22 hours 30 minutes and 54 seconds.",
           x = 6.917, y = 46.125, size = 2.8, hjust = 0.5, family = "Cabin",
           fill = NA, label.color = NA) +
  annotate(geom = "richtext", label = "*The map below shows the average speed of each finisher. D'Haene is the black circle, and Dauwalter the black square*",
           x = 6.917, y = 46.1, size = 2.8, hjust = 0.5, family = "Cabin",
           fill = NA, label.color = NA) + 
  labs(colour = "Elevation (m)",
       caption = "@jamie_bio | source: International Trail Running Association (ITRA)") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(family = "Cabin", size = 8),
    legend.position=c(0.5, 0.025),
    legend.justification = "bottom",
    legend.direction = "horizontal",
    legend.text = element_blank(),
    legend.title = element_blank())

anim <- plot +
  transition_reveal(time)

animate(anim, nframes = 200, 
        height = 8, width = 6.5, units = "in", res = 150)

anim_save(paste0("ultra_running_", format(Sys.time(), "%d%m%Y"), ".gif"))

