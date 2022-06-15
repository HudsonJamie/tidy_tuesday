# us_droughts.R
# Jamie Hudson
# Created: 15 June 2022
# Edited: 16 June 2022
# Data: National Integrated Drought Information System

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(lubridate)
library(statebins)
library(showtext)
library(geojsonio)
library(gganimate)
library(rgeos)
library(broom)
library(usdata)

font_add_google("Merriweather")
showtext_opts(dpi = 320)
showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 24)

# wrangle data ------------------------------------------------------------

fips <- tuesdata$`drought-fips`

fips_df <- fips %>% 
  clean_names() %>% 
  mutate(date = ymd(date)) %>%
  mutate(month = format(date, "%b"),
         year = format(date, "%Y"),
         state = abbr2state(state)) %>%
  group_by(year, state) %>%
  summarise(mean_val = mean(dsci))

# read in hexgrid file
spdf <- geojson_read("data/us_states_hexgrid.geojson",  what = "sp")

spdf@data <-  spdf@data %>% 
  mutate(state = gsub(" \\(United States\\)", "", google_name),
         google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2)) 

spdf_fortified_plot <- spdf_fortified %>%
  left_join(., fips_df, by=c("id"="state")) 

# plot ------------------------------------------------------------

(fips_plot <- spdf_fortified_plot %>% 
  ggplot() +
  geom_polygon(aes(fill =  mean_val, x = long, y = lat, group = group)) +
  scale_fill_distiller(palette = "OrRd", direction = 1, limits = c(0, 500), breaks = c(0, 250, 500)) +
  geom_text(data = centers, aes(x = x, y = y, label = id), color = "white", size = 3, alpha = 1, family = "Merriweather") +
  theme_void() +
  coord_map() +
  labs(title = "Drought Severity in the U.S.",
       subtitle = "Drought Severity and Coverage Index (DSCI) scores for US states between 2000-2022. A DSCI value of 0 means \nthat none of the area is abnormally dry or in drought, and 500 means that all of the area is in D4, exceptional drought.\n \nYear: {closest_state}",
       fill = "DSCI score",
       caption = "@jamie_bio | source = National Integrated Drought Information System") + 
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5,
                               barwidth = unit(25, 'lines'), barheight = unit(1, 'lines'))) +
  theme(legend.position = "bottom",
        legend.justification = "bottom",
        legend.direction = "horizontal",   
        panel.background = element_rect(fill = "white", color = "white"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(family = "Merriweather"),
        legend.title = element_text(family = "Merriweather"),
        plot.title = element_text(family = "Merriweather", face = "bold",
                                  size = 28, hjust = 0.5),
        plot.subtitle = element_text(size = 9, vjust = 0.1, hjust = 0.5,
                                     family = "Merriweather", lineheight = 1.1),
        plot.caption = element_text(family =  "Merriweather", size = 8,
                                    margin = margin(20, 0, 0, 0), hjust = 0.9)))

(anim <- fips_plot +
  transition_states(year, transition_length = 1,
                    state_length = 2)) 

animate(anim, 
        height = 7, width = 8, units = "in", res = 300,
        end_pause = 10)

anim_save(paste0("drought_severity_", format(Sys.time(), "%d%m%Y"), ".gif"))
