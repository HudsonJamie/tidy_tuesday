# water_sources.R
# Jamie Hudson
# Created: 06 May 2021
# Edited: 06 May 2021
# Data: Water Point Data Exchange (https://data.waterpointdata.org/dataset/Water-Point-Data-Exchange-WPDx-Basic-/jfkt-jmqa)
## Much of this code was based on this tutorial by Emil Hvitfeld: https://www.hvitfeldt.me/blog/recreate-sankey-flow-chart/

# load libraries ----------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(countrycode)
library(gganimate)
library(RColorBrewer)
library(showtext)
library(ggtext)
library(ggshapes)
library(ggforce)

# load data ---------------------------------------------------------------

tt <- tt_load("2021-05-04")

water <- tt$water

# wrangle data ------------------------------------------------------------

# Sigmoid function from Emil Hvitfeldt
sigmoid <- function(x_from, x_to, y_from, y_to, scale = 5, n = 100) {
  x <- seq(-scale, scale, length = n)
  y <- exp(x) / (exp(x) + 1)
  tibble(x = (x + scale) / (scale * 2) * (x_to - x_from) + x_from,
         y = y * (y_to - y_from) + y_from)
}

# Select countries in Africa
water_africa <- water %>%
  mutate(continent = countryname(country_name, destination = 'continent')) %>%
  filter(continent == "Africa")

# Find 5 countries in Africa with most number of water sources
top_5_africa <- water_africa %>%
  count(country_name) %>%
  slice_max(n, n = 5)

top_5_africa_water <- water_africa %>%
  filter(country_name %in% top_5_africa$country_name) %>%
  select(water_source, facility_type, country_name, install_year) %>%
  mutate(from = 5) %>%
  drop_na(install_year)

data <- top_5_africa_water %>%
  group_by(country_name) %>%
  filter(install_year > 1990) %>% 
  slice_head(n = 100) %>%
  mutate(country_num = case_when(
    country_name == "Ethiopia" ~ 5,
    country_name == "Uganda" ~ 1,
    country_name == "Nigeria" ~ 2,
    country_name == "Sierra Leone" ~ 3,
    country_name == "Liberia" ~ 4
  )) %>%
  ungroup() %>%
  arrange(install_year)

data_2 <- data %>%
  group_by(country_num) %>%
  do(add_row(., install_year = setdiff(seq(1990,2020,1), data$install_year), .before=0
  )) %>%
  arrange(install_year) %>%
  ungroup()

point_data <- map_df(seq_len(nrow(data_2)),
                     ~ sigmoid(-0.2, 1.05, as.numeric(data_2[.x, 5]), as.numeric(data_2[.x, 6])) %>%
                       mutate(time = row_number() + .x,
                              y = y + runif(1, -0.15, 0.15),
                              id = .x) %>%
                       bind_cols(bind_rows(replicate(100, data_2[.x, ], simplify = FALSE)))) %>%
  mutate(data = rep(data_2$install_year, each = 100)) %>%
  mutate(data2 = as.numeric(as.factor(data))) %>%
  group_by(x, data) %>%
  mutate(data3 = data2 + x) %>%
  ungroup() %>% 
  group_by(time) %>% 
  mutate(frame = max(install_year)) %>% 
  ungroup() %>% 
  arrange(install_year)

ending_box <- data_2 %>%
  pull(country_num) %>%
  unique() %>%
  map_df(~ data.frame(x = c(1, 1.99, 1.99, 1),
                      y = c(0.3, 0.3, -0.3, -0.3) + .x,
                      id = .x))

end_points <- point_data %>%
  group_by(id) %>%
  filter(time == max(time)) %>%
  ungroup() %>%
  group_by(country_name) %>%
  mutate(row_n = ifelse(country_name != "NA", row_number(),
                        NA)) %>%
  ungroup() %>%
  mutate(x_1 = 2,
         x_2 = x_1 - (2-1.01)/100*row_n) %>%
  mutate(y_1 = case_when(country_num == "1" ~ 0.7,
                         country_num == "2" ~ 1.7,
                         country_num == "3" ~ 2.7,
                         country_num == "4" ~ 3.7,
                         country_num == "5" ~ 4.7),
         y_2 = case_when(country_num == "1" ~ 1.3,
                         country_num == "2" ~ 2.3,
                         country_num == "3" ~ 3.3,
                         country_num == "4" ~ 4.3,
                         country_num == "5" ~ 5.3)) %>%
  mutate(time = as.integer(time)) %>%
  ungroup()

end_points_1 <- end_points %>% mutate(count = seq(505,1,-1)) %>%
  uncount(count, .id = "tst") %>%
  mutate(time = time + tst - 1) %>%
  do(add_row(., time = seq(2,100,1), .before=0)) %>%
  mutate(time = as.integer(time)) %>%
  group_by(country_num) %>%
  mutate(country_id = row_number()) %>%
  mutate(x_1_new = ifelse(tst == 1 & country_id !=1, lag(x_2),
                          ifelse(tst == 1 & country_id == 1, x_1, NA))) %>%
  ungroup() %>%
  group_by(id) %>%
  fill(x_1_new, .direction = "down")

# plot --------------------------------------------------------------------

anim <- ggplot() +
  geom_point(point_data, mapping = aes(x = x, y = y, colour = install_year),
             size = 3) +
  geom_label(point_data, hjust = 0,
             mapping = aes(label = paste("Current year:", frame), x = -0.8, y = 0.9), color = "black", fill = NA,
  size = 8, label.size = NA, family = "Open Sans Light") +
  geom_rect(data = end_points_1,
            aes(xmin = x_1_new,
                xmax = x_2,
                ymin = y_1,
                ymax = y_2, fill = install_year, group = NA)) +
  geom_path(data = ending_box,
            aes(x, y, group = id), 
            size = 1.5,
            color = "black") +
  coord_flip() +
  annotate(geom="text", x = 2.1, y = 1,
           label="Uganda", family = "Open Sans",
           size = 5.5) +
  annotate(geom="text", x = 2.1, y = 2,
           label="Nigeria", family = "Open Sans",
           size = 5.5) +
  annotate(geom="text", x = 2.1, y = 3,
           label="Sierra Leone", family = "Open Sans",
           size = 5.5) +
  annotate(geom="text", x = 2.1, y = 4,
           label="Liberia", family = "Open Sans",
           size = 5.5) +
  annotate(geom="text", x = 2.1, y = 5,
           label="Ethiopia", family = "Open Sans",
           size = 5.5) +
  lims(y = c(0, 6.8)) +
  expand_limits(x=c(-1,1.5)) +
    labs(title = "Water access points in five African countries",
         subtitle = "Each water droplet represents one water access point that was installed between 1990-2020 from one  \nof the five African countries with the highest number of reported water sources. \nNote that only a random sample of n = 100 water access points per country are represented.",
         fill = "Year of installation",
         caption = "@jamie_bio | source: Water Point Data Exchange") +
  scale_x_reverse() +
    geom_curve(aes(x = -0.6, y = 5.7, xend = -0.2, yend = 4.97), curvature = 0.4,
               size = 20, colour = "grey") +
    geom_curve(aes(x = -0.75, y = 5.7, xend = -0.2, yend = 4.78), curvature = 0.4, colour = "black") +
    geom_curve(aes(x = -0.45, y = 5.7, xend = -0.2, yend = 5.17), curvature = 0.4, colour = "black") +
    geom_rect(aes(ymin = 5.97,
                ymax = 6.03,
                xmin = -1.1,
                xmax = -0.6), fill = "grey", colour = "black") +
    geom_rect(aes(ymin = 5.75,
                  ymax = 6.25,
                  xmin = -0.6,
                  xmax = 2), fill = "grey", colour = "black") +
  geom_lissajous(aes(a = 2, b = 1, delta = 1,
                     x0 = -1.05,
                     y0 = 6,
                     xscale = 0.1,
                     yscale = 0.8), fill = "grey") +
  geom_ellipse(aes(x0 = -0.6, y0 = 6, a = 0.5, b = 0.3, angle = -pi/2, m1 = 3), fill = "grey") +
    geom_ellipse(aes(x0 = -0.2, y0 = 5, a = 0.07, b = 0.3, angle = -pi, m1 = 3), fill = "grey") +
    geom_ellipse(aes(x0 = -1.05, y0 = 6, a = 0.07, b = 0.12, angle = -pi, m1 = 2, m2 = 2), fill = "grey") + 
    scale_color_gradient(low="#1A3661", high="#8CCFFC") +
    scale_fill_gradient(low = "#1A3661", high = "#8CCFFC") +
  theme_void() +
  theme(text = element_text(family = "Open Sans"),
        plot.title = element_text(size = 35, family = "Open Sans SemiBold"),
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(hjust = 0.98),
        legend.position = c(0.3,0.7),
        legend.direction = "horizontal",
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(1, "cm"),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        plot.background = element_rect(fill = "#F9F2E8"),
        plot.margin = margin(15, 0, 5, 15)) +
  guides(color = "none",
         fill = guide_colourbar(title.position = "top",
                                title.hjust = .5)) +
  transition_time(time)
  
animate(anim, nframes = 300, duration = 12, width = 800, height = 600, end_pause = 60)

# save plot

anim_save(
  paste0("water_sources_", format(Sys.time(), "%d%m%Y"), ".gif"))
