# choc_ratings.R
# Jamie Hudson
# Created: Date
# Edited: Date
# Data: Data

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(showtext)
library(waffle)
library(ggforce)
library(patchwork)
font_add_google("Bebas Neue")
font_add_google("Playfair Display")
showtext_opts(dpi = 320)
showtext_auto()

# load dataset ------------------------------------------------------------

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

# wrangle data ------------------------------------------------------------

s_am_countries <- c("Argentina", "Bolivia", "Brazil",
                   "Chile", "Colombia", "Ecuador",  
                   "Guyana", "Paraguay", "Peru",
                   "Suriname", "Uruguay", "Venezuela")

s_am_choc <-
  chocolate %>% 
  filter(country_of_bean_origin %in% s_am_countries) 

s_am.maps <- map_data("world", region = s_am_countries)

# distinct countries of interest
choc_s_am_countires <- s_am_choc %>% 
  filter(rating >= 3.5) %>% 
  distinct(country_of_bean_origin)

# map data of countries of interest
choc.sam.maps <- s_am.maps %>% 
  filter(region %in% choc_s_am_countires$country_of_bean_origin)

# create waffle function
waffle_function <- function(country) {
  s_am_choc %>% 
    filter(rating >= 3.5,
           country_of_bean_origin == country) %>% 
    group_by(country_of_bean_origin) %>% 
    count(rating) %>% 
    ggplot(aes(fill = as.character(rating), values = n)) +
    geom_waffle(n_rows = 8, flip = T,
                colour = "#512E1E", size = 0.5,
                radius = unit(1, "pt")) +
    scale_fill_manual(values = c("#906C50", "#7E3E06", "#3C1C0B")) +
    expand_limits(x=c(0,0), y=c(0,0)) +
    lims(y = c(-1, 17)) +
    geom_text(label = country, mapping = aes(x = 0.5, y = -0.5),
              hjust = 0, family = "Bebas Neue", colour = "grey60", 
              size = 7) +
    labs(fill = NULL, colour = NULL) +
    coord_equal() +
    theme_void() +
    theme(legend.position = "none")
}

# create individual waffles
bolivia_waffle <- waffle_function("Bolivia")
brazil_waffle <- waffle_function("Brazil")
colombia_waffle <- waffle_function("Colombia")
ecuador_waffle <- waffle_function("Ecuador")
peru_waffle <- waffle_function("Peru") +
  geom_text(label = "Peru", mapping = aes(x = 0.5, y = -0.5),
            hjust = 0, family = "Bebas Neue", colour = "#582C80", 
            size = 7) 
venezuela_waffle <- waffle_function("Venezuela") +
  geom_text(label = "Venezuela", mapping = aes(x = 0.5, y = -0.5),
            hjust = 0, family = "Bebas Neue", colour = "#582C80", 
            size = 7) 

# middle of countries for arrow
country_cord <- choc.sam.maps %>% 
  filter(!grepl('Isla', subregion)) %>%
  group_by(region) %>% 
  mutate(lon = mean(long),
         lat = mean(lat)) %>% 
  select(lon, lat, region) %>% 
  filter(row_number()==1) %>% 
  ungroup()

# plot ------------------------------------------------------------

# plot map
(map <- ggplot(s_am.maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group),
               fill = "white", colour = "black") +
    geom_polygon(data = choc.sam.maps,
                 aes(group = group, fill = region),
                 colour = "black") +
    scale_fill_manual(values = c("#BFADDB", "#BFADDB", "#BFADDB",
                                 "#BFADDB", "#582C80", "#582C80")) +
  coord_fixed() +
    lims(x = c(-115, -5),
         y = c(-60, 20)) +
    theme_void() +
    theme(legend.position = "none")
)

# plot legend
(legend <- 
  ggplot() +
    geom_regon(aes(x0 = 1, y0 = 1, sides = 4, 
                   angle = 0, r = 0.7), fill = "#906C50", 
               colour = "#512E1E", size = 0.3, 
               radius = unit(1, "pt")) +
    geom_regon(aes(x0 = 1, y0 = 4, sides = 4, 
                   angle = 0, r = 0.7), fill = "#7E3E06", 
               colour = "#512E1E", size = 0.3,
               radius = unit(1, "pt")) +
    geom_regon(aes(x0 = 1, y0 = 7, sides = 4, 
                   angle = 0, r = 0.7), fill = "#3C1C0B", 
               colour = "#512E1E", size = 0.3,
               radius = unit(1, "pt")) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  lims(y = c(0, 17)) +
  labs(fill = NULL, colour = NULL) +
    coord_equal(clip = "off") +
    theme_void() +
    geom_richtext(label = c("**3.5**:  \nSatisfactory", "**3.75**:  \nPraiseworthy", "**4.0**:  \nSuperior"), 
              mapping = aes(x = c(2,2,2), y = c(1, 4, 7)),
              hjust = 0, family = "Playfair Display", 
              # fontface = "italic",
              size = 4,
              fill = NA, label.color = NA,) +
    theme(legend.position = "none")
)

# bring together
map +
  annotate("curve", x = country_cord$lon[2], xend = -38, y = country_cord$lat[2], yend = -38, 
           colour = "black", size = 0.8, 
           curvature = -0.3, linetype = "dotted") +
  annotate("curve", x = country_cord$lon[6], xend = -25, y = country_cord$lat[6], yend = 3, 
           colour = "black", size = 0.7, 
           curvature = -0.1, linetype = "dotted") +
  annotate("curve", x = country_cord$lon[3], xend = -61.5, y = country_cord$lat[3], yend = 16, 
           colour = "black", size = 0.7, 
           curvature = -0.1, linetype = "dotted") +
  annotate("curve", x = country_cord$lon[4], xend = -88, y = country_cord$lat[4], yend = 6, 
           colour = "black", size = 0.7, 
           curvature = -0.1, linetype = "dotted") +
  annotate("curve", x = country_cord$lon[5] - 2, xend = -91, y = country_cord$lat[5], yend = -18, 
           colour = "black", size = 0.7, 
           curvature = -0.1, linetype = "dotted") +
  annotate("curve", x = country_cord$lon[1], xend = -84, y = country_cord$lat[1], yend = -45, 
           colour = "black", size = 0.7, 
           curvature = -0.1, linetype = "dotted") +
  inset_element(brazil_waffle, left = 0.5, bottom = 0.13, right = 0.9, top = 0.53) +
  inset_element(venezuela_waffle, left = 0.65, bottom = 0.45, right = 1.05, top = 0.85) +
  inset_element(colombia_waffle, left = 0.35, bottom = 0.85, right = 0.75, top = 1.25) +
  inset_element(ecuador_waffle, left = 0, bottom = 0.65, right = 0.4, top = 1.05) +
  inset_element(peru_waffle, left = -0.03, bottom = 0.32, right = 0.37, top = 0.72) +
  inset_element(bolivia_waffle, left = 0.05, bottom = 0.1, right = 0.45, top = 0.5) +
  inset_element(legend, left = 0.65, bottom = 0.05, right = 1.05, top = 0.45) +
  plot_annotation(title = "<span style = 'color:#582C80;'>Venezuela</span> and <span style = 'color:#582C80;'>Peru</span> set the Bar",
                  subtitle = "Each square represents a chocolate bar rated 3.5 or higher manufacutured using South American cacao beans.",
                  caption = "@jamie_bio | source:  Flavors of Cacao by way of Georgios and Kelsey",
                  theme = theme(panel.background = element_blank(),
                                plot.background = element_rect(fill = "white", colour = "white"),
                                plot.title = element_markdown(size = 35,
                                                          colour = "black", family = "Playfair Display",
                                                          face = "bold", hjust = 0.5),
                                plot.subtitle = element_text(family = "Playfair Display", size = 13, 
                                                                 colour = "black",
                                                                 lineheight = 1.2, hjust = 0.5),
                                plot.caption = element_text(family = "Playfair Display", size = 7,
                                                                colour = "black")))

ggsave(
  paste0("choc_ratings_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 9.5,
  height = 8
)
