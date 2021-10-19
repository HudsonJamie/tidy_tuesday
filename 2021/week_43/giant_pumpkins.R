# giant_pumpkins.R
# Jamie Hudson
# Created: 19 October 2021
# Edited: 19 October 2021
# Data: BigPumpkins.com

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(statebins)
library(showtext)
library(patchwork)
library(ggtext)
font_add_google("Merriweather")
font_add_google("Open Sans")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 43)
pumpkins <- tuesdata$pumpkins

# wrangle data ------------------------------------------------------------

pumpkins_df <- pumpkins %>% 
  separate(id, c("year", "type"), sep = "-") %>% 
  filter(place != c("EXH", "DMG")) %>% 
  mutate(weight_lbs = parse_number(weight_lbs),
         place = parse_number(place))

# plot ------------------------------------------------------------

giant_pumpkin <- pumpkins_df %>% 
  filter(type == "P",
         country == "United States") %>% 
  group_by(state_prov) %>% 
  mutate(med_weight = median(weight_lbs)) %>% 
  mutate(state = state_prov)
  
A <- ggplot(giant_pumpkin, aes(state = state, fill = med_weight)) +
  geom_statebins(lbl_size = 2.5) +
  coord_equal() +
  labs(title = "<span style = 'color:#F26814;'>Giant Pumpkin</span>",
       fill = "Median Weight (lbs)") +
  scale_fill_distiller(palette = "Oranges",
                       direction = 1) +
  guides(fill = guide_colorbar(title.position = 'top',
                               direction = "horizontal",
                               title.hjust = .5,
                               barwidth = 5,
                               barheight = 0.8)) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.title = element_markdown(family = "Merriweather", hjust = 0.2),
        legend.text = element_text(family = "Open Sans", size = 5),
        legend.title = element_text(family = "Open Sans", size = 8),
        legend.position = "bottom")

watermelon <- pumpkins_df %>% 
  filter(type == "W",
         country == "United States") %>% 
  group_by(state_prov) %>% 
  mutate(med_weight = median(weight_lbs)) %>% 
  mutate(state = state_prov)

B <- ggplot(watermelon) +
  geom_statebins(aes(state = state, fill = med_weight)) +
  coord_equal() +
  labs(title = "<span style = 'color:#42AA5D;'>Giant Watermelon</span>",
       fill = "Median Weight (lbs)") +
  scale_fill_distiller(palette = "YlGn",
                       direction = 1) +
  guides(fill = guide_colorbar(title.position = 'top',
                               direction = "horizontal",
                               title.hjust = .5,
                               barwidth = 5,
                               barheight = 0.8)) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.title = element_markdown(family = "Merriweather", hjust = 0.2),
        legend.text = element_text(family = "Open Sans", size = 5),
        legend.title = element_text(family = "Open Sans", size = 8),
        legend.position = "bottom")


tomatoes <- pumpkins_df %>% 
  filter(type == "T",
         country == "United States") %>% 
  group_by(state_prov) %>% 
  mutate(med_weight = median(weight_lbs)) %>% 
  mutate(state = state_prov)

C <- ggplot(tomatoes, aes(state = state, fill = med_weight)) +
  geom_statebins(lbl_size = 2.5) +
  coord_equal() +
  labs(title = "<span style = 'color:#CB1C1D;'>Tomato</span>",
       fill = "Median Weight (lbs)") +
  scale_fill_distiller(palette = "Reds",
                       direction = 1) +
  guides(fill = guide_colorbar(title.position = 'top',
                               direction = "horizontal",
                               title.hjust = .5,
                               barwidth = 5,
                               barheight = 0.8)) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.title = element_markdown(family = "Merriweather", hjust = 0.2),
        legend.text = element_text(family = "Open Sans", size = 5),
        legend.title = element_text(family = "Open Sans", size = 8),
        legend.position = "bottom")

A + B + C +
  plot_annotation(title = "Great Pumpkin Commonwealth",
                           caption = "@jamie_bio | source = BigPumpkins.com",
                           subtitle = "The median weight of Giant Pumpkins, Giant Watermelon, and Tomatoes grown in each state entered into Great Pumpkin Commonwealth competitions.",
                           theme = theme(text = element_text(family ="Merriweather"),
                                         plot.title = element_text(size = 20),
                                         plot.title.position = 'plot',
                                         plot.subtitle = element_text(size = 11,
                                                                      family ="Open Sans"),
                                         plot.caption.position = 'plot',
                                         plot.caption = element_text(size = 6)))

ggsave(
  paste0("giant_pumpkins_med_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 12,
  height = 5
)

#max

giant_pumpkin_max <- pumpkins_df %>% 
  filter(type == "P",
         country == "United States") %>% 
  group_by(state_prov) %>% 
  mutate(max_weight = max(weight_lbs)) %>% 
  mutate(state = state_prov)

D <- ggplot(giant_pumpkin_max, aes(state = state, fill = max_weight)) +
  geom_statebins(lbl_size = 2.5) +
  coord_equal() +
  labs(title = "<span style = 'color:#F26814;'>Giant Pumpkin</span>",
       fill = "Maximum Weight (lbs)") +
  scale_fill_distiller(palette = "Oranges",
                       direction = 1) +
  guides(fill = guide_colorbar(title.position = 'top',
                               direction = "horizontal",
                               title.hjust = .5,
                               barwidth = 6,
                               barheight = 0.8)) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.title = element_markdown(family = "Merriweather", hjust = 0.2),
        legend.text = element_text(family = "Open Sans", size = 5),
        legend.title = element_text(family = "Open Sans", size = 8),
        legend.position = "bottom")

watermelon_max <- pumpkins_df %>% 
  filter(type == "W",
         country == "United States") %>% 
  group_by(state_prov) %>% 
  mutate(max_weight = max(weight_lbs)) %>% 
  mutate(state = state_prov)

E <- ggplot(watermelon_max) +
  geom_statebins(aes(state = state, fill = max_weight)) +
  coord_equal() +
  labs(title = "<span style = 'color:#42AA5D;'>Giant Watermelon</span>",
       fill = "Maximum Weight (lbs)") +
  scale_fill_distiller(palette = "YlGn",
                       direction = 1) +
  guides(fill = guide_colorbar(title.position = 'top',
                               direction = "horizontal",
                               title.hjust = .5,
                               barwidth = 6,
                               barheight = 0.8)) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.title = element_markdown(family = "Merriweather", hjust = 0.2),
        legend.text = element_text(family = "Open Sans", size = 5),
        legend.title = element_text(family = "Open Sans", size = 8),
        legend.position = "bottom")


tomatoes_max <- pumpkins_df %>% 
  filter(type == "T",
         country == "United States") %>% 
  group_by(state_prov) %>% 
  mutate(max_weight = max(weight_lbs)) %>% 
  mutate(state = state_prov)

F <- ggplot(tomatoes_max, aes(state = state, fill = max_weight)) +
  geom_statebins(lbl_size = 2.5) +
  coord_equal() +
  labs(title = "<span style = 'color:#CB1C1D;'>Tomato</span>",
       fill = "Maximum Weight (lbs)") +
  scale_fill_distiller(palette = "Reds",
                       direction = 1) +
  guides(fill = guide_colorbar(title.position = 'top',
                               direction = "horizontal",
                               title.hjust = .5,
                               barwidth = 6,
                               barheight = 0.8)) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.title = element_markdown(family = "Merriweather", hjust = 0.2),
        legend.text = element_text(family = "Open Sans", size = 5),
        legend.title = element_text(family = "Open Sans", size = 8),
        legend.position = "bottom")

D + E + F +
  plot_annotation(title = "Great Pumpkin Commonwealth",
                  caption = "@jamie_bio | source = BigPumpkins.com",
                  subtitle = "The maximum weight of Giant Pumpkins, Giant Watermelons, and Tomatoes grown in each state entered into Great Pumpkin Commonwealth competitions.",
                  theme = theme(text = element_text(family ="Merriweather"),
                                plot.title = element_text(size = 20),
                                plot.title.position = 'plot',
                                plot.subtitle = element_text(size = 11,
                                                             family ="Open Sans"),
                                plot.caption.position = 'plot',
                                plot.caption = element_text(size = 6)))

ggsave(
  paste0("giant_pumpkins_max_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 12,
  height = 5
)
