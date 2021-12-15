# spice_girls.R
# Jamie Hudson
# Created: 14 December 2021
# Edited: 15 December 2021
# Data: Spice Girl data from Jacquie Tran through Spotify and Genius

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(patchwork)
library(ggforce)
font_add_google("Laila", regular.wt = 400, bold.wt = 500)
font_add_google("Chicle")
showtext_opts(dpi = 320)
showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-12-14')

studio_album_tracks <- tuesdata$studio_album_tracks

# wrangle data ------------------------------------------------------------

songs_df <- studio_album_tracks %>%
  pivot_longer(cols = c(danceability, valence, energy, liveness, speechiness, acousticness), values_to = "val",
               names_to = "metric") %>%
  mutate(metric.num = as.numeric(as.factor(metric))) %>%
  group_by(album_release_date, track_number) %>% 
  mutate(song_id = cur_group_id())

shape_df <- tribble(
  ~x, ~y, ~x1, ~y1, ~x2, ~y2,
  0, -0.3, 1.1, 1.1,  5.5, 1.1,
  0, 1.5, 1.1, 1.25, 5.5, 1.25,
  7, 1.5, 5.9, 1.25, 5.9, 1.25,
  7, -0.3, 5.9, 1.1, 5.9, 1.1
)

arrow <- tribble(
  ~x, ~y, ~x1, ~y1, ~val,
  5.6, 1.185, 0.5, 0, "0",
  5.7, 1.235, 0.5, 0.5, "0.5",
  5.8, 1.185, 0.5, 1, "1"
)
# plot ------------------------------------------------------------

(songs_eq <- songs_df %>% 
   ggplot() +
   geom_segment(mapping = aes(x = as.factor(metric), xend = as.factor(metric), y = 0, yend =1),
                size = 3, colour = "#821120", lineend = "round") +
   geom_shape(shape_df, mapping = aes(x = x, y = y),
              fill = "#ECCFD8", radius = unit(3, 'mm')) +
   geom_shape(shape_df, mapping = aes(x = x1, y = y1),
              fill = "white", radius = unit(1, 'mm')) +
   geom_shape(shape_df, mapping = aes(x = x2, y = y2),
              fill = "#D03994", radius = unit(1, 'mm')) +
   geom_shape(arrow, mapping = aes(x = x, y = y),
              fill = "white") +
   geom_shape(arrow, mapping = aes(x = x, y = y -  c(0.015, 0.12, 0.015)),
              fill = "white") +
   geom_segment(mapping = aes(x = 0.8, xend = 6.2, y = 1, yend =1),
                size = 0.5, colour = "#819985", lineend = "round") +
   geom_segment(mapping = aes(x = 0.8, xend = 6.2, y = 0.5, yend = 0.5),
                size = 0.5, colour = "#819985", lineend = "round") +
   geom_segment(mapping = aes(x = 0.8, xend = 6.2, y = 0, yend = 0),
                size = 0.5, colour = "#819985", lineend = "round") +
   geom_segment(mapping = aes(x = as.factor(metric), xend = as.factor(metric), y = 0, yend =1),
                size = 1.875, colour = "#D03994", lineend = "round") +
   geom_point(mapping = aes(x = as.factor(metric), y = as.numeric(val)), shape = 21,
              size = 10.3125, colour = "grey50", fill = "#1FB9A2") +
   geom_point( mapping = aes(x = as.factor(metric), y = as.numeric(val)), shape = 21,
               size = 5.5, colour = "grey50", fill = "#83D7D0") +
   geom_text(mapping = aes(x = 1.3, y = 1.175, label = track_name),
             size = 4, family = "Laila", hjust = 0) +
   geom_text(mapping = aes(x = 3.5, y = 1.35, label = paste0("Album: ", album_name)),
             size = 6, family = "Laila", fontface = "bold") +
   scale_fill_identity() +
   facet_wrap(~ song_id, ncol = 5) +
   theme(panel.grid = element_blank(),
         panel.background = element_rect(fill = "#fffce3"),
         panel.spacing = unit(2, "lines"),
         plot.background = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         strip.background = element_rect(fill = "#fffce3"),
         strip.text.x = element_blank()) 
)

(eq_legend <- ggplot() +
    geom_segment(mapping = aes(x = c(1,2,3,4,5,6), xend = c(1,2,3,4,5,6), y = 0, yend =1),
                 size = 3, colour = "#821120", lineend = "round") +
    geom_shape(shape_df, mapping = aes(x = x, y = y),
               fill = "#ECCFD8", radius = unit(3, 'mm')) +
    geom_shape(shape_df, mapping = aes(x = x1, y = y1),
               fill = "white", radius = unit(1, 'mm')) +
    geom_shape(shape_df, mapping = aes(x = x2, y = y2),
               fill = "#D03994", radius = unit(1, 'mm')) +
    geom_shape(arrow, mapping = aes(x = x, y = y),
               fill = "white") +
    geom_shape(arrow, mapping = aes(x = x, y = y -  c(0.015, 0.12, 0.015)),
               fill = "white") +
    geom_segment(mapping = aes(x = 0.8, xend = 6.2, y = 1, yend =1),
                 size = 0.5, colour = "#819985", lineend = "round") +
    geom_segment(mapping = aes(x = 0.8, xend = 6.2, y = 0.5, yend = 0.5),
                 size = 0.5, colour = "#819985", lineend = "round") +
    geom_segment(mapping = aes(x = 0.8, xend = 6.2, y = 0, yend = 0),
                 size = 0.5, colour = "#819985", lineend = "round") +
    geom_segment(mapping = aes(x = c(1,2,3,4,5,6), xend = c(1,2,3,4,5,6), y = 0, yend =1),
                 size = 3, colour = "#D03994", lineend = "round") +
    geom_point(mapping = aes(x = c(1,2,3,4,5,6), y = 0.5), shape = 21,
               size = 15, colour = "black", fill = "#1FB9A2") +
    geom_point( mapping = aes(x = c(1,2,3,4,5,6), y = 0.5),  shape = 21,
                size = 8, colour = "black", fill = "#83D7D0") +
    geom_text(mapping = aes(x = 1.3, y = 1.175, label = "Name of song"),
              size = 7, family = "Laila", hjust = 0) +
    geom_text(mapping = aes(x = 3.5, y = 1.35, label = paste0("Album: ", "Name of album")),
              size = 9, family = "Laila", fontface = "bold") +
    geom_text(arrow, mapping = aes(x = 0.3, y = y1, label = val),
              size = 6, family = "Laila") +
    geom_text(mapping = aes(x = c(1,2,3,4,5,6), y = c(rep(c(-0.1, -0.2), 3)),
                            label = c("Acousticness", "Danceability",
                                      "Energy", "Liveness",
                                      "Speechiness", "Valence")),
              size = 6, family = "Laila") +
    scale_fill_identity() +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "#fffce3"),
          panel.border = element_blank(),
          plot.background = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_blank()) 
)

final_plot <- songs_eq +
  plot_annotation(title = "Zigazig-R",
                  subtitle = "Each equalizer displays different Spotify audio features of each \nsong released by the Spice Girls. Acousticness and Liveness relate \nto context, whereas Danceability, Energy, and Valence portray mood. \nAcoustic and Speechiness are consistently low, whereas Energy and \nDanceability are unsurprisingly heavily featured.",
                  caption = "@jamie_bio | source: Jacquie Tran with Spotify and Genius",
                  theme = theme(plot.title = element_text(size = 110,
                                                          margin = margin(20,0,0,0),
                                                          family = "Chicle",
                                                          colour = "#358274",
                                                          hjust = 0.3),
                                plot.subtitle = element_text(size = 30,
                                                             lineheight = 1.1,
                                                             margin = margin(40, 0, 50, 0),
                                                             family = "Laila",
                                                             colour = "#358274"),
                                plot.caption = element_text(size = 25,
                                                            family = "Laila",
                                                            colour = "#358274",
                                                            margin = margin(0, 0, 20, 0)),
                                panel.background = element_rect(fill = "#fffce3"),
                                plot.background = element_rect(fill = "#fffce3",
                                                               colour = "black"),
                                plot.margin = margin(30,30,0,30))) +
  inset_element(eq_legend, left = 0.6, bottom = 1, right = 1, top = 1.25, align_to = 'full', clip = F)

ggsave(paste0("spice_girls_", format(Sys.time(), "%d%m%Y"), ".png"), 
       dpi = 320,
       width = 24,
       height = 30)
