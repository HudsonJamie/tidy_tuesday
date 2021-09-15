# us_billboard.R
# Jamie Hudson
# Created: 15 Sep 2021
# Edited: 15 Sep 2021
# Data: Data.World by way of Sean Miller, Billboard.com and Spotify

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(patchwork)
library(showtext)
library(ggtext)

font_add_google("Limelight", "lime")
font_add_google("Montserrat", "mont")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 38)
billboard <- tuesdata$billboard
audio_features <- tuesdata$audio_features

# wrangle data ------------------------------------------------------------
billboard_comb <- left_join(billboard, audio_features) %>% 
  mutate(week_id = mdy(week_id),
         year = year(week_id))

# function to scale values
normalise<-function(m){
  (m - min(m, na.rm = T))/(max(m, na.rm = T)-min(m, na.rm = T))
}

beatles <- billboard_comb %>% 
  filter(str_detect(performer, "The Beatles")) %>% 
  filter(week_position == 1) %>% 
  group_by(song_id) %>% 
  filter(row_number()==1) %>% 
  arrange(week_id) %>% 
  ungroup() %>% 
  mutate(id =  row_number()) %>% 
  mutate(valence_normalised = normalise(valence),
         valence_group = cut(valence_normalised, breaks=c(-0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels=seq(1,10,1)),
         loudness_normalised = normalise(loudness),
         loundess_group = cut(loudness_normalised, breaks=c(-0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels=seq(1,10,1)),
         dance_normalised = normalise(danceability),
         dance_group = cut(dance_normalised, breaks=c(-0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels=seq(1,10,1)),
         speechiness_normalised = normalise(speechiness),
         speechiness_group = cut(speechiness_normalised, breaks=c(-0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels=seq(1,10,1)))

# data frames for high/low notes
beatles_low_valence <- beatles %>% 
  filter(as.numeric(valence_group) <= 6)
beatles_high_valence <- beatles %>% 
  filter(as.numeric(valence_group) > 6)


beatles_low_loud <- beatles %>% 
  filter(as.numeric(loundess_group) <= 6)
beatles_high_loud <- beatles %>% 
  filter(as.numeric(loundess_group) > 6)

beatles_low_speech <- beatles %>% 
  filter(as.numeric(speechiness_group) <= 6)
beatles_high_speech <- beatles %>% 
  filter(as.numeric(speechiness_group) > 6)

beatles_low_dance <- beatles %>% 
  filter(as.numeric(dance_group) <= 6)
beatles_high_dance <- beatles %>% 
  filter(as.numeric(dance_group) > 6)

# data for bar lines
bar_lines <- tibble(x1 = c(6.5, 10.5, 12.5, 14.5, 16.5, 18.5),
                    x2 = c(6.5, 10.5, 12.5, 14.5, 16.5, 18.5),
                    y1 = 3,
                    y2 = 11)
# plot ------------------------------------------------------------

A <- ggplot(beatles, aes(x = id, y = as.numeric(valence_group))) +
    geom_point(size = 2) +
    lims(y = c(-1,15)) +
    geom_hline(yintercept = c(3,5,7,9,11), size = 0.2) +
    geom_segment(data = bar_lines, (aes(x = x1, y = y1, xend = x2, yend = y2)), 
                 colour = "black",
                 size = 0.5) +
    annotate(geom = "text", label = "Bar lines seperate years",
             x = 15, y = 14, size = 2.5, hjust = 0, family = "mont") +
    annotate("curve", x = 14.9, xend = 14.5, y = 14, yend = 12, 
             colour = "black", size = 0.2, arrow = arrow(length = unit(0.15, "cm")),
             curvature = 0.3) +
    annotate(geom = "text", label = "C4 (lowest value)",
             x = 6, y = 0, size = 2.5, hjust = 0, family = "mont") +
    annotate("curve", x = 9, xend = 9.7, y = 0, yend = 0.4, 
             colour = "black", size = 0.2, arrow = arrow(length = unit(0.15, "cm")),
             curvature = 0.1) +
    annotate(geom = "text", label = "D5 (higest value)",
             x = 6.6, y = 14, size = 2.5, hjust = 0, family = "mont") +
    annotate("curve", x = 6.5, xend = 6, y = 14, yend = 12, 
             colour = "black", size = 0.2, arrow = arrow(length = unit(0.15, "cm")),
             curvature = 0.3) +
    annotate(geom = "text", label = "Break (no Spotify data)",
             x = 15, y = 0, size = 2.5, hjust = 0, family = "mont") +
    annotate("curve", x = 17, xend = 18, y = 1.2, yend = 7.6, 
             colour = "black", size = 0.2, arrow = arrow(length = unit(0.15, "cm")),
             curvature = 0.2) +
    geom_segment(data = beatles_low_valence, aes(x = id + 0.11, y = as.numeric(valence_group), xend = id + 0.11, yend = as.numeric(valence_group) + 6), 
                 colour = "black",
                 size = 0.4) +
    geom_segment(data = beatles_high_valence, aes(x = id - 0.11, y = as.numeric(valence_group), xend = id - 0.11, yend = as.numeric(valence_group) - 6), 
                 colour = "black",
                 size = 0.4) +
    geom_rect(aes(xmin = 16.8, ymin = 8.2, xmax = 17.2, ymax = 9), fill = "black") +
    geom_rect(aes(xmin = 17.8, ymin = 8.2, xmax = 18.2, ymax = 9), fill = "black") +
    geom_rect(aes(xmin = 19.8, ymin = 8.2, xmax = 20.2, ymax = 9), fill = "black") +
    labs(title = "Valence (higher value = more positive)") +
    scale_x_discrete(limits= paste0(beatles$song, " (", beatles$year, ")")) +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(family = "lime"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())

B <- ggplot(beatles, aes(x = id, y = as.numeric(loundess_group))) +
  geom_point(size = 2) +
  lims(y = c(-1,15)) +
  geom_hline(yintercept = c(3,5,7,9,11), size = 0.2) +
  geom_segment(data = bar_lines, (aes(x = x1, y = y1, xend = x2, yend = y2)), 
               colour = "black",
               size = 0.5) +
  geom_segment(data = beatles_low_loud, aes(x = id + 0.11, y = as.numeric(loundess_group), xend = id + 0.11, yend = as.numeric(loundess_group) + 6), 
               colour = "black",
               size = 0.4) +
  geom_segment(data = beatles_high_loud, aes(x = id - 0.11, y = as.numeric(loundess_group), xend = id - 0.11, yend = as.numeric(loundess_group) - 6), 
               colour = "black",
               size = 0.4) +
  geom_rect(aes(xmin = 16.8, ymin = 8.2, xmax = 17.2, ymax = 9), fill = "black") +
  geom_rect(aes(xmin = 17.8, ymin = 8.2, xmax = 18.2, ymax = 9), fill = "black") +
  geom_rect(aes(xmin = 19.8, ymin = 8.2, xmax = 20.2, ymax = 9), fill = "black") +
  labs(title = "Loudness") +
  scale_x_discrete(limits= paste0(beatles$song, " (", beatles$year, ")")) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(family = "lime"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

C <- ggplot(beatles, aes(x = id, y = as.numeric(speechiness_group))) +
  geom_point(size = 2) +
  lims(y = c(-1,15)) +
  geom_hline(yintercept = c(3,5,7,9,11), size = 0.2) +
  geom_segment(data = bar_lines, (aes(x = x1, y = y1, xend = x2, yend = y2)), 
               colour = "black",
               size = 0.5) +
  geom_segment(data = beatles_low_speech, aes(x = id + 0.11, y = as.numeric(speechiness_group), xend = id + 0.11, yend = as.numeric(speechiness_group) + 6), 
               colour = "black",
               size = 0.4) +
  geom_segment(data = beatles_high_speech, aes(x = id - 0.11, y = as.numeric(speechiness_group), xend = id - 0.11, yend = as.numeric(speechiness_group) - 6), 
               colour = "black",
               size = 0.4) +
  geom_rect(aes(xmin = 16.8, ymin = 8.2, xmax = 17.2, ymax = 9), fill = "black") +
  geom_rect(aes(xmin = 17.8, ymin = 8.2, xmax = 18.2, ymax = 9), fill = "black") +
  geom_rect(aes(xmin = 19.8, ymin = 8.2, xmax = 20.2, ymax = 9), fill = "black") +
  labs(title = "Speechiness") +
  scale_x_discrete(limits= paste0(beatles$song, " (", beatles$year, ")")) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(family = "lime"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

(D <- ggplot(beatles, aes(x = id, y = as.numeric(dance_group))) +
    geom_point(size = 2) +
    lims(y = c(-1,15)) +
    geom_hline(yintercept = c(3,5,7,9,11), size = 0.2) +
    geom_segment(data = bar_lines, (aes(x = x1, y = y1, xend = x2, yend = y2)), 
                 colour = "black",
                 size = 0.5) +
    geom_segment(data = beatles_low_dance, aes(x = id + 0.11, y = as.numeric(dance_group), xend = id + 0.11, yend = as.numeric(dance_group) + 6), 
                 colour = "black",
                 size = 0.5) +
    geom_segment(data = beatles_high_dance, aes(x = id - 0.11, y = as.numeric(dance_group), xend = id - 0.11, yend = as.numeric(dance_group) - 6), 
                 colour = "black",
                 size = 0.5) +
    geom_rect(aes(xmin = 16.8, ymin = 8.2, xmax = 17.2, ymax = 9), fill = "black") +
    geom_rect(aes(xmin = 17.8, ymin = 8.2, xmax = 18.2, ymax = 9), fill = "black") +
    geom_rect(aes(xmin = 19.8, ymin = 8.2, xmax = 20.2, ymax = 9), fill = "black") +
    labs(title = "Danceability") +
    scale_x_discrete(limits= paste0(beatles$song, " (", beatles$year, ")")) +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(family = "lime"),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,
                                     family = "mont", size = 7))
)


A/B/C/D +
  plot_annotation(title = 'The Beatles` hits in America',
                  subtitle = "The Beatles achieved a total of **20 US Billboard number one hits**.  \nThis sheet music of **scaled data** was created by min-max scaling Spotify's audio features (between 0-1), and  \nsplitting these values into intervals of size 0.1. The subsequent 10 intervals were then assigned a music note from  \nC4 (the lowest value) to E5 (the highest value). Those songs without Spotify data are denoted with a break.",
                  caption = "@jamie_bio | source: Data.World by way of Sean Miller, Billboard.com and Spotify",
                  theme = theme(plot.title = element_text(size = 28,
                                                          colour = "black", family = "lime",
                                                          face = "bold"),
                                plot.subtitle = element_markdown(family = "mont", size = 8, 
                                                             colour = "black",
                                                             lineheight = 1.2),
                                plot.caption = element_text(family = "mont", size = 7,
                                                            colour = "black")))

ggsave(
  paste0("us_billboard_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 6.5,
  height = 8
)
