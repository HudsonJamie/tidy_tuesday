# doctor_who.R
# Jamie Hudson
# Created: 25 Nov 2021
# Edited: 25 Nov 2021
# Data: {datardis} package by way of Jonathan Kitt

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(showtext)
font_add(family = "Genos", regular = "Genos-VariableFont_wght.ttf",
         bold = "Genos-VariableFont_wght.ttf")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-11-23')
directors <- tuesdata$directors
episodes <- tuesdata$episodes
writers <- tuesdata$writers
imdb <- tuesdata$imdb

# wrangle data ------------------------------------------------------------

### NOTE THAT THIS ADAPTED FROM CEDRIC SCHERER'S CODE (https://github.com/z3tt/TidyTuesday/blob/master/R/2020_12_TheOffice.Rmd)
### AND THUS HE SHOULD TAKE ALL OF THE CREDIT FOR THIS.

ave_viewers <- episodes %>% 
  rename(season = season_number,
         ep_num = episode_number) %>% 
  drop_na(era) %>% 
  filter(season != 13) %>% 
  arrange(season, ep_num) %>% 
  mutate(episode_id = row_number()) %>%
  group_by(season) %>% 
  mutate(
    avg = mean(uk_viewers),
    episode_mod = episode_id + (9 * season),
    mid = mean(episode_mod)
  ) %>% 
  ungroup() %>% 
  mutate(season = factor(season))

df_lines <-
  ave_viewers %>% 
  group_by(season) %>% 
  summarise(
    start_x = min(episode_mod) - 5,
    end_x = max(episode_mod) + 5,
    y = unique(avg)
  ) %>% 
  pivot_longer(
    cols = c(start_x, end_x),
    names_to = "type",
    values_to = "x"
  ) %>% 
  mutate(
    x_group = if_else(type == "start_x", x + .1, x - .1),
    x_group = if_else(type == "start_x" & x == min(x), x_group - .1, x_group),
    x_group = if_else(type == "end_x" & x == max(x), x_group + .1, x_group)
  )

# plot ------------------------------------------------------------

ave_viewers %>% 
  ggplot(aes(episode_mod, uk_viewers)) +
  geom_segment(aes(xend = episode_mod,
                   yend = avg, 
                   colour = season, 
                   colour = after_scale(colorspace::lighten(colour, .2))),
               linetype = "dashed",
               size = 0.3) +
  geom_line(data = df_lines,
            aes(x, y),
            colour = "grey80",
            size = 0.3) +
  geom_line(data = df_lines,
            aes(x_group, y, 
                colour = factor(season), 
                colour = after_scale(colorspace::darken(colour, .2))),
            size = 1.5) +
  geom_point(aes(size = rating,
                 fill = season,
                 colour = season),
             pch = 23) +
  geom_label(aes(mid, 14,
                 label = glue::glue(" Season {season} "),
                 colour = factor(season),
                 colour = after_scale(colorspace::darken(colour, .2))),
             fill = NA,
             family = "Genos",
             label.padding = unit(.2, "lines"),
             label.r = unit(.25, "lines"),
             label.size = .5) +
  scale_x_continuous(expand = c(.015, .015)) +
  scale_y_continuous(expand = c(.03, .03),
                     limits = c(4, 14.5),
                     breaks = seq(6, 13.5, by = 2),
                     sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(values = c("#005f73","#0a9396","#94d2bd", "#BFD5B2",
                                "#e9d8a6", "#E9C46A", "#ee9b00","#ca6702",
                                "#E76F51", "#bb3e03", "#ae2012","#9b2226"),
                     guide = "none")+
  scale_fill_manual(values = c("#005f73","#0a9396","#94d2bd", "#BFD5B2",
                               "#e9d8a6", "#E9C46A", "#ee9b00","#ca6702",
                               "#E76F51", "#bb3e03", "#ae2012","#9b2226"),
                    guide = "none") +
  scale_size_binned(name = "Episode rating (%)",
                    range = c(.3, 3)) +
  annotate("text", x = 130, y = 13, size = 4,
           label = "The two highest viewed episodes \nare both Christmas episodes",
           family = "Genos", colour = "white") +
  annotate("curve", x = 107, xend = 94, y = 13, yend = max(ave_viewers$uk_viewers), 
           colour = "white", size = 0.2, arrow = arrow(length = unit(0.15, "cm")),
           curvature = -0.1) +
  annotate("curve", x = 130, xend = 134, y = 12.5, yend = sort(ave_viewers$uk_viewers, TRUE)[2] + 0.1, 
           colour = "white", size = 0.2, arrow = arrow(length = unit(0.15, "cm")),
           curvature = 0.1) +
  labs(title = "Doctor Who: An Unexpected Christmas Tradition?", y = "Number of UK viewers (in millions)",
       subtitle = "Viewing figures for episodes of the post-2005 Revived Era (excluding TV specials)",
       x = NULL,
       caption = "@jamie_bio | source = {datardis} R package") +
  guides(size = guide_bins(show.limits = T,
                           direction = "horizontal",
                           title.position = "bottom",
                           title.hjust = 0.5,
                           title.vjust = 0.05,
                           axis.colour = "white",
                           axis.linewidth = 1,
                           override.aes = list(colour = "white"))) +
  theme(plot.background = element_rect(fill = "#09131C", colour = "#fafaf5"),
        plot.margin = margin(10, 25, 10, 25),
        panel.background = element_rect(fill = NA, colour = NA),
        panel.border = element_rect(fill = NA, colour = NA),
        panel.grid.major.y = element_line(colour = "grey20",
                                          linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size = 14, 
                                    margin = margin(r = 10)),
        plot.caption = element_text(family = "Genos",
                                    size = 8,
                                    colour = "grey70"),
        legend.position = c(.1, .08),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.width = unit(2, "lines"),
        plot.title = element_text(colour = "white",
                                  family = "Genos",
                                  size = 50,
                                  margin = margin(5, 0, 0, 0)),
        plot.subtitle = element_text(colour = "white",
                                   family = "Genos",
                                   size = 20,
                                   margin = margin(10, 0, -10, 0)),
        legend.text = element_text(colour = "white",
                                   family = "Genos",
                                   size = 10),
        legend.title = element_text(colour = "white",
                                    family = "Genos",
                                    size = 12),
        axis.title = element_text(colour = "white",
                                  family = "Genos"),
        axis.text = element_text(colour = "white",
                                 family = "Genos"))

ggsave(paste0("doctor_who_viewership_3_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 12,
       height = 6)

