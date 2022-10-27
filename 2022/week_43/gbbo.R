# gbbo.R
# Jamie Hudson
# Created: 27 October 2022
# Edited: 27 October 2022
# Data: Bakeoff package (https://bakeoff.netlify.app/)

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(showtext)
font_add_google("Sniglet", "sniglet")
font_add_google("Comfortaa", "comfortaa")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)


# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 43)

bakers <- tuesdata$bakers
challenges <- tuesdata$challenges
episodes <- tuesdata$episodes
ratings <- tuesdata$ratings

# wrangle data ------------------------------------------------------------

### NOTE THAT THIS ADAPTED FROM CEDRIC SCHERER'S CODE (https://github.com/z3tt/TidyTuesday/blob/master/R/2020_12_TheOffice.Rmd)
### AND THUS HE SHOULD TAKE ALL OF THE CREDIT FOR THIS.

ave_viewers <- ratings %>% 
  group_by(series) %>% 
  mutate(
    ave_views = mean(viewers_7day),
    episode_mod = episode_count + (5 * series),
    mid = mean(episode_mod)
  ) %>% 
  ungroup() %>% 
  mutate(series = factor(series))

df_lines <-
  ave_viewers %>% 
  group_by(series) %>% 
  summarise(
    start_x = min(episode_mod) - 3,
    end_x = max(episode_mod) + 3,
    y = unique(ave_views)
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
  ggplot(aes(x = episode_mod, y = viewers_7day)) +
  geom_segment(aes(xend = episode_mod,
                   yend = ave_views, 
                   colour = series, 
                   colour = after_scale(colorspace::lighten(colour, .2))),
               size = 1.2) +
  geom_line(data = df_lines,
            aes(x, y),
            colour = "grey80",
            size = 0.3) +
  geom_line(data = df_lines,
            aes(x_group, y,
                colour = factor(series),
                colour = after_scale(colorspace::darken(colour, .2))),
            size = 0.8) +
  geom_label(aes(mid, 17,
                 label = glue::glue(" Season {series} "),
                 colour = factor(series),
                 colour = after_scale(colorspace::darken(colour, .2))),
             fill = NA,
             family = "comfortaa",
             size = 3,
             label.padding = unit(.2, "lines"),
             label.r = unit(.25, "lines"),
             label.size = .5) +
  scale_x_continuous(expand = c(.015, .015)) +
  scale_y_continuous(expand = c(.03, .03),
                     limits = c(1, 20),
                     breaks = seq(1, 15.5, by = 2),
                     sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(values = c("#fbf8cc", "#FDE4CF","#FFCFD2","#F1C0E8", "#CFBAF0",
                                 "#A3C4F3", "#90DBF4", "#8EECF5","#98F5E1",
                                 "#B9FBC0"),
                      guide = "none") +
  scale_fill_manual(values = c("#fbf8cc", "#FDE4CF","#FFCFD2","#F1C0E8", "#CFBAF0",
                               "#A3C4F3", "#90DBF4", "#8EECF5","#98F5E1",
                               "#B9FBC0"),
                    guide = "none") +
  scale_size_binned(name = "No. viewers within 28-day \nwindow from airdate (millions)",
                    range = c(.1, 1.5),
                    n.breaks = 3) +
  annotate("text", x = 88, y = 7, size = 4,
           label = "GBBO switched from \nBBC to Channel 4 \nbefore the 8th season",
           family = "sniglet", colour = "#F2F2F2") +
  annotate("text", x = 120, y = 14, size = 4,
           label = "The final episode of the 7th \nseason was off the tarts!",
           family = "sniglet", colour = "#F2F2F2") +
  geom_curve(x = 107, xend = 100, y = 15, yend = 15.9,
             curvature = 0.2, lineend = "round",
           colour = "white", size = 0.5, 
           arrow = arrow(length = unit(0.4, "cm"),
                         type="closed"),
           ) +
  geom_curve(x = 88, xend = 100, y = 8.8, yend = 11,
             colour = "white", size = 0.5,
             curvature = -0.2, lineend = "round",
             arrow = arrow(length = unit(0.4, "cm"),
                           type="closed")) +
  labs(title = "The Great British Bake Off is all you knead", y = "Number of UK viewers (millions)",
       subtitle = "Viewing figures per GBBO episode dropped after switching from BBC to Channel 4",
       x = NULL,
       caption = "@jamie_bio | source = {bakeoff} R package") +
  guides(size = guide_bins(show.limits = T,
                           direction = "horizontal",
                           title.position = "bottom",
                           title.hjust = 0.5,
                           title.vjust = 0.05,
                           axis.colour = "#F2F2F2",
                           axis.linewidth = 1,
                           override.aes = list(colour = "#F2F2F2"))) +
  theme(plot.background = element_rect(fill = "#313140", colour = "#313140"),
        plot.margin = margin(10, 25, 10, 25),
        panel.background = element_rect(fill = NA, colour = NA),
        panel.border = element_rect(fill = NA, colour = NA),
        panel.grid.major.y = element_line(colour = "#7a7aa3",
                                          linetype = "dotted",
                                          size = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size = 12, 
                                    margin = margin(r = 10)),
        plot.caption = element_text(family = "comfortaa",
                                    size = 8,
                                    colour = "#F2F2F2"),
        legend.position = c(0.5, 0.05),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.width = unit(2.2, "lines"),
        plot.title = element_text(colour = "#F2F2F2",
                                  family = "sniglet",
                                  size = 40,
                                  margin = margin(0, 0, 0, 0)),
        plot.subtitle = element_text(colour = "#F2F2F2",
                                     family = "comfortaa",
                                     size = 17,
                                     margin = margin(10, 0, -20, 0)),
        legend.text = element_text(colour = "#F2F2F2",
                                   family = "comfortaa",
                                   size = 8),
        legend.direction = "horizontal",
        legend.box.just = "center",
        legend.title = element_text(colour = "#F2F2F2",
                                    family = "comfortaa",
                                    size = 8),
        axis.title = element_text(colour = "#F2F2F2",
                                  family = "comfortaa",
                                  hjust = 0.3),
        axis.text = element_text(colour = "#F2F2F2",
                                 family = "comfortaa"))

ggsave(paste0("gbbo_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 12,
       height = 6)
