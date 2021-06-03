# survivor.R
# Jamie Hudson
# Created: 03 June 2021
# Edited: 03 May 2021
# Data: From Daniel Oehm who produced the {survivoR} package

library(tidytuesdayR)
library(tidyverse)
library(patchwork)
library(showtext)

font_add_google("Raleway", "Raleway")
showtext_auto()


# Load data ---------------------------------------------------------------

viewers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/viewers.csv')

# Plots -------------------------------------------------------------------

(ratings <- viewers %>% 
  drop_na(rating_18_49) %>% 
  ggplot(aes(x = episode_number_overall, y = 1, fill = rating_18_49)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlGnBu", limits = c(0,10), 
                       breaks = c(0, 5, 10), direction = 1) +
  scale_x_continuous(breaks=c(1, 596),
                   labels=c("Season 1", "Season 40")) +
  coord_cartesian(expand = FALSE) +
  guides(fill = guide_colorbar(title.position = 'bottom', title.hjust = .5,
                                barwidth = unit(20, 'lines'), barheight = unit(1, 'lines'))) +
  labs(fill = "Show rating per episode") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = "grey25", 
                                       fill = "grey25"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(family = "Raleway",
                                   size = 10, colour = "grey97"),
        legend.position = c(0.5, -0.6),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "grey25"),
        legend.text = element_text(family = "Raleway",
                                   colour = "grey97"),
        legend.title = element_text(family = "Raleway", size = 15,
                                    colour = "grey97"),
        plot.margin = margin(10,25,100,25, "pt")))

(viewership <- viewers %>%
    drop_na(viewers) %>% 
    ggplot(aes(x = episode_number_overall, y = 1, fill = viewers)) +
    geom_tile() +
    scale_x_continuous(breaks=c(1, 596),
                       labels=c("Season 1", "Season 40")) +
    scale_fill_distiller(palette = "YlOrRd",
                         limits = c(0,52), 
                         breaks = c(0, 25, 50), direction = 1) +
    guides(fill = guide_colorbar(title.position = 'bottom', title.hjust = .5,
                                 barwidth = unit(20, 'lines'), barheight = unit(1, 'lines'))) +
    labs(fill = "Viewers per episode (millions)") +
    coord_cartesian(expand = FALSE) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(colour = "grey25", 
                                         fill = "grey25"),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(family = "Raleway",
                                     size = 10, colour = "grey97"),
          legend.position = c(0.5, -0.6),
          legend.direction = "horizontal",
          legend.background = element_rect(fill = "grey25"),
          legend.text = element_text(family = "Raleway",
                                     colour = "grey97"),
          legend.title = element_text(family = "Raleway", size = 15,
                                      colour = "grey97"),
          plot.margin = margin(10,25,100,25, "pt")))


viewership / ratings +
  plot_annotation(title = "Survivor, show your stripes",
                  caption = "@jamie_bio | source = Daniel Oehm {survivoR}",
                  subtitle = "Bonus Fact: The song Survivor by Destiny's Child was named after the show, as the group lost three members in one year",
                  theme = theme(text = element_text(family ="Raleway",
                                                    colour = "grey97"),
                                plot.title = element_text(size = 40),
                                plot.title.position = 'plot',
                                plot.subtitle = element_text(size = 11),
                                plot.caption.position = 'plot',
                                plot.background = element_rect(fill = "grey25")))

ggsave(
  paste0("survivor2", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 10,
  height = 7.5
)