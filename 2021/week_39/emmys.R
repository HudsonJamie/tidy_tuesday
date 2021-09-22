# emmys.R
# Jamie Hudson
# Created: 22 September 2021
# Edited: 22 September 2021
# Data: emmys.com

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(ggrepel)
library(ggtext)
library(showtext)

font_add_google("Roboto", "roboto_thin", regular.wt = 100, bold.wt = 500)
font_add_google("Lato", "Lato")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 39)
nominees <- tuesdata$nominees

# wrangle data ------------------------------------------------------------

networks <- nominees %>% 
  filter(distributor %in% c("HBO", "Netflix", "NBC",
                            "FX Networks", "ABC", "CBS",
                            "FOX", "Hulu", "Prime Video",
                            "National Geographic", "National Geographic Channel", 
                            "Showtime", "NGC", "Nat Geo WILD", "NatGeoTV.com",
                            "AMC", "PBS"),
         year >= 2013) %>% 
  mutate(distributor = case_when(distributor == "Prime Video" ~ "AMAZON",
                                 distributor == "National Geographic" ~ "NAT GEO",
                                 distributor == "National Geographic Channel" ~ "NAT GEO",
                                 distributor == "NGC" ~ "NAT GEO",
                                 distributor == "Nat Geo WILD" ~ "NAT GEO",
                                 distributor == "NatGeoTV.com" ~ "NAT GEO",
                                 TRUE ~ distributor),
         distributor = toupper(distributor)) %>% 
  distinct(category, title, .keep_all = T) %>%
  count(distributor, year, sort = T) 

hbo_netflix <- networks %>% 
  filter(distributor %in% c("HBO", "NETFLIX"))

labels <- networks %>% 
  filter(year == 2021)

# plot ------------------------------------------------------------

ggplot(hbo_netflix, aes(x = year, y = n, colour = distributor)) +
  geom_line(data = networks, linetype = "dotted", 
            size = 0.3) + 
  geom_label(aes(label = year, y = 71), fill = "white",
             size = 1.7, colour = "grey80", label.size = NA,
             fontface = "bold") + 
  geom_point(size = 5) +
  geom_line() +
  geom_text(aes(label = n), hjust = 0.5, vjust = 0.5,
            size = 1.7, colour = "white") +
  geom_segment((aes(x = 2013, y = 0, xend = 2021, yend = 0)), 
               colour = "grey90",
               size = 0.3) +
  geom_text_repel(data = labels, 
    aes(color = distributor, label = distributor),
    family = "Lato", fontface = "bold",
    size = 1.7, direction = "y",
    xlim = c(2021.3, NA), hjust = 1,
    segment.size = 0.2,
    segment.alpha = .3,
    segment.linetype = "dotted",
    box.padding = 0.05,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  scale_color_manual(values = c("#2CC6A6", "#FFC000", "#E60F2A", "#56C4D9",
                                "#3B50BB", "#009EAD", "black",
                                "#00C4B0", "#FF9400", "#FFBF2B",
                                "#E60822", "#B89FE1",
                                "#D2D9F0")) +
  coord_cartesian(
    clip = "off") +
  scale_x_continuous(
    expand = c(0.01, 0.3),
    limits = c(2013, 2022), 
    breaks = seq(2013, 2021, 1)
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 155)) +
  labs(title = "<span style='color:#E60822;'>**Netflix**</span> overtakes <span style='color:black;'>**HBO**</span> since the 2017 Emmys",
       subtitle = "This is a recreation of a plot produced by Susie Lu which compared emmy nominations of HBO to Netflix, as well as other networks (up to 2017). \nIt appears the TidyTuesday dataset is missing some data (especially in 2014), as some values do not match Susie's plot (itself a makeover of \nplots via The Verge and Statistica).",
       caption = "@jamie_bio | source: emmys.com") +
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90",
                                          size = 0.3),
        legend.position = "none",
        plot.title = element_markdown(size = 20, family = "roboto_thin"),
        plot.subtitle = element_text(size = 6, family = "roboto_thin",
                                     lineheight = 1.1),
        plot.caption = element_text(size = 4, family = "roboto_thin"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

ggsave(
  paste0("emmys_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 6,
  height = 4.5
)

