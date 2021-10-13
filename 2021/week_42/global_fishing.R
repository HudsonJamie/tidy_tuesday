# global_fishing.R
# Jamie Hudson
# Created: 13 October 2021
# Edited: 13 October 2021
# Data: OurWorldinData.org

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggtext)
library(png)
library(grid)

font_add_google("Oswald", "oswald")
font_add_google("Roboto", "roboto")
showtext_opts(dpi = 320)
showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 42)

sustain <- tuesdata$`fish-stocks-within-sustainable-levels`

boat <- readPNG("img/trawler.png", T)
boatr <- rasterGrob(boat, interpolate = TRUE)

# wrangle data ------------------------------------------------------------

sustain <- sustain %>% 
  filter(Entity == "World") %>% 
  rename(sustainable = 4,
         overexploited = 5)

# plot ------------------------------------------------------------

ggplot(sustain, aes(x = Year, y = overexploited)) +
  geom_rect(xmin = 1965, xmax = 2030, ymin = 35, ymax = 60,
            fill = "#DAEEF0") +
  geom_rect(xmin = 1965, xmax = 2030, ymin = -5, ymax = 35,
            fill = "#72D7DA") +
  annotation_custom(boatr, xmin = 2012, xmax = 2026, ymin = 28, ymax = 50) +
  geom_segment(aes(xend = Year), yend=1,
               linetype = "longdash", colour = "white",
               size = 0.5) +
  geom_segment(yend = 0, y = 0, 
               xend=1972, x = 1973.5,
               linetype = "dotted", colour = "#0575A4",
               size = 0.3) +
  geom_segment(yend = 10, y = 10, 
               xend=1972, x = 1973.5,
               linetype = "dotted", colour = "#0575A4",
               size = 0.3) +
  geom_segment(yend = 34, y = 34, 
               xend=1972, x = 2016.5,
               linetype = "dotted", colour = "#0575A4",
               size = 0.3) +
  geom_line(colour = "white", size = 0.8) +
  geom_point(colour = "#ffb159", size = 4) +
  geom_text(aes(label = Year, y = -1.1, x = Year - 0.6),
            angle = 60, size = 2.5, colour = "#0575A4",
            family = "roboto") +
  annotate("text", label = c("0%", "10%", "34%"), x = c(1971, 1971, 1971), y = c(0, 10, 34),
           size = 3, colour = "#0575A4", fontface = "bold") +
  geom_richtext(label = "J", aes(x = Year - 0.119, y = 0.3),
                size = 4, colour = "white", family = "roboto",
                fill = NA, label.color = NA) +
  geom_richtext(label = "<span style='color:#ff8d0d;'>One third </span>of global fish stocks are <span style='color:#ff8d0d;'>overexploited</span>", 
                x = 1970.5, y = 41, hjust = 0,
                size = 10, family = "oswald",
                fill = NA, label.color = NA) +
  geom_text(label = "The percentage share of fish stocks that are exploited has been rising since 1974. Note the y-axis though, the values are \nstill <50% meaning most fish stocks are within biologically sustainable levels (FAO, 2020).", 
            x = 1971, y = 38.5, hjust = 0, vjust = 1,
            size = 3, family = "roboto") +
  geom_text(label = "@jamie_bio | source: OurWorldinData.org", 
            x = 2016, y = -3, hjust = 0, vjust = 1,
            size = 2, colour = "#0575A4", family = "roboto") +
  lims(x = c(1971, 2023), y = c(-2, 42)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.ticks = element_blank())

ggsave(paste0("global_fishing_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 9,
       height = 6)



