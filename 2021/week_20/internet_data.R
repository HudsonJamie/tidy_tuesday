# internet_Data.R
# Jamie Hudson
# Created: 14 May 2021
# Edited: 14 May 2021
# Data: Microsoft (https://github.com/microsoft/USBroadbandUsagePercentages)

# load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(zipcodeR)
library(cowplot)
library(usdata)
library(janitor)
library(ggforce)
library(geofacet)
library(showtext)

# load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 20)

broadband <- tuesdata$broadband_zip

broadband$state <- abbr2state(broadband$ST)

# wrangle data ------------------------------------------------------------

broadbands <- broadband %>% 
  group_by(state) %>% 
  clean_names() %>% 
  summarise(broadband_usage = mean(as.numeric(broadband_usage), na.rm = T)) %>% 
  mutate(circle = case_when(broadband_usage >= 0.15 ~ "green",
                            broadband_usage < 0.15 ~ "grey60"),
         bar_one = case_when(broadband_usage >= 0.30 ~ "green",
                             broadband_usage < 0.30 ~ "grey60"),
         bar_two = case_when(broadband_usage >= 0.45 ~ "green",
                             broadband_usage < 0.45 ~ "grey60"),
         bar_three = case_when(broadband_usage >= 0.60 ~ "green",
                             broadband_usage < 0.60 ~ "grey60"))

# plots -------------------------------------------------------------------

# plot legend

legend <- ggplot() +
  geom_circle(aes(x0 = 0, y0 = 1, r = .8), fill = "green", colour = "grey15") +
  geom_arc_bar(aes(x0 = 0, y0 = 1, r0 = 1.5, r = 2.5, start = 0.8,
                   end =  -0.8), fill = "green", radius = unit(2.5, 'mm'), colour = "grey15") +
  geom_arc_bar(aes(x0 = 0, y0 = 1, r0 = 3.5, r = 4.5, start = 0.8,
                   end =  -0.8), fill = "green", radius = unit(2.5, 'mm'), colour = "grey15") +
  geom_arc_bar(aes(x0 = 0, y0 = 1, r0 = 5.5, r = 6.5, start = 0.8,
                   end =  -0.8), fill = "green", radius = unit(2.5, 'mm'), colour = "grey15") +
  annotate("text", x = 2.2, y = 1, size = 5, color = "white",
           label = ">15%",
           family = "Roboto Mono") +
  annotate("text", x = 3, y = 2.6, size = 5, color = "white",
           label = ">30%",
           family = "Roboto Mono") +
  annotate("text", x = 4.4, y = 4, size = 5, color = "white",
           label = ">45%",
           family = "Roboto Mono") +
  annotate("text", x = 6.1, y = 6, size = 5, color = "white",
           label = ">60%",
           family = "Roboto Mono") +
  coord_fixed() +
  lims(x = c(-4.7, 9)) +
  theme(plot.title = element_text(colour = "white"),
        plot.background = element_blank(),
        panel.background = element_rect(fill = "grey15"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# main plot

map <- ggplot(broadbands) +
  geom_circle(aes(x0 = 0, y0 = 1, r = 0.9,  fill = circle), colour = "grey15") +
  lims(x = c(-5.5, 5.5),
       y = c(0, 8.5)) +
  geom_arc_bar(aes(x0 = 0, y0 = 1, r0 = 1.5, r = 2.5, start = 0.8,
                   end =  -0.8, fill = bar_one), radius = 0.039, colour = "grey15") +
  geom_arc_bar(aes(x0 = 0, y0 = 1, r0 = 3.5, r = 4.5, start = 0.8,
                   end =  -0.8, fill = bar_two), radius = 0.039, colour = "grey15") +
  geom_arc_bar(aes(x0 = 0, y0 = 1, r0 = 5.5, r = 6.5, start = 0.8,
                   end =  -0.8, fill = bar_three), radius = 0.039, colour = "grey15") +
  scale_fill_identity() +
  coord_fixed() +
  labs(title = "US Broadband Speeds",
       subtitle = "\nThe percentage of residents in each state that \nhave access to fixed terrestrial broadband \nspeeds of 25 Mbps/3 Mbps\n \n",
       caption = "@jamiebio | source: Microsoft") +
  theme(text = element_text(family = "Roboto Mono"),
        plot.title = element_text(colour = "white", size = 50),
        plot.subtitle = element_text(colour = "white", size = 20),
        plot.caption = element_text(colour = "white"),
        plot.background = element_rect(fill = "grey15"),
        panel.background = element_rect(fill = "grey15"),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = "white", size = 15),
        strip.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(20,55,20,55)) +
  facet_geo(~ state, grid = "us_state_grid2", label = "code")

ggdraw(map) +
  draw_plot(legend, x = 0.54, y = 0.8, 0.65, 0.17)

ggsave(paste0("internet_data", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 12.25,
       height = 11.15,
       type = "cairo-png")
