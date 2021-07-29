# olympics.R
# Jamie Hudson
# Created: 28 Jul 2021
# Edited: 28 Jul 2021
# Data: Data

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(countrycode)
library(ggflags)
library(ggtext)
library(patchwork)
library(showtext)
library(png)
font_add_google("Lato", "lato")
showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 31)
olympics <- tuesdata$olympics

# wrangle data ------------------------------------------------------------

# add continents

# obtained noc_regions from Kaggle 
# original csv had typo for Bolivia, 
# had to change Micronesia to "Micronesia (Federated States of)",
# and change UK to "Great Britain"... probably other mistakes too
noc <- read_csv("data/noc_regions.csv") %>% 
  rename(noc = NOC)

olympics_2 <- inner_join(olympics, noc) %>% 
  mutate(cont = countrycode(region, origin = "country.name", destination = "region"))

countries <- olympics_2 %>%
  filter(season == "Summer") %>% 
  group_by(year, region, sex) %>%    
  summarise(n_female = n()) %>% 
  mutate(prop_female = n_female / sum(n_female) * 100) %>% 
  filter(year == 2016,
         sex == "F") %>% 
  arrange(desc(n_female)) %>% 
  ungroup() %>% 
  slice_head(n = 50)

medal.data <- olympics_2 %>% 
  filter(season == "Summer",
         year == 2016,
         !is.na(medal)) %>% 
  select(sport, region, sex) %>% 
  group_by(region, sex) %>% 
  summarise(medals = n()) %>%
  mutate(prop_medals = medals / sum(medals) * 100,
         minprop = min(prop_medals)) ##note issue here with team sports- e.g. football gives 11 medals for team


combined <- inner_join(medal.data, countries) %>% 
  mutate(diff = prop_medals - prop_female) %>% 
  pivot_longer(c("prop_medals", "prop_female"), names_to = "type",
               values_to = "prop") %>% 
  ungroup() %>% 
  mutate(region = fct_reorder(region, diff),
         iso2 = countrycode(region, "country.name", "iso2c"),
         iso2 = tolower(iso2)) %>% 
  filter(diff > 0)

img <- readPNG("data/rings.png", T)
rio <- readPNG("data/rio.png", T)

# plot ------------------------------------------------------------

# percentage of female athletes
(plot_1 <- olympics %>% 
   filter(season == "Summer") %>% 
   group_by(year, sex) %>%       
   summarise(n = n()) %>%
   mutate(freq = n / sum(n) * 100) %>% 
   filter(sex == "F") %>% 
   ungroup() %>% 
   add_row(year = c(1916, 1940, 1944), sex = (rep("F", 3))) %>% 
   ggplot(aes(x = year, y = freq)) +
   geom_line() +
   geom_point(size = 3, shape = 21, fill = "white",
              stroke = 1) +
   geom_hline(yintercept = 50, linetype = 'dashed') +
   annotate("text", label = "Equal representation of male and female athletes", x = 1900, y = 53, size = 2.5,
            colour = "black",
            hjust = 0, family = "lato") +
   annotate("text", label = "In Rio 2016, 45.5% of \ncompetitors were female", x = 2015, y = 30,
            size = 2.5, colour = "black",
            hjust = 0.5, family = "lato") +
   geom_curve(aes(x = 2015, y = 35, xend = 2016, yend = 43),
              arrow = arrow(length = unit(0.07, "inch")),
              size = 0.2, color = "gray20", curvature = 0.2) +
   scale_x_continuous(breaks = seq(1900, 2030, 20),
                      labels = seq(1900, 2030, 20),
                      limits = c(1896, 2016)) +
   scale_y_continuous(labels = function(x) paste0(x, "%"),
                      limits = c(0, 60)) +
   labs(y = "Percentage of competing athletes \nwho are female",
        x = "Year",
        subtitle = "We are approaching an equal percentage of female and male athletes \nat the Summer Olympics.") +
   coord_cartesian(clip = 'off') +
   theme_bw() +
   theme(text = element_text(family = "lato"),
         plot.subtitle = element_text(face = "italic", colour = "#194586",
                                      size = 9),
         panel.grid = element_blank(),
         panel.border = element_blank(),
         axis.line = element_line(colour = "black", size = 0.3),
         legend.position = "none",
         plot.margin = margin(5.5, 50, 5.5, 5.5),
         axis.title.y = element_text(size = 8,
                                     colour = "grey40"),
         axis.text = element_text(family = "lato", size = 6,
                                  colour = "grey40"),
         axis.ticks = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank())
)

# number of events

(plot_2 <- olympics %>% 
    filter(season == "Summer") %>%
    group_by(year, sex) %>%    
    summarise(n.event = n_distinct(event)) %>%
    mutate(freq = n.event / sum(n.event),
           minfreq = min(freq)) %>% 
    ungroup() %>% 
    add_row(year = c(1916, 1916, 1940, 1940, 1944, 1944), sex = (rep(c("M", "F"), 3))) %>% 
    ggplot(aes(x = year, y = n.event, colour = sex)) +
    geom_line(na.rm = F) +
    geom_point(size = 3, shape = 21, fill = "white",
               stroke = 1) +
    scale_x_continuous(breaks = seq(1900, 2030, 20),
                       labels = seq(1900, 2030, 20),
                       limits = c(1896, 2020)) +
    scale_color_manual(values = c("#2D85C3", "#EF3855")) +
    annotate("text", label = "Male", x = 2020, y = 170, size = 2.5,
             colour = "#EF3855", hjust = 0, family = "lato") +
    annotate("text", label = "Female", x = 2020, y = 145, size = 2.5,
             colour = "#2D85C3", hjust = 0, family = "lato") +
    labs(y = "Number of events",
         x = "Year",
         subtitle = "Despite heading towards equality, the number of events that men can compete \nin during each Summer Olympics has always been greater than the number of events \nwomen can compete in.") +
    coord_cartesian(clip = 'off') +
    theme_bw() +
    theme(text = element_text(family = "lato"),
          plot.subtitle = element_text(face = "italic", colour = "#194586",
                                       size = 9),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black", size = 0.3),
          legend.position = "none",
          plot.margin = margin(5.5, 50, 5.5, 5.5),
          axis.text = element_text(family = "lato", size = 6,
                                   colour = "grey40"),
          axis.title = element_text(size = 8, 
                                    colour = "grey40"),
          axis.ticks = element_blank())
)

(plot_1 + inset_element(img, 0.75, 1.2, 1.05, 1.4, 
                        clip = F)) / plot_2 +
  plot_annotation(
    title = 'Women in the Olympics',
    caption = "@jamie_bio | source: rgriffin on Kaggle",
    theme = theme(text = element_text(family = "lato", colour = "#194586"),
                  plot.title = element_markdown(size = 20, face = "bold",
                                                margin = margin(15,0,5,0),
                                                hjust = 0.27),
                  plot.caption = element_text(size = 6, colour = "grey50")
    )) 

ggsave(
  paste0("summer_olympics_1_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 5.5,
  height = 6.4
)

# performance of countries

(plot_3 <- ggplot(combined, aes(x = region, y = prop, colour = type)) +
    geom_line(aes(group = region), colour = "grey80") +
    geom_point(size = 3, shape = 21, fill = "white",
               stroke = 1) +
    geom_flag(y = -3, aes(country = iso2), size = 3) +
    coord_flip(clip = 'off') +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100),
                       labels = function(x) paste0(x, "%"),
                       limits = c(0, 110)) +
    expand_limits(x= c(0, 32)) +
    labs(title = "Superwomen in 2016",
         subtitle = "Twenty-nine countries had female athletes that won a greater percentage of \nmedals than expected, based on the percentage of competing athletes from \nthe country that were female.",
         caption = "@jamie_bio | source: rgriffin on Kaggle") +
    scale_color_manual(values = c("#20A14D", "#F5C300")) +
    annotate("text", label = "The percentage of athletes \nthat were female", x = 30.5, y = 30, size = 2.5,
             colour = "#20A14D") +
    annotate("text", label = "The percentage of medals \nwon by female athletes", x = 30.5, y = 100, size = 2.5,
             colour = "#F5C300") +
    # annotate(geom = "richtext", x = 4.5, y = 15, label = rio_logo, fill = NA, label.color = NA) + 
    theme_bw() +
    theme(text = element_text(family = "lato", colour = "#194586"),
          plot.title = element_markdown(size = 20, face = "bold",
                                        margin = margin(15,0,15,0)),
          plot.subtitle = element_text(face = "italic",
                                       size = 9),
          plot.caption = element_text(size = 6, colour = "grey50"),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line.x = element_line(colour = "black", size = 0.3),
          axis.ticks = element_blank(),
          axis.text = element_text(family = "lato", size = 8,
                                   colour = "grey40"),
          axis.text.x = element_text(size = 6),
          legend.position = "none",
          axis.title = element_blank())
)

(plot_3 + inset_element(rio, 0.7, 1.12, 0.9, 1.23, 
                        clip = F))

ggsave(
  paste0("summer_olympics_2_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 5.5,
  height = 6.4
)
