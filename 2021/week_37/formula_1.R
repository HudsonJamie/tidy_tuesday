# formula_one.R
# Jamie Hudson
# Created: 07 September 2021
# Edited: 07 September 2021
# Data: ergast.com/mrd/db

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(showtext)

font_add_google("Racing Sans One", "racing")
font_add_google("Lato", "lato")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 37)

driver_standings <- tuesdata$driver_standings
races <- tuesdata$races
drivers <- tuesdata$drivers
results <- tuesdata$results

# wrangle data ------------------------------------------------------------

# Also add results as "position" from driver_standings is position in whole
# championship, not position in race
driver_results_df <- driver_standings %>% 
  left_join(races, by = "raceId") %>% 
  rename(driver_url = url) %>% 
  left_join(drivers, by = "driverId") %>% 
  left_join(results, by = c("raceId", "driverId")) %>% 
  rename(race.pos = position.y)

# Create colour palettes based on Constructor Team colours

hamilton.pal <- colorRampPalette(c("#afede3", "#0E3B39"))
hamilton.col.pal <- hamilton.pal(6)

schumacher.pal <- colorRampPalette(c("#FFF200", "#BD0000"))
schumacher.col.pal <- schumacher.pal(6)

vettel.pal <- colorRampPalette(c("#ffc906", "#223971"))
vettel.col.pal <- vettel.pal(6)

# Create dfs for arrows

hamilton.arrows <- data.frame(xend = c(18.7, 19.7, 19.7, 20.7, 21.7, 21.7, 17.7),
                                x = c(19.7, 20.7, 20.7, 21.7, 22.7, 22.7, 18.7),
                                yend = c(2008, 2014, 2015, 2017, 2018, 2019, 2020),
                                y = c(2008, 2014, 2015, 2017, 2018, 2019, 2020))



schumacher.arrows <- data.frame(xend = c(16.7, 17.7, 17.7, 17.7, 17.7, 16.7, 18.7),
                            x = c(17.7, 18.7, 18.7, 18.7, 18.7, 17.7, 19.7),
                            yend = c(1994, 1995, 2000, 2001, 2002, 2003, 2004),
                            y = c(1994, 1995, 2000, 2001, 2002, 2003, 2004))



vettel.arrows <- data.frame(xend = c(19.7, 19.7, 20.7, 19.7),
                            x = c(20.7, 20.7, 21.7, 20.7),
                            yend = c(2010, 2011, 2012, 2013),
                            y = c(2010, 2011, 2012, 2013))

# plot ------------------------------------------------------------

# Hamilton plot
driver_results_df %>% 
  filter(driverRef == "hamilton") %>% 
  drop_na() %>% 
  mutate(race.pos = as.numeric(race.pos),
         race.pos.fact = cut(race.pos, breaks = c(0, 1, 2, 3, 4, 5, max(race.pos, na.rm = T)))) %>%
  ggplot(aes(x = round, y = year, fill = race.pos.fact)) +
  scale_y_reverse(breaks = c(2007, 2021)) +
  geom_segment(aes(x = -1, y = 2008, xend = -1, yend = 2020),
               colour = "white", size = 0.25,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = 2, y = 2022, xend = 20, yend = 2022),
               colour = "white", size = 0.25,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_tile(colour = "grey30", size = 1) +
  annotate(geom = "text", label = c("2007", "2021", "Year"), 
           colour = "white", x = c(-1, -1, -2), y = c(2007, 2021, 2014), 
           size = 3.5, angle = c(0, 0, 90), family = "lato") +
  annotate(geom = "text", label = c("1", "21", "Race No."), 
           colour = "white", x = c(1, 21, 10), y = c(2022, 2022, 2023), size = 3.5,
           family = "lato") +
  annotate(geom = "text", label = c("1st Championship", "2nd Championship", "3rd Championship", "4th Championship", "5th Championship", "6th Championship", "7th Championship"), 
           colour = "white", x = c(20, 21, 21, 22, 23, 23, 19), y = c(2008, 2014, 2015, 2017, 2018, 2019, 2020), 
           size = 2.5, hjust = 0, family = "lato") +
  geom_segment(data = hamilton.arrows, aes(x = x, y = y, xend = xend, yend = yend),
               inherit.aes = FALSE,
               colour = "white", size = 0.3,
               arrow = arrow(length = unit(0.1, "cm"))) +
  scale_fill_manual(values = hamilton.col.pal, na.value = "#E8F071",
                    labels = c("1", "2", "3", "4", "5", ">5", "DNF")) +
  lims(x = c(-2, 27)) +
  guides(fill = guide_legend(nrow = 1,
                             title.position = "top")) +
  coord_fixed() +
  labs(title = "Lewis Hamilton Grand Prix positions",
       subtitle = "Lewis Hamilton has won a total of 99 Grand Prix races and 7 Championships.",
       fill = "Final race position",
       caption = "@jamie_bio | source: ergast.com/mrd/db") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(colour = "grey30", fill = "grey30"),
        plot.background = element_rect(colour = "grey30", fill = "grey30"),
        plot.title = element_text(size = 25, family = "racing"),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 6, family = "lato"),
        legend.background = element_rect(fill = "grey30"),
        legend.title.align = 0.5,
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.key.height=grid::unit(0.5,"cm"),
        legend.key.width=grid::unit(0.5,"cm"),
        legend.box.margin = margin(-10, 0, 0, 0),
        text = element_text(colour = "white", family = "lato"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_rect(colour = "grey30"))

ggsave(paste0("hamilton_f1_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 7,
       height = 5)

# Schumacher plot
driver_results_df %>% 
  filter(driverRef == "michael_schumacher") %>% 
  drop_na() %>% 
  mutate(race.pos = as.numeric(race.pos),
         race.pos.fact = cut(race.pos, breaks = c(0, 1, 2, 3, 4, 5, max(race.pos, na.rm = T)))) %>%
  ggplot(aes(x = round, y = year, fill = race.pos.fact)) +
  scale_y_reverse(breaks = c(1991, 2012)) +
  geom_segment(aes(x = -0.7, y = 1992, xend = -0.7, yend = 2011),
               colour = "white", size = 0.25,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = 2, y = 2013.5, xend = 19, yend = 2013.5),
               colour = "white", size = 0.25,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_tile(colour = "grey30", size = 1) +
  annotate(geom = "text", label = c("1991", "2012", "Year"), 
           colour = "white", x = c(-0.7, -0.7, -1.7), y = c(1991, 2012, 2001.5), 
           size = 3.5, angle = c(0, 0, 90), family = "lato") +
  annotate(geom = "text", label = c("1", "20", "Race No."), 
           colour = "white", x = c(1, 20, 10), y = c(2013.5, 2013.5, 2014.5), size = 3.5,
           family = "lato") +
  annotate(geom = "text", label = c("1st Championship", "2nd Championship", "3rd Championship", "4th Championship", "5th Championship", "6th Championship", "7th Championship"), 
           colour = "white", x = c(18, 19, 19, 19, 19, 18, 20), y = c(1994, 1995, 2000, 2001, 2002, 2003, 2004), 
           size = 2.5, hjust = 0, family = "lato") +
  geom_segment(data = schumacher.arrows, aes(x = x, y = y, xend = xend, yend = yend),
               inherit.aes = FALSE,
               colour = "white", size = 0.3,
               arrow = arrow(length = unit(0.1, "cm"))) +
  scale_fill_manual(values = schumacher.col.pal, na.value = "black",
                    labels = c("1", "2", "3", "4", "5", ">5", "DNF")) +
  lims(x = c(-2, 27)) +
  guides(fill = guide_legend(nrow = 1,
                             title.position = "top")) +
  # coord_fixed() +
  labs(title = "Michael Schumacher Grand Prix positions",
       subtitle = "Michael Schumacher won a total of 91 Grand Prix races and 7 Championships.",
       fill = "Final race position",
       caption = "@jamie_bio | source: ergast.com/mrd/db") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(colour = "grey30", fill = "grey30"),
        plot.background = element_rect(colour = "grey30", fill = "grey30"),
        plot.title = element_text(size = 25, family = "racing"),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 6, family = "lato"),
        legend.background = element_rect(fill = "grey30"),
        legend.title.align = 0.5,
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.key.height=grid::unit(0.5,"cm"),
        legend.key.width=grid::unit(0.5,"cm"),
        legend.box.margin = margin(-10, 0, 0, 0),
        text = element_text(colour = "white", family = "lato"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_rect(colour = "grey30"))

ggsave(paste0("schumacher_f1_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 7,
       height = 5)

# Vettel plot
driver_results_df %>% 
  filter(driverRef == "vettel") %>% 
  drop_na() %>% 
  mutate(race.pos = as.numeric(race.pos),
         race.pos.fact = cut(race.pos, breaks = c(0, 1, 2, 3, 4, 5, max(race.pos, na.rm = T)))) %>%
  ggplot(aes(x = round, y = year, fill = race.pos.fact)) +
  scale_y_reverse(breaks = c(2007, 2021)) +
  geom_segment(aes(x = -1, y = 2008, xend = -1, yend = 2020),
               colour = "white", size = 0.25,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = 2, y = 2022, xend = 20, yend = 2022),
               colour = "white", size = 0.25,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_tile(colour = "grey30", size = 1) +
  annotate(geom = "text", label = c("2007", "2021", "Year"), 
           colour = "white", x = c(-1, -1, -2), y = c(2007, 2021, 2014), 
           size = 3.5, angle = c(0, 0, 90), family = "lato") +
  annotate(geom = "text", label = c("1", "21", "Race No."), 
           colour = "white", x = c(1, 21, 10), y = c(2022, 2022, 2023), size = 3.5,
           family = "lato") +
  annotate(geom = "text", label = c("1st Championship", "2nd Championship", "3rd Championship", "4th Championship"), 
           colour = "white", x = c(21, 21, 22, 21), y = c(2010, 2011, 2012, 2013), 
           size =  2.5, hjust = 0, family = "lato") +
  geom_segment(data = vettel.arrows, aes(x = x, y = y, xend = xend, yend = yend),
               inherit.aes = FALSE,
               colour = "white", size = 0.3,
               arrow = arrow(length = unit(0.1, "cm"))) +
  scale_fill_manual(values = vettel.col.pal, na.value = "red",
                    labels = c("1", "2", "3", "4", "5", ">5", "DNF")) +
  lims(x = c(-2, 27)) +
  guides(fill = guide_legend(nrow = 1,
                             title.position = "top")) +
  coord_fixed(clip = "off") +
  labs(title = "Sebastian Vettel Grand Prix positions",
       subtitle = "Sebastian Vettel has won a total of 53 Grand Prix races and 4 Championships.",
       fill = "Final race position",
       caption = "@jamie_bio | source: ergast.com/mrd/db") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(colour = "grey30", fill = "grey30"),
        plot.background = element_rect(colour = "grey30", fill = "grey30"),
        plot.title = element_text(size = 25, family = "racing"),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 6, family = "lato"),
        legend.background = element_rect(fill = "grey30"),
        legend.title.align = 0.5,
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.key.height=grid::unit(0.5,"cm"),
        legend.key.width=grid::unit(0.5,"cm"),
        legend.box.margin = margin(-10, 0, 0, 0),
        text = element_text(colour = "white", family = "lato"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_rect(colour = "grey30"))

ggsave(paste0("vettel_f1_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 7,
       height = 5)

