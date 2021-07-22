# us_droughts.R
# Jamie Hudson
# Created: 21 Jul 2021
# Edited: 22 Jul 2021
# Data: U.S. Drought Monitor (https://droughtmonitor.unl.edu/DmData/DataDownload/ComprehensiveStatistics.aspx)

# load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(geofacet)
library(gganimate)
library(showtext)
font_add_google("Merriweather", "merriweather")
showtext::showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 30)

drought <- tuesdata$drought

# convert the map_date variable to date format

drought <- drought %>% 
  mutate(map_date = ymd(map_date))


# tidy the drought level

drought_level <- drought %>%
  group_by(state_abb, map_date) %>%
  arrange(state_abb, desc(map_date), drought_lvl) %>%
  mutate(area_pct_2 = case_when(drought_lvl == "D4" ~ area_pct,
                                  drought_lvl != "None" ~ 
                                    area_pct - lead(area_pct),
                                  TRUE ~ area_pct),
         pop_pct_2 = case_when(drought_lvl == "D4" ~ pop_pct,
                                drought_lvl != "None" ~ 
                                 pop_pct - lead(pop_pct),
                                TRUE ~ pop_pct)) %>% 
  ungroup()

# Filter for CA

ca_drought <- drought_level %>% 
  filter(state_abb == "CA") %>% 
  mutate(year = year(map_date),
         mnthdate = format(map_date, format="%m/%d"),
         mnthdate = as.Date(mnthdate, "%m/%d")) %>% #mnthdate still shows year (default as the current year). Not end of world here as we overwrite the x axis anyway
  filter(drought_lvl == "D2") %>% 
  select(map_date, mnthdate, year, area_pct)

(plot <- ca_drought %>% 
  ggplot(aes(x = mnthdate, y = area_pct, colour = area_pct)) +
  geom_hline(
    aes(yintercept = y),
    data.frame(y = c(25,50,75)),
    color = "lightgrey"
  ) +
  geom_line(size = 1) +
  scale_colour_viridis_c(option = "inferno", limits = c(0, 100),
                         breaks=c(0, 50, 100), labels=c("0%", "50%", "100%")) +
  scale_x_date(breaks = "1 month", date_labels = "%B") +
  labs(title = "California under drought conditions",
       subtitle = "Percetange of California experiencing at least 'severe drought' conditions \nbetween July 2001 and July 2021. \nYear: {closest_state}",
       x = "Month",
       y = "Percetange of California under X",
       colour = "",
       caption = "@jamie_bio | source = U.S. Drought Monitor") +
  guides(colour = guide_colorbar(title.position = 'bottom', title.hjust = 0.5,
                               barwidth = unit(15, 'lines'), barheight = unit(1, 'lines'))) +
  coord_polar() +
  theme(legend.position = "bottom",
        legend.justification = "bottom",
        legend.direction = "horizontal",   
        panel.background = element_rect(fill = "white", color = "white"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(family = "merriweather", face = "bold",
                                  size = 22),
        plot.subtitle = element_text(size = 12, vjust = 0.1),
        plot.caption = element_text(family =  "merriweather")))

anim <- plot +
  transition_states(year, transition_length = 2,
                    state_length = 2) +
  shadow_mark(color = 'grey50', alpha = 0.5, past = T)

animate(anim, nframes = 500, 
        height = 8, width = 8, units = "in", res = 150)

anim_save(paste0("ca_drought_", format(Sys.time(), "%d%m%Y"), ".gif"))

