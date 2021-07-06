# independence_days.R
# Jamie Hudson
# Created: 06 Jul 2021
# Edited: 06 Jul 2021
# Data: Wikipedia (https://en.wikipedia.org/wiki/List_of_national_independence_days)

# load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(patchwork)
library(ggtext)
library(ggfx)
library(showtext)

font_add_google("Raleway", "Raleway")
showtext_auto()

holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')

# Most notorious colonisers seem to be UK, France, and Spain
holidays %>% 
  count(independence_from) %>% 
  arrange(desc(n))

uk <- holidays %>% 
  filter(independence_from %in% c("United Kingdom", "Kingdom of Great Britain",
                                  "United Kingdom of Great Britain and Ireland",
                                  "Australia, New Zealand and the United Kingdom"))

spain <- holidays %>% 
  filter(str_detect(independence_from, "Span|Spain"))
         
france <- holidays %>% 
  filter(str_detect(independence_from, "France"))

# Create data for middle of each month (i.e. where J,F,M etc fits on the figure)
month_day <- yday(seq(as.Date("2021/02/01"), as.Date("2022/01/01"), "months") -1)
month_day <- as.data.frame(month_day) 

month_day_lag <- month_day %>%
  mutate_all(funs(. - lag(.))) %>% 
  rename(month_day_2 = month_day) %>% 
  replace_na(list(month_day_2 = 31))

month_day_2 <- cbind(month_day, month_day_lag)

month_day_df <- month_day_2 %>% 
  mutate(x = (month_day - (month_day_2/2)))

month_name <- c("J", "F", "M", "A", "M", "J", 
                "J", "A", "S", "O", "N", "D")

month_day_final <- cbind(month_day_df, month_name)

# Individual firework plots

(uk_fig <- uk %>% 
  mutate(day = yday(date_parsed)) %>% 
  count(day) %>% 
  ggplot(aes(x = day, y = n)) +
    geom_segment(month_day_final, mapping = aes(x = month_day, xend = month_day, y = 0, yend = 1.7), 
                 colour = "white", alpha = 0.3, linetype = 2) +
    geom_text(data = month_day_final, aes(x = x, y = 2, label = month_name),
              size = 3, colour = "white", alpha = 0.5) +
    with_outer_glow(
      geom_segment(aes(x = day, xend = day, y = 0, yend = n), 
                   colour = "white", size = 0.5), 
      colour = "orange", sigma = 5, expand = 1) +
    with_outer_glow(
      with_inner_glow(geom_point(size=3,alpha=0.75,color="white"),
                      colour = "white", sigma = 1),
      colour="orange", sigma = 10, expand = 3.5) +
    coord_polar()+
  expand_limits(x = 0) +
    theme_void() +
  theme(panel.background = element_rect(fill = NA, color  =  NA),
        panel.grid = element_blank())
)

(fra_fig <- france %>% 
    mutate(day = yday(date_parsed)) %>% 
    count(day) %>% 
    ggplot(aes(x = day, y = n)) +
    geom_segment(month_day_final, mapping = aes(x = month_day, xend = month_day, y = 0, yend = 1.7), 
                 colour = "white", alpha = 0.3, linetype = 2) +
    geom_text(data = month_day_final, aes(x = x, y = 2, label = month_name),
              size = 3, colour = "white", alpha = 0.5) +
    with_outer_glow(
      geom_segment(aes(x = day, xend = day, y = 0, yend = n), 
                   colour = "white", size = 0.5), 
      colour = "blue", sigma = 5, expand = 1) +
    with_outer_glow(
      with_inner_glow(geom_point(size=3,alpha=0.75,color="white"),
                      colour = "white", sigma = 1),
      colour="blue", sigma = 10, expand = 3.5) +
    coord_polar() +
    expand_limits(x = 0) +
    theme_void() +
    theme(panel.background = element_rect(fill = NA, color  =  NA),
          panel.grid = element_blank())
)

(esp_fig <- spain %>% 
  mutate(day = yday(date_parsed)) %>% 
  count(day) %>% 
  ggplot(aes(x = day, y = n)) +
  geom_segment(month_day_final, mapping = aes(x = month_day, xend = month_day, y = 0, yend = 1.7), 
               colour = "white", alpha = 0.3, linetype = 2) +
  geom_text(data = month_day_final, aes(x = x, y = 2, label = month_name),
            size = 3, colour = "white", alpha = 0.5) +
  with_outer_glow(
    geom_segment(aes(x = day, xend = day, y = 0, yend = n), 
                   colour = "white", size = 0.5, alpha = 0.5), 
    colour = "purple", sigma = 5, expand = 1) +
  with_outer_glow(
    with_inner_glow(geom_point(size=3,alpha=0.75,color="white"),
                      colour = "white", sigma = 1),
      colour="purple", sigma = 10, expand = 10) +
  coord_polar() +
  expand_limits(x = 0) +
    annotate("text", x = 277, y = 4, family = "Raleway", size = 2.5,
      color = "white", lineheight = .9, label = "Five countries celebrate Independence Day \nin Central America on September 15th,\n Costa Rica, El Salvador, Guatemala, \nHonduras, and Nicaragua") +
    geom_segment(aes(x = 270, y = 4.5, xend = 260, yend = 4.95), 
                 arrow = arrow(length = unit(0.07, "inch")), 
                 size = 0.4, color = "white") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", color  =  NA),
        panel.grid = element_blank(),
        plot.margin = margin(0,0,-5,0, "cm"))
)

# Combine figures

esp_fig +
  inset_element(uk_fig, 0, 0.6, 0.4, 1) +
  inset_element(fra_fig, 0.6, 0.6, 1, 1) +
  plot_annotation(
    title = 'Freedom fireworks & \nindependence illuminations',
    subtitle = "Each firework represents the day in the year when a country celebrates its independence from <span style='color:#ffe0a6;'>British</span>, <span style='color:#9c9cff'>French</span>, or <span style='color:#ffd4ff'>Spanish</span> rule.  \nThe length of each spark represents the number of countries with a holiday on the same day",
    caption = "@jamie_bio | source: Wikipedia and Isabella Velasquez",
    theme = theme(plot.margin = unit(c(0,0,0,0), "cm"),
                  text = element_text(family ="Raleway"),
      plot.title = with_outer_glow(element_text(
        size = 30,
        colour = "white",
        margin = margin(10, 0, 0, 0),
        hjust = 0.5
      ), colour="white", sigma = 3, expand = 2),
      plot.subtitle = element_markdown(
        colour = "white",
        size = 8,
        hjust = 0.5),
      plot.caption = element_text(
        colour = "white",
        hjust = 0.98,
        size = 6,
        margin = margin(-150, 0, 0)),
      plot.background = element_rect(colour = "black",
                                     fill = "black")))

ggsave(
  paste0("independence_days_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 7.1,
  height = 6.4
)
 