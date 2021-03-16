## load libraries
library(tidyverse)
library(lubridate)
library(ggdark)
library(ggtext)

## load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 12)
games <- tuesdata$games

football_manager_monthly <- games %>%
  filter(str_detect(gamename, "Football Manager"),
         !str_detect(gamename, "Touch")) %>%
  mutate(month = factor(month,
                        levels = c("January", "February", "March",
                                   "April", "May", "June",
                                   "July", "August", "September",
                                   "October", "November", "December")),
         date = make_date(year, month)) 



# Create dataframe with coords for arrows
seasons <- tribble(
  ~season, ~start,
  "2012", "2012-08-01",
  "2013", "2013-08-01",
  "2014", "2014-08-01",
  "2015", "2015-08-01",
  "2016", "2016-08-01",
  "2017", "2017-08-01",
  "2018", "2018-08-01",
  "2019", "2019-08-01",
  "2020", "2020-09-01"
)

seasons_df <- football_manager_monthly %>% 
  filter(date %in% as.Date(seasons$start)) %>% 
  mutate(gamename_1 = gamename) %>% 
  separate(gamename_1, c(NA, NA, "game_year"), convert = T) %>% 
  filter(year == game_year)


p.subtitle <- glue::glue("The number of concurrent players that play the Football Manager (FM) games on Steam.
Each game has \n
reasonably consistant patterns, with a <span style='color:#FFFF00'>peak in player numbers each January (Christmas presents?)</span>. \n
Interestingly, the number of concurrent players playing <span style='color:#FFFF00'>drops rapidly once the following \n
<span style='color:#FFFF00'>football season starts</span> (represented by the arrows on the plot). The <span style='color:#FFFF00'>largest spike in early 2020 \n
<span style='color:#FFFF00'>is the begining of the pandemic</span>.")

ggplot() +
  geom_line(data = football_manager_monthly, aes(x = date, y = avg, colour = gamename), size = 1.5) +
  geom_point(data = football_manager_monthly, aes(x = date, y = avg, colour = gamename), size = 2.5) + 
  scale_colour_manual(values = c("#FAC339", "#F06B00", "#F84D4D", "#CD6093", "#FBAEB6", "#7DF2CF", 
                                 "#47F3FC", "#99E550"
                                 )) +
  geom_segment(aes(x = seasons_df$date, y = seasons_df$avg + 10000, xend = seasons_df$date, yend = seasons_df$avg + 3000),
               arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", x = seasons_df$date + 10, y = seasons_df$avg + 12000, label = rev(c("2012/13",
                                                                              "2014/15",
                                                                              "2015/16",
                                                                              "2016/17",
                                                                              "2017/18",
                                                                              "2018/19",
                                                                              "2020/21")),
           family = "Barcade Brawl",
           size = 2) +
  annotate("rect", xmin = as.Date("2012-01-01"), xmax = as.Date("2014-01-01"), ymin = 65000, ymax = 84000,
           alpha = .8, fill = "black", colour = "white") +
  geom_segment(aes(x = as.Date("2013-01-01"), y = 75000, xend = as.Date("2013-01-01"), yend = 67000),
               size = 1,
               arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", x = as.Date("2013-01-01"), y = 80000, label = "start of \nfootball season", 
           family = "Barcade Brawl",
           size = 3) +
  labs(title = "Football Manager  on Steam",
       subtitle = p.subtitle,
       caption = "@jamie_bio | source: STEAM\n") +
  ylab("Average number of
       \n concurrent players \n") +
  dark_theme_grey() +
  theme(panel.grid.major = element_line(colour = "grey20"),
        panel.grid.minor.x = element_line(colour = "grey20"),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(family = "Barcade Bold", size = 30),
        plot.subtitle = element_markdown(size = 7, margin = margin(t = 10, b = 10)),
        plot.caption = element_markdown(size = 5),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 6),
        axis.title.x = element_blank(),
        text = element_text(family = "Barcade Brawl",
                            colour = "white"))

ggsave("steam_plot.png",
       width = 31,
       height = 20,
       unit = "cm",
       dpi = 320,
       type = "cairo-png")
 

