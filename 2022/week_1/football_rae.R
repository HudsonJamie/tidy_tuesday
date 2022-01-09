# football_rae.R
# Jamie Hudson
# Created: 9 Jan 2022
# Edited: 9 Jan 2022
# Data: ONS, transfermarkt, and UNdata

# load libraries ------------------------------------------------------------

library(worldfootballR)
library(tidyverse)
library(showtext)
library(cowplot)
library(patchwork)
library(ggtext)
font_add_google("Lato")
font_add_google("DM Serif Text")
showtext_opts(dpi = 320)
showtext_auto()

# load dataset ------------------------------------------------------------

# load spain birth data (from http://data.un.org/Data.aspx?d=POP&f=tableCode%3A55#POP)

spainbirth <- read_csv("data/UNdata_Export_spain.csv") 

# load italy birth data (from http://data.un.org/Data.aspx?d=POP&f=tableCode%3A55#POP)

italybirth <- read_csv("data/UNdata_Export_italy.csv") 

# load UK birth data (available from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/articles/howpopularisyourbirthday/2015-12-18)

ukbirth <- read_csv("data/eng_data.csv") 


# Premier League ----------------------------------------------------------

prem_league_url_2012 <- tm_league_team_urls("England", 2012, league_url = NA)


prem_players_2012 =  list()

for(i in 1:length(prem_league_url_2012)) {
  prem_players_2012[[i]] <- tm_team_player_urls(prem_league_url_2012[[i]])
}

prem_data_2012 <- unlist(prem_players_2012)

prem_2012 <- tm_player_bio(prem_data_2012) %>% 
  drop_na(date_of_birth)

prem_2012$date_of_birth <- lubridate::ymd(prem_2012$date_of_birth)
prem_2012$birth_month <- lubridate::month(prem_2012$date_of_birth)

prem_eng_2012 <- prem_2012 %>% 
  filter(citizenship %in% c("England"))

prem_eng_2012_df <- prem_eng_2012 %>% 
  group_by(birth_month) %>% 
  summarise(count = n()) %>% 
  ungroup()


ukbirth_tidy <- ukbirth %>% 
  mutate(month = str_extract(date, '\\b\\w+$'),
         month2 = as.Date(paste0(date, "-2020"), format = "%d-%b-%Y"),
         month3 = lubridate::month(month2)) %>% 
  group_by(month3) %>% 
  summarise(mean = mean(average)) %>% 
  ungroup() %>% 
  mutate(total = sum(mean),
         month_prop = mean/total,
         month_prop_football = month_prop*nrow(prem_eng_2012))

eng_join <- full_join(prem_eng_2012_df, ukbirth_tidy, by = c("birth_month" = "month3")) %>% 
  select(birth_month, count, month_prop_football) %>% 
  mutate(diff = count - month_prop_football)

(eng_plot <- ggplot(eng_join, aes(x = birth_month, y = diff)) +
   geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
   annotate(geom = "text", label = "Unlike Spain and Italy, in England \nSeptember babies are oldest in their school year, \nand the RAE can subsequently be seen to be shifted.",
            x = 9, y = 22, size = 3, hjust = 0.5, family = "Lato", colour = "grey30") +
   annotate("curve", x = 9, xend = 9, y = 13, yend = 3,
            colour = "grey30", size = 0.4, arrow = arrow(length = unit(0.15, "cm")),
            curvature = 0.15) +
   geom_segment(aes(xend = birth_month,
                    yend = 0, colour = diff),
                size = 1.5) +
   scale_colour_gradient2(low = "#3F8987", high = "#B5482E", midpoint = 0,
                          limits = c(-22.5, 22.5)) +
   geom_point(aes(fill=diff), col = "grey30", size = 5, shape = 23) +
   scale_fill_gradient2(low = "#3F8987", high = "#B5482E", midpoint = 0,
                        limits = c(-22.5, 22.5)) +
   scale_x_continuous(breaks = seq(1,12,1),
                      labels = c("J", "F", "M", "A", "M", "J", "J", "A",
                                 "S", "O", "N", "D"),
                      expand = c(0.02,0)) +
   lims(y = c(-28, 28)) +
 labs(x = "Birth Month",
      y = NULL,
      title = "English players in the Premier League") +
   theme(panel.grid = element_blank(),
         panel.grid.major.y = element_line(colour = "grey85", linetype = "dashed"),
         plot.background = element_rect(fill = "#f7f4e4", colour = "#f7f4e4"),
         panel.background = element_rect(fill = "#f7f4e4", colour = "#f7f4e4"),
         axis.text = element_text(size = 10, colour = "grey50", family = "Lato"),
         axis.ticks = element_blank(),
         legend.position = "none",
         plot.title = element_text(family = "DM Serif Text", size = 15),
         axis.title.x = element_text(margin = margin(5,0,0,0), colour = "grey50", family = "Lato"))
)

# La Liga -----------------------------------------------------------------

spain_league_url_2012 <- tm_league_team_urls("Spain", 2012, league_url = NA)


spain_players_2012 =  list()

for(i in 1:length(spain_league_url_2012)) {
  spain_players_2012[[i]] <- tm_team_player_urls(spain_league_url_2012[[i]])
}

spain_data_2012 <- unlist(spain_players_2012)

spain_2012 <- tm_player_bio(spain_data_2012) %>% 
  drop_na(date_of_birth)

spain_2012$date_of_birth <- lubridate::ymd(spain_2012$date_of_birth)
spain_2012$birth_month <- lubridate::month(spain_2012$date_of_birth)

la_liga_spain_2012 <- spain_2012 %>% 
  filter(citizenship %in% c("Spain"))


spainbirth_tidy <- spainbirth %>% 
  filter(Reliability != "Provisional figure",
         Month != "Total",
         Year >= 1996) %>% 
  select(Year, Month, Value) %>% 
  mutate(date = as.Date(paste0("01-", Month, "-", Year), format = "%d-%B-%Y"),
         month3 = lubridate::month(date)) %>% 
  group_by(month3) %>% 
  summarise(mean = mean(Value)) %>% 
  ungroup() %>% 
  drop_na() %>% 
  mutate(total = sum(mean),
         month_prop = mean/total,
         month_prop_football = month_prop*nrow(la_liga_spain_2012))


la_liga_spain_2012_df <- la_liga_spain_2012 %>% 
  group_by(birth_month) %>% 
  summarise(count = n()) %>% 
  ungroup()

spain_join <- full_join(la_liga_spain_2012_df, spainbirth_tidy, by = c("birth_month" = "month3")) %>% 
  select(birth_month, count, month_prop_football) %>% 
  mutate(diff = count - month_prop_football)

(spain_plot <- ggplot(spain_join, aes(x = birth_month, y = diff)) +
   geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
   geom_segment(aes(xend = birth_month,
                    yend = 0, colour = diff),
                size = 1.5) +
   scale_colour_gradient2(low = "#3F8987", high = "#B5482E", midpoint = 0,
                          limits = c(-22.5, 22.5)) +
   geom_point(aes(fill=diff, col = diff), col = "grey30", size = 5, shape = 21) +
   scale_fill_gradient2(low = "#3F8987", high = "#B5482E", midpoint = 0,
                        limits = c(-22.5, 22.5)) +
   scale_x_continuous(breaks = seq(1,12,1),
                      labels = c("J", "F", "M", "A", "M", "J", "J", "A",
                                 "S", "O", "N", "D"),
                      expand = c(0.02,0)) +
   annotate(geom = "text", label = "Dark dotted line represents monthly baseline \n(i.e. if footballer birth data followed national monthly average)",
            x = 9.4, y = 10.5, size = 3, hjust = 0.5, family = "Lato", colour = "grey30") +
   annotate(geom = "text", label = "Positive values = more footballers born in the \nmonth than expected based on national birth rates",
            x = 1.1, y = 25, size = 3, hjust = 0, colour = "#B5482E", family = "Lato", fontface = "italic") +
   annotate(geom = "text", label = "Negative values = fewer footballers born in the \nmonth than expected based on national birth rates",
            x = 1.1, y = -15, size = 3, hjust = 0, colour = "#3F8987", family = "Lato", fontface = "italic") +
   annotate("curve", x = 9.5, xend = 9.5, y = 6, yend = 1, 
            colour = "grey30", size = 0.4, arrow = arrow(length = unit(0.15, "cm")),
            curvature = -0.15) +
   lims(y = c(-28, 28)) +
   labs(x = NULL,
        y = NULL,
        title = "Spanish players in La Liga") +
   theme(panel.grid = element_blank(),
         panel.grid.major.y = element_line(colour = "grey85", linetype = "dashed"),
         plot.background = element_rect(fill = "#f7f4e4", colour = "#f7f4e4"),
         panel.background = element_rect(fill = "#f7f4e4", colour = "#f7f4e4"),
         axis.text = element_text(size = 10, colour = "grey50", family = "Lato"),
         axis.ticks = element_blank(),
         legend.position = "none",
         plot.title = element_text(family = "DM Serif Text", size = 15),
         axis.text.x = element_blank())
)


# Serie A -----------------------------------------------------------------

italy_league_url_2012 <- tm_league_team_urls("Italy", 2012, league_url = NA)


italy_players_2012 =  list()

for(i in 1:length(italy_league_url_2012)) {
  italy_players_2012[[i]] <- tm_team_player_urls(italy_league_url_2012[[i]])
}

italy_data_2012 <- unlist(italy_players_2012)

italy_2012 <- tm_player_bio(italy_data_2012) %>% 
  drop_na(date_of_birth)

italy_2012$date_of_birth <- lubridate::ymd(italy_2012$date_of_birth)
italy_2012$birth_month <- lubridate::month(italy_2012$date_of_birth)

serie_a_italy_2012 <- italy_2012 %>% 
  filter(citizenship %in% c("Italy"))

italybirth_tidy <- italybirth %>% 
  filter(Reliability != "Provisional figure",
         Month != "Total",
         Year > 1995) %>% 
  select(Year, Month, Value) %>% 
  mutate(date = as.Date(paste0("01-", Month, "-", Year), format = "%d-%B-%Y"),
         month3 = lubridate::month(date)) %>% 
  group_by(month3) %>% 
  summarise(mean = mean(Value)) %>% 
  ungroup() %>% 
  drop_na() %>% 
  mutate(total = sum(mean),
         month_prop = mean/total,
         month_prop_football = month_prop*nrow(serie_a_italy_2012))

serie_a_italy_2012_df <- serie_a_italy_2012 %>% 
  group_by(birth_month) %>% 
  summarise(count = n()) %>% 
  ungroup()

italy_join <- full_join(serie_a_italy_2012_df, italybirth_tidy, by = c("birth_month" = "month3")) %>% 
  select(birth_month, count, month_prop_football) %>% 
  mutate(diff = count - month_prop_football)

(italy_plot <- ggplot(italy_join, aes(x = birth_month, y = diff)) +
   geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
   geom_segment(aes(xend = birth_month,
                    yend = 0, colour = diff),
                size = 1.5) +
   scale_colour_gradient2(low = "#3F8987", high = "#B5482E", midpoint = 0,
                          limits = c(-22.5, 22.5)) +
   geom_point(aes(fill=diff), col = "grey30", size = 5, shape = 22) +
   scale_fill_gradient2(low = "#3F8987", high = "#B5482E", midpoint = 0,
                        limits = c(-22.5, 22.5)) +
   scale_x_continuous(breaks = seq(1,12,1),
                      labels = c("J", "F", "M", "A", "M", "J", "J", "A",
                                 "S", "O", "N", "D"),
                      expand = c(0.02,0)) +
   annotate(geom = "text", label = "In Italy and Spain, children born at the \nbegining of the year are the oldest in their cohort, \nand subsequently there are more footballers \nborn in these months than expected.",
            x = 3, y = -14.8, size = 3, hjust = 0.5, family = "Lato", colour = "grey30") +
   lims(y = c(-28, 28)) +
   labs(x = NULL,
        y = NULL,
        title = "Italian players in Serie A") +
   coord_cartesian(clip = "off") +
   theme(panel.grid = element_blank(),
         panel.grid.major.y = element_line(colour = "grey85", linetype = "dashed"),
         plot.background = element_rect(fill = "#f7f4e4", colour = "#f7f4e4"),
         panel.background = element_rect(fill = "#f7f4e4", colour = "#f7f4e4"),
         axis.text = element_text(size = 10, colour = "grey50", family = "Lato"),
         axis.ticks = element_blank(),
         legend.position = "none",
         plot.title = element_text(family = "DM Serif Text", size = 15),
         axis.text.x = element_blank())
)

# bring together ----------------------------------------------------------

plot_grid(spain_plot, italy_plot, eng_plot, nrow = 3) +
  plot_annotation(title = 'Birthdate Influences Footballers Opportunity',
                  subtitle = "The **Relative Age Effect (RAE)** refers to the overrepresentation of players born in certain parts of the year than  \nexpected based on national birth distribution. This is often related to age groups at school. It is not rare for some  \nchildren to be almost 12 months older than others in their cohort, leading to an age advantage in athletic activities.  \n \nThe plots below show the difference in the number of home-grown professional footballers who played in the  \nPremier League (England), La Liga (Spain), and Serie A (Italy) in 2012 who are born in each month compared to the  \nnumber expected based on average monthly births in each country.",
                  caption = "@jamie_bio | source: Transfermarkt by way of {WorldFootballR}, ONS, and UNdata",
                  theme = theme(plot.title = element_text(size = 27, hjust = 0.5,
                                                          colour = "black", family = "DM Serif Text"),
                                plot.subtitle = element_markdown(family = "Lato", size = 11, 
                                                                 colour = "grey20",
                                                                 lineheight = 1.2),
                                plot.caption = element_text(family = "Lato", size = 7,
                                                            colour = "grey30"),
                                plot.background = element_rect(fill = "#f7f4e4", colour = "#f7f4e4"),
                                panel.background = element_rect(fill = "#f7f4e4", colour = "#f7f4e4"),))

ggsave(
  paste0("football_rae_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 8,
  height = 10
)



