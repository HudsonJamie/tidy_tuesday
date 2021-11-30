# cricket_odi.R
# Jamie Hudson
# Created: 30 Nov 2021
# Edited: 30 Nov 2021
# Data: ESPN Cricinfo

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(png)
library(grid)
library(ggtext)
library(showtext)
font_add_google("Playfair Display", "playfair")
font_add_google("Lato", "Lato")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)


# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2021-11-30")
matches <- tuesdata$matches

# wrangle data ------------------------------------------------------------

matches <- matches %>% 
  mutate(match.date = as.Date(match_date, format = "%b %d, %Y"),
         match.date = case_when(is.na(match.date) ~ as.Date(match_date, format = "%b %d-%d, %Y"),
                                TRUE ~ match.date),
         margin_type = case_when(margin_type == "run" ~ "runs",
                                 margin_type == "wicket" ~ "wickets",
                                 TRUE ~ margin_type))
  
matches_df <- matches %>% 
  drop_na(match.date) %>% 
  group_by(month = floor_date(match.date, "month"),
           toss_decision) %>% 
  mutate(margin_type = case_when(margin_type == "runs" ~ "Batting First",
                                 margin_type == "wickets" ~ "Fielding First",
                                 str_detect(winner, "Match tied") ~ "Match Tied",
                                 str_detect(winner, "opposition conceded") ~ "Conceded",
                                 margin_type == "default" ~ "Conceded"),
         id = ifelse(toss_decision == "field first", row_number(), -row_number())) %>% 
  drop_na(margin_type) %>% 
  mutate(toss.decision = case_when(toss_decision == "field first" ~ "Fielding First",
                                   toss_decision == "bat first" ~ "Batting First"), 
         margin.type = case_when(margin_type == toss.decision ~ "Yes",
                                 str_detect(winner, "Match tied") ~ "Match Tied",
                                 str_detect(winner, "opposition conceded") ~ "Conceded",
                                 margin_type == "Conceded" ~ "Conceded",
                                 TRUE ~ "No"),
         id = ifelse(toss_decision == "field first", row_number(), -row_number()),
         margin.type = factor(margin.type, levels = c("Yes", "Match Tied", "No", "Conceded")))

# load bat and ball
bat <- readPNG("images/cricket_bat.png", T)
batr <- rasterGrob(bat, interpolate = TRUE)

ball <- readPNG("images/cricket_ball.png", T)
ballr <- rasterGrob(ball, interpolate = TRUE)

# plot ------------------------------------------------------------

ggplot(matches_df, aes(x = month, y = id, fill = margin.type)) +
  geom_point(shape = 21, size = 2.5, colour = "white", stroke = 0.1) +
  scale_x_date(labels = seq(1996, 2005, 1),
                   breaks = seq(from = as.Date("1996-01-01"), to = as.Date("2005-01-01"), by = "years")) +
  expand_limits(x = as.Date("1995-06-01"), y = -23) +
  labs(fill = "Did the team that won the toss \ngo on to win the match?",
       y = NULL,
       x = "Year",
       title = "Does winning the toss influence ODI results?",
       caption = "@jamie_bio | source = ESPN Cricinfo by way of Hassanasir",
       subtitle = "Between 1996 and 2005 there were 1,235 One Day International (ODI) cricket matches played that ended in a result. At the start of each match, there is a coin toss to determine  \nwhich team bats and fields first. \nAltogether, the number of matches won (<span style='color:#F2A551;'>**616**</span>) by the team that won the toss is similar to the number won (<span style='color:#BC1C21;'>**605**</span>) by those who lost the toss.  \nHowever, there seems to be a temporal trend as between 1996-1998, **45%** of matches were won by the team that won the toss, but by 2003-2005 this had risen to **60%**.") +
  annotate("text", x = as.Date("1995-06-15"), y = 2,
            label = "Decide to bat first", 
           family = "playfair", fontface = "italic", size = 3,
           colour = "grey50") +
  annotate("text", x = as.Date("1995-06-15"), y = -2,
            label = "Decide to field first",
           family = "playfair", fontface = "italic", size = 3,
           colour = "grey50") +
  annotate("text", x = as.Date("1997-07-01"), y = 18,
           label = "In 1996 Sri Lanka won the World Cup \nsemi-final by default after crowd trouble \ninterrupted the match.",
           size = 3, family = "Lato", fontface = "italic") +
  annotate("text", x = as.Date("2002-01-01"), y = 13,
           label = "In 2001 Pakistan were awarded \nthe match vs England after the \ncrowd accessed the pitch.",
           size = 3, family = "Lato", fontface = "italic") +
  annotate("curve", x = as.Date("1997-01-01"), xend = as.Date("1996-03-10"),
           y = 14.5, yend = 6.5, size = 0.6, arrow = arrow(length = unit(0.15, "cm")),
           curvature = 0.15, colour = "white") +
  annotate("curve", x = as.Date("1997-01-01"), xend = as.Date("1996-03-10"),
           y = 14.5, yend = 6.5, size = 0.2, arrow = arrow(length = unit(0.15, "cm")),
           curvature = 0.15) +
  annotate("curve", x = as.Date("2002-01-01"), xend = as.Date("2001-06-10"),
           y = 9.5, yend = 2.5, size = 0.6, arrow = arrow(length = unit(0.15, "cm")),
           curvature = -0.15, colour = "white") +
  annotate("curve", x = as.Date("2002-01-01"), xend = as.Date("2001-06-10"),
           y = 9.5, yend = 2.5, size = 0.2, arrow = arrow(length = unit(0.15, "cm")),
           curvature = -0.15) +
  annotation_custom(batr, xmin = as.Date("1995-05-20"), xmax = as.Date("1995-09-01"), ymin = 2, ymax = 12) +
  annotation_custom(ballr, xmin = as.Date("1995-05-20"), xmax = as.Date("1995-09-01"), ymin = -2, ymax = -8) +
  annotate("segment", x = as.Date("1995-06-01"), xend = as.Date("2005-12-31"),
           y = 0, yend = 0, size = 0.5, linetype = "dotted", colour = "grey50") +
  scale_fill_manual(values = c("#F2A551", "grey", "#BC1C21", "black")) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5,
                             nrow = 2))  +
  theme(panel.border = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.title = element_text(size = 30, family = "playfair", hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_markdown(size = 10, family = "Lato", hjust = 0.5,
                                         lineheight = 1.1, face = "italic", colour = "grey30"),
        plot.caption = element_text(size = 7, family = "playfair",
                                    colour = "grey80"),
        legend.position = c(0.85, 0.13),
        legend.direction = "horizontal",
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(family = "Lato", size = 7.5,
                                   face = "italic"),
        legend.title = element_text(family = "Lato", size = 9.5,
                                    face = "italic"),
        axis.text.x = element_text(family = "playfair", colour = "grey50"),
        axis.title.x = element_text(family = "playfair", colour = "grey50"),
        axis.ticks.x = element_line(colour = "grey80"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# save plot
ggsave(paste0("cricket_odi_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 12.2,
       height = 6.2)
