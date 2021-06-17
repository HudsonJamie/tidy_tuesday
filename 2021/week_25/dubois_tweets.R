# dubois_tweets.R
# Jamie Hudson
# Created: 17 June 2021
# Edited: 17 June 2021
# Data: From Anthony Starks, Allen Hillery, and Sekou Tyler

library(tidyverse)
library(tidytuesdayR)
library(maps)
library(ggimage)
library(gt)
library(patchwork)
library(png)
library(ggtext)
library(showtext)
library(webshot2)
library(lubridate)

font_add_google("Lato", "lato")
showtext_auto()

# load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 25)
tweets <- tuesdata$tweets

# map ---------------------------------------------------------------------


world <- map_data("world") %>% 
  filter(region != "Antarctica")

(worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group),
               fill = "#E6E6E6", colour = "#B7B7B7") + 
  geom_point(data = tweets, aes(x = long, y = lat), shape = 21,
             fill = "#DC163C", colour = "white", size = 3,
             stroke = 0.5) +
    labs(title = "Twitter Activity") +
  coord_fixed(1.3) +
    theme(axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          panel.background = element_rect(fill = "#C1D1D2"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.background = element_rect(fill = "#F9F0E6", colour = "black"),
          plot.margin = margin(10,0,0,0, unit = "pt")))


# tweets_bar --------------------------------------------------------------

tweets_bar_data <- tweets %>% 
  group_by(username) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  arrange(desc(followers)) %>% 
  select(username, followers, verified) %>% 
  slice_head(n = 10)

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

img <- "logo.png"

(tweets_bar <- ggplot(tweets_bar_data) +
  geom_col(aes(y = reorder(paste("@",username), followers), x = followers),
           fill = "#DC163C", width = 0.7) +
  geom_text(aes(y = paste("@",username), x = 700, label= scales::comma(followers)), color="black", size=3.5,
            hjust = 0) +
    geom_text(aes(y = paste("@",username), x = -34000, label= paste("@",username), fontface=2), color="black", size=3.5,
              hjust = 0) +
    labs(title = "Top 10 Users with the Highest Follower Count",
         subtitle = "<span style = 'color:#1CC8FB;'>*</span> Verified Users") +
    geom_image(data = . %>% filter(verified == TRUE), aes(y = reorder(paste("@",username), followers), x = followers + 4500),
               image=img, size=.05, asp = 2.1) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_markdown(hjust = 0.5, family = "lato"),
          panel.background = element_rect(fill = "#F9F0E6"),
          plot.background = element_rect(fill = "#F9F0E6", colour = "black"),
          axis.text.x = element_text(colour = "#F9F0E6"),
          plot.margin = margin(10,0,0,0, unit = "pt")))

# tweets_line --------------------------------------------------------------

(tweets_line <- tweets %>% mutate(date = as.Date(datetime)) %>% 
  group_by(date) %>% 
  count() %>%
  ggplot() +
    geom_line(aes(x = date, y = n), size = 0.9, colour = "#DC163C") +
    scale_x_date(breaks = as.Date(c("2021-02-02", "2021-02-16",
                                    "2021-03-02", "2021-03-16", "2021-03-30",
                                    "2021-04-13", "2021-04-27", "2021-05-11")),
                 date_labels = "%d %b",
                 limits = as.Date(c("2021-02-02","2021-05-11"))) +
  labs(title = "Tweets Over Time") +
    theme(axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.background = element_rect(fill = "#F9F0E6"),
          plot.background = element_rect(fill = "#F9F0E6", colour = "black"),
          plot.margin = margin(10,0,0,0, unit = "pt")))


# table -------------------------------------------------------------------

tweets_tbl <- tweets %>% 
  select(datetime, username, content, like_count, retweet_count, quote_count) %>% 
  rename(Datetime = datetime,
         "Twitter Handle" = username,
         Tweet = content,
         "Like Count" = like_count,
         "Retweet Count" = retweet_count,
         "Quote Count" = quote_count) %>% 
  arrange(Datetime) %>% 
  mutate(Datetime = format(Datetime, "%d/%m/%Y %H:%M:%S")) %>% 
  slice_head(n = 10)

abbrev_text <- function(x) {
  paste0(
    "<div style=\"display:table;table-layout:fixed;width:100%;\">",
    "<div title=\"", x , "\", ", # `<p>` has been changed to `<div>` here
    "style=\"overflow-x:hidden;text-overflow:ellipsis;white-space:nowrap\">",
    x,
    "</div>",
    "</div>"
  )
}

(twt_tbl <- tweets_tbl %>% 
  gt() %>% 
  tab_options(
    table.background.color = "#F9F0E6",
    data_row.padding = px(0.7),
    column_labels.font.weight = 'bold',
    table.width = px(1170),
    table.font.names = "lato",
    table.font.weight = "lighter") %>% 
  cols_width(
    ends_with("Count") ~ px(150),
    ends_with("eet") ~ px(850),
    starts_with("Date") ~ px(200),
    starts_with("Twit") ~ px(150)
  ) %>% 
    text_transform(locations = cells_body(columns = Tweet),
                   fn = abbrev_text)
)

# Issue with gtsave/webshot2 keeping headings as bold. Workout is save as html and convert to png (https://github.com/rstudio/gt/issues/621)
gtsave(twt_tbl, 
       filename = "tweet_table.html")

webshot2::webshot("tweet_table.html", "tweet_table.png",
                  vwidth = 1700, vheight = 4000, zoom = 5, expand = c(-10, -25, -10, -25))

# total tweets ------------------------------------------------------------

ttl_tweets <- nrow(tweets)

(text1 <- ggplot() +
  annotate(
    "richtext", x = 0.5, y = 0.5, size = 7,
    colour = "black", family = "lato",
    label = glue::glue(
      "<b>Total Tweets</b>  \n{ttl_tweets}"
    ),
    label.colour = NA,
    fill =  "#F9F0E6") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "#F9F0E6"),
        plot.background = element_rect(fill = "#F9F0E6", colour = "black")))


# unique users ------------------------------------------------------------


unique_users <- tweets %>% 
  distinct(username) %>% 
  count()

(text2 <- ggplot() +
  annotate(
    "richtext",
    x = 0.5,
    y = 0.5,
    size = 7,
    colour = "black",
    family = "lato",
    label = glue::glue(
      "<b>Total Unique Users</b>  \n{unique_users$n}"
    ),
    label.colour = NA,
    fill =  "#F9F0E6") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "#F9F0E6"),
        plot.background = element_rect(fill = "#F9F0E6", colour = "black")))


# date common -------------------------------------------------------------


(date_common <- tweets %>% mutate(date = as.Date(datetime)) %>% 
  group_by(date) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  slice_head(n = 1) %>% 
  mutate(date = format(date, "%B %dth")))

text3 <- ggplot() +
  annotate(
    "richtext",
    x = 0.5,
    y = 0.5,
    size = 7,
    colour = "black",
    family = "lato",
    label = glue::glue(
      "<b>Date with the most activity</b>  \n{date_common$date} ({date_common$n} Tweets)"
    ),
    label.colour = NA,
    fill =  "#F9F0E6") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "#F9F0E6"),
        plot.background = element_rect(fill = "#F9F0E6", colour = "black"))


# spacer with correct colour ----------------------------------------------


(space <- ggplot() +
  theme(panel.background = element_rect(fill = "#F9F0E6", colour = "#F9F0E6"),
        plot.background = element_rect(fill = "#F9F0E6", colour = "#F9F0E6")))


# bring together ----------------------------------------------------------


layout <- '
AAABBBCCC
DDDEEEFFF
DDDEEEFFF
GGGGGGGGG
GGGGGGGGG
'
(plot <- wrap_plots(A = text1, B = text2, C = text3,
           D = worldplot, E = tweets_bar, F = tweets_line, G = space, design = layout))

tbl_img <- readPNG("./tweet_table.png", native = T)

plot + inset_element(tbl_img,
                  left = 0, bottom = -0.1,
                  right = 1, top = 1.1, align_to = "full") + theme_void() + 
  plot_annotation(title = "#DuBoisChallenge Twitter Usage",
                  subtitle = "This is a static recreation of the dashboard created by Sekou Tyler in Tableau as part of the #DuBoisChallenge. \nThe goal of the challenge is to celebrate the data visualization legacy of W.E.B. DuBois by recreating the visualizations from the 1900 Paris Exposition using modern tools. \nThe dashboard takes a look at specific twitter usage of the challenge.",
                  caption = "@jamie_bio | source = Anthony Starks, Allen Hillery & Sekou Tyler",
                  theme = theme(plot.caption.position = 'plot',
                                plot.caption = element_text(),
                                plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
                                plot.subtitle = element_text(size = 9, hjust = 0.5,
                                                             lineheight = 1.1),
                                plot.background = element_rect(fill = "#F9F0E6"),
                                panel.background = element_rect(fill = "#F9F0E6")
                  )) &
  theme(text = element_text(family = "lato"))

ggsave(paste0("dubois_twitter_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 180,
       width = 13.5,
       height = 7)
