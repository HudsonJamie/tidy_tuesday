library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(janitor)
library(ggtext)
library(patchwork)
library(glue)

tuesdata <- tidytuesdayR::tt_load(2021, week = 17)

netflix <- tuesdata$netflix_titles

tv <- netflix %>% 
  mutate(date_added = mdy(netflix$date_added)) %>% 
  filter(type == "TV Show")

## Popular on netflix

ratings <- read_csv("data/tv_shows.csv") %>% 
  clean_names() %>% 
  select(title, im_db, rotten_tomatoes)

tv_ratings <- inner_join(tv, ratings)


# Big plot ----------------------------------------------------------------
min_imdb <- tv_ratings %>% 
  mutate(duration = str_extract(duration, "\\d")) %>% 
  mutate(rotten_tomatoes = parse_number(rotten_tomatoes)) %>%
  drop_na(im_db) %>% 
  slice(which.min(im_db))

max_imdb <- tv_ratings %>% 
  mutate(duration = str_extract(duration, "\\d")) %>% 
  mutate(rotten_tomatoes = parse_number(rotten_tomatoes)) %>%
  drop_na(im_db) %>% 
  slice(which.max(im_db))

GWH_arrow <-
  tibble(
    x1 = 2,
    x2 = 1.2,
    y1 = 1,
    y2 = 1.7
  )

BB_arrow <-
  tibble(
    x1 = 6.3,
    x2 = 5.5,
    y1 = 9.7,
    y2 = 9.55
  )

set.seed(2021)
(big_plot <- tv_ratings %>% 
  mutate(duration = str_extract(duration, "\\d")) %>% 
  mutate(rotten_tomatoes = parse_number(rotten_tomatoes)) %>%
  drop_na(rating) %>% 
ggplot() +
  geom_jitter(aes(x = duration, y = im_db, colour = date_added), size = 3, alpha = 0.85) +
  scale_colour_gradient(low = "pink", high = "#E30914", na.value = NA, trans = "date",
                        guide = guide_colorbar(title.position = "top")) +
    scale_y_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10)) +
  labs(x = "Number of seasons",
       y = "IMDB rating",
       colour = "Date series added") +
    annotate("richtext", x = 3.3, y = 1, family = "Kanit Light",
      size = 3.5, color = "white", lineheight = .9, label = glue::glue(
        "<span style='font-family:Kanit'>{min_imdb$title}</span> has the  \nlowest IMDB rating of <span style='font-family:Kanit'>{min_imdb$im_db}</span>"
      ),label.colour = NA, fill = NA
    ) +
    annotate("richtext", x = 7.5, y = 9.5, family = "Kanit Light",
             size = 3.5, color = "white", label = glue::glue(
               "<span style='font-family:Kanit'>{max_imdb$title}</span> has the  \nhighest IMDB rating of <span style='font-family:Kanit'>{max_imdb$im_db}</span>"
             ),label.colour = NA, fill = NA
    ) +
    geom_curve(
      data = BB_arrow,
      aes(
        x = x1,
        y = y1,
        xend = x2,
        yend = y2
      ),
      arrow = arrow(length = unit(0.07, "inch")),
      size = 0.5,
      curvature = +0.1,
      color = "white"
    ) +
    geom_curve(
      data = GWH_arrow,
      aes(
        x = x1,
        y = y1,
        xend = x2,
        yend = y2
      ),
      arrow = arrow(length = unit(0.07, "inch")),
      size = 0.5,
      curvature = -0.1,
      color = "white"
    ) +
  theme_minimal() +
  theme(text = element_text(family = "Kanit Light"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50",
                                          linetype="dashed"),
        axis.text = element_text(colour = "white", size = 12),
        axis.title = element_text(colour = "white", size = 14),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(colour = "white", size = 10),
        legend.title = element_text(colour = "white", size = 11),
        legend.title.align=0.5,
        legend.direction = "horizontal",
        legend.position = c(0.8, 0.25)
        ))


# Popular on netflix ------------------------------------------------------

top_genres <- tv_ratings %>%
  select(date_added, title, listed_in) %>%
  mutate(listed_in = strsplit(listed_in, ",")) %>%
  unnest(listed_in) %>% 
  mutate(listed_in = str_remove_all(listed_in, c("TV|Movies|Series|Features|Shows")),
         listed_in = str_trim(listed_in),
         listed_in = str_replace(listed_in, "Kids'", "Kids")) %>% 
  group_by(listed_in) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  top_n(n = 5)

top_genres$listed_in_f <- factor(top_genres$listed_in, levels=top_genres$listed_in)

(popular <- tv_ratings %>%
  mutate(listed_in = strsplit(listed_in, ",")) %>%
  unnest(listed_in) %>% 
  mutate(listed_in = str_remove_all(listed_in, c("TV|Movies|Series|Features|Shows")),
         listed_in = str_trim(listed_in),
         listed_in = str_replace(listed_in, "Kids'", "Kids")) %>% 
  filter(listed_in %in% top_genres$listed_in) %>% 
  mutate(listed_in_f = factor(listed_in, levels=top_genres$listed_in)) %>% 
  ggplot(aes(x = date_added)) +
  geom_histogram(fill="#E30914", binwidth = 365/4) +
    labs(title = "Popular on Netflix <span style='font-size:12pt'>(Genres with highest number of shows)</span>",
         y = "Number of \nshows") +
  facet_wrap(.~listed_in_f, nrow = 1) +
  scale_x_date(breaks = as.Date(c("2015-01-01", "2020-01-01")),
               labels = c("2015","2020")) +
  theme(text = element_text(family = "Kanit Light"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "grey80", colour = "grey80"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_markdown(colour = "white", family = "Kanit",
                                  size = 16),
        axis.text = element_text(colour = "white", size = 10),
        axis.title = element_text(colour = "white", size = 12),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "grey80", colour = "grey80")
  ))



# Recently watched --------------------------------------------------------

top_genres_RT <- tv_ratings %>%
  select(date_added, title, listed_in, rotten_tomatoes) %>%
  mutate(listed_in = strsplit(listed_in, ",")) %>%
  unnest(listed_in) %>% 
  mutate(listed_in = str_remove_all(listed_in, c("TV|Movies|Series|Features|Shows")),
         listed_in = str_trim(listed_in)) %>% 
  mutate(rotten_tomatoes = parse_number(rotten_tomatoes)) %>%
  drop_na(rotten_tomatoes) %>% 
  group_by(listed_in) %>% 
  summarise(ave = mean(rotten_tomatoes, na.rm = T), n = n()) %>% 
  arrange(desc(ave)) %>% 
  filter(n > 20) %>% 
  top_n(5)

top_genres_RT$listed_in_f <- factor(top_genres_RT$listed_in, levels=top_genres_RT$listed_in)

(recently_watched <- tv_ratings %>%
  mutate(listed_in = strsplit(listed_in, ",")) %>%
  unnest(listed_in) %>% 
  mutate(listed_in = str_remove_all(listed_in, c("TV|Movies|Series|Features|Shows")),
         listed_in = str_trim(listed_in)) %>% 
    mutate(rotten_tomatoes = parse_number(rotten_tomatoes)) %>%
    drop_na(rotten_tomatoes) %>% 
  filter(listed_in %in% top_genres_RT$listed_in) %>% 
  mutate(listed_in_f = factor(listed_in, levels=top_genres_RT$listed_in)) %>% 
  ggplot(aes(x = date_added, y = rotten_tomatoes)) +
  geom_point(colour="#E30914", size = 0.5) +
    scale_y_continuous(limits = c(0,110), breaks = c(0,50,100)) +
  labs(title = "Recently Watched <span style='font-size:12pt'>(Genres with highest average Rotten Tomatoes rating)</span>",
       y = "Rotten Tomatoes \nrating (%)") +
  facet_wrap(.~listed_in_f, nrow = 1) +
  scale_x_date(breaks = as.Date(c("2015-01-01", "2020-01-01")),
               labels = c("2015","2020")) +
  theme(text = element_text(family = "Kanit Light"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "grey80", colour = "grey80"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_markdown(colour = "white", family = "Kanit",
                                  size = 16),
        axis.text = element_text(colour = "white", size = 10),
        axis.title = element_text(colour = "white", size = 12),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "grey80", colour = "grey80")
  ))


# Title -------------------------------------------------------------------


(title <-
  tibble(
    x = c(-1, -1, 1.5, -1, -1), y = c(0.46, 0.32, 0.32, 0.23, 0.07),
    label = c("NETFLIX",
              "@jamie_bio",
              "| source: Shivam Bansal - Kaggle",
              "Week 17, 2021 Coming Tuesday",
              "Here I combined Netflix TV show data <br>
              with IMDB and Rotten Tomatoes ratings <br>to
              create a ggplot which mimics <br>the Netflix homepage"
  )) %>% 
  ggplot(aes(x, y, label = label)) +
  geom_textbox(
    width = unit(5, "inch"),
    family = c("Bebas Neue","Kanit Light","Kanit Light", "Kanit", "Kanit Light"),
    color = c("#E30914", "#5CC679","grey80", "white", "grey80"),
    size = c(30, 4, 4, 6, 5),
    fill = NA,
    box.colour = NA,
    hjust = 0.3
  ) +
  scale_x_continuous(limits = c(-5,5)) +
    scale_y_continuous(limits = c(0, 0.55)) +
    theme_bw() +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank()))


# Bring together ----------------------------------------------------------
layout <- c(
  area(t = 1, l = 1, b = 8, r = 4),
  area(t = 1, l = 5, b = 8, r = 10 ),
  area(t = 9, l = 1, b = 10, r = 10 ),
  area(t = 11, l = 1, b = 12, r = 10 ))

layout
plot(layout)

title + big_plot + popular + recently_watched +
  plot_layout(design = layout) & 
  theme(plot.background = element_rect(fill = "black", colour = "black")) 

ggsave(
  paste0("netflix", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 12,
  height = 8.36,
  type = "cairo-png"
)


