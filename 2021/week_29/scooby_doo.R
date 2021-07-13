# scooby_doo.R
# Jamie Hudson
# Created: 13 Jul 2021
# Edited: 13 Jul 2021
# Data: From Kaggle and Scoobypedia


# Load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggtext)
library(patchwork)

font_add_google("Mystery Quest", "mystery")
showtext_auto()

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 29)
scoobydoo <- tuesdata$scoobydoo


# Wrangle data ------------------------------------------------------------

# Velma

rect <- data.frame(xmin=5000, xmax=10000, ymin=-Inf, ymax=Inf)

velma_df <- scoobydoo %>% 
  mutate(jinkies = as.numeric(jinkies),
         jinkies_pm = jinkies/as.numeric(run_time),
         velma_find = case_when(caught_velma == "TRUE" ~ "Yes",
                                TRUE ~ "No")) 

velma_df_caught <- velma_df %>% 
  filter(velma_find == "Yes")

velma_df_series <- velma_df %>% 
  filter(series_name == "A Pup Named Scooby-Doo")

(jinkies_plot <- 
  ggplot(velma_df, aes(x = date_aired, y = jinkies_pm, colour = velma_find)) +
  annotate(geom = "rect", xmin=min(velma_df_series$date_aired) - 500, xmax=max(velma_df_series$date_aired) + 500, ymin = -0.1, ymax=max(as.numeric(velma_df_series$jinkies_pm), na.rm = T) + 0.1,
             alpha = 0.2, fill="#F9981C") +
  geom_point(size = 3, alpha = 0.5, colour = "grey90") +
  geom_point(data = velma_df_caught, size = 4, colour = "#F9981C") +
  geom_curve(aes(x = max(velma_df_series$date_aired) + 2000, y = 0.5, xend = max(velma_df_series$date_aired) + 600, yend = 0.45),
             arrow = arrow(length = unit(0.1, "inch")), size = 0.2, curvature = -0.2, color = "white"
  ) +
  annotate("text", x = max(velma_df_series$date_aired) + 5700, y = 0.52, 
           label = "The series ' A Pup Named Scooby-Doo ' \nwas the Golden age of Jinkies'",
           family = "mystery", colour = "white", size = 5.5) +
    annotate("text", x = as.Date("1980-01-01"), y = 0.52, 
             label = "Velma caught the villain",
             family = "mystery", colour = "#F9981C", size = 4.5) +
    geom_curve(aes(x = as.Date("1980-01-01"), y = 0.5, xend = as.Date("1980-01-01") + 2800, yend = 0.48),
               arrow = arrow(length = unit(0.1, "inch")), size = 0.2, curvature = 0.1, color = "#F9981C"
    ) +
    annotate("text", x = as.Date("1977-01-01"), y = 0.32, 
             label = "Velma did not catch the villain",
             family = "mystery", colour = "grey80", size = 4.5) +
    geom_curve(aes(x = as.Date("1977-01-01"), y = 0.3, xend = as.Date("1979-06-01"), yend = 0.25),
               arrow = arrow(length = unit(0.1, "inch")), size = 0.2, curvature = 0.1, color = "white"
    ) +
  labs(title = "Jinkies per minute peaked in the last 1980s and early 1990s",
       colour = "Did Velma catch the villain?") +
  xlab("Date of episode") +
  ylab("Jinkies per minute") +
  coord_cartesian(ylim=c(0, 0.6)) +
  theme(plot.title = element_text(size = 23,
                                  colour = "#F9981C"),
        text = element_text(family = "mystery"),
        axis.text = element_text(colour = "grey80"),
        axis.title = element_text(colour = "white", size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA),
        plot.background = element_rect(fill = "grey10", colour = "grey10"),
        panel.background = element_rect(fill = "grey10")
        ))

# Shaggy

zoinks <- scoobydoo %>% 
  mutate(zoinks = as.numeric(zoinks),
         zoinks_pm = zoinks/as.numeric(run_time),
         shaggy_find = case_when(caught_shaggy == "TRUE" ~ "Yes",
                                 TRUE ~ "No")) 

zoinks_shaggy_caught <- zoinks %>% 
  filter(shaggy_find == "Yes")

max_zoinks <- zoinks %>% 
  arrange(desc(zoinks_pm)) %>% 
  slice_head()

(zoinks_plot <- 
  ggplot(zoinks, aes(x = date_aired, y = zoinks_pm, colour = shaggy_find)) +
  geom_point(size = 3, alpha = 0.5, colour = "grey90") +
  geom_point(data = zoinks_shaggy_caught, size = 4, colour = "#B8BE18") +
  geom_vline(xintercept = as.Date("1993-06-01"), linetype = "dashed", colour = "grey70") +
  annotate("text", x = as.Date("1980-01-01"), y = 0.96, label = "CBS + ABC network era",
           family = "mystery", colour = "white", size = 5.5) +
  annotate("text", x = as.Date("2010-01-01"), y = 0.95, label = "Post CBS + ABC era",
           family = "mystery", colour = "white", size = 5.5) +
    annotate("text", x = as.Date("2001-01-01"), y = 0.4, 
             label = "Shaggy caught the villain",
             family = "mystery", colour = "#B8BE18", size = 4.5) +
    geom_curve(aes(x = as.Date("2001-01-01"), y = 0.36, xend = as.Date("1992-06-01"), yend = 0.25),
               arrow = arrow(length = unit(0.1, "inch")), size = 0.2, curvature = -0.1, color = "#B8BE18"
    ) +
    annotate("text", x = as.Date("1977-01-01"), y = 0.74, 
             label = "Shaggy did not catch the villain",
             family = "mystery", colour = "grey80", size = 4.5) +
    geom_curve(aes(x = as.Date("1977-01-01"), y = 0.71, xend = as.Date("1980-06-01"), yend = 0.65),
               arrow = arrow(length = unit(0.1, "inch")), size = 0.2, curvature = 0.1, color = "white"
    ) +
  labs(title = "Zoinks per minute decreased once Scooby Doo moved from ABC",
       colour = "Did Shaggy catch the villain?") +
  xlab("Date of episode") +
  ylab("Zoinks per minute") +
  coord_cartesian(ylim=c(0, 0.96)) +
  theme(plot.title = element_text(size = 23,
                                  colour = "#B8BE18"),
        text = element_text(family = "mystery"),
        axis.text = element_text(colour = "grey80"),
        axis.title = element_text(colour = "white", size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA),
        plot.background = element_rect(fill = "grey10", colour = "grey10"),
        panel.background = element_rect(fill = "grey10")
  ))


# Combine plots -----------------------------------------------------------

jinkies_plot / zoinks_plot +
  plot_annotation(title = "Scooby Doo and the Ree-hee-heely useless metrics",
                  caption = "@jamie_bio | source = Kaggle thanks to plummye",
                  theme = theme(plot.caption.position = 'plot',
                                plot.caption = element_text(family = "mystery",
                                                            colour = "#9A8CBF",
                                                            size = 12),
                                plot.title = element_text(family = "mystery",
                                                          colour = "#9A8CBF",
                                                          size = 35),
                                plot.background = element_rect(fill = "grey10"),
                                panel.background = element_rect(fill = "grey10")
                  ))

ggsave(
  paste0("scooby_doo_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 11,
  height = 12
)
