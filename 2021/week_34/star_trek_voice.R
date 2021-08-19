# star_trek_voice.R
# Jamie Hudson
# Created: 20 Aug 2021
# Edited: 20 Aug 2021
# Data: SpeechInteraction.org

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(ggbump)
library(ggforce)
library(patchwork)
library(showtext)
library(g)
font_add_google("Rambla", "rambla")
showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 34)
computer <- tuesdata$computer

# wrangle data ------------------------------------------------------------

top_char <- computer %>% 
  count(char) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 7)

top_data <- computer %>% 
  filter(char %in% top_char$char)

# plot ------------------------------------------------------------

df <- top_data %>% filter(char != "Computer Voice") %>% 
  mutate(type = case_when(type == "question" ~ "Question",
                          type == "command" ~ "Command",
                          TRUE ~ type)) %>% 
  group_by(char) %>% 
  count(type) %>% 
  ungroup() %>% 
  add_row(char = "Picard", type = "Comment", n = 0) %>% # Manually add Picard data, as otherwise geom_sigmoid doesn't include all seven types.
  group_by(char) %>% 
  mutate(type = fct_reorder(type, n),
         sum_n = sum(n))


df_2 <- df %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(area = (sum_n/320)*15,
         circ = sqrt(area/pi)*2,
         y = 93.5 + circ) # assuming scale_size_area(max_size) = area of largest value

(plot <- df %>% 
    ggplot(aes(x = type, y = n)) +
    geom_segment(mapping = aes(x = type, xend = type, y = 0, yend = n),
                 color = "grey90", linetype = "dotted", size = 0.2) +
    geom_sigmoid(aes(x = type, y = -3, xend = 4, yend = -18, group = factor(type)),
                 direction = "y", color = "grey90", smooth = 5,
                 size = 0.2) + 
    geom_text(aes(x = 4, y = -22, label = char),
              colour = "grey90", family = "rambla", size = 3.5) +
    geom_text(aes(y = n + 2.5, label = n),
              colour = "grey90", size = 2.5, family = "rambla") +
    geom_point(df_2, mapping = aes(x = 2.5, y = 90, size = sum_n), 
               colour = c("#81A7A8", "#9F9DA6", "#C1C730", "#85110f", "#D6A444", "#2B53A6"), alpha = 0.5, stroke = 0) +
    geom_text(df_2, mapping = aes(x = 2.5, y = y, label = sum_n),
              colour = "grey90", size = 2.5, family = "rambla") +
    scale_size_area(max_size = 15) +
    theme_dark() +
    facet_wrap(~fct_reorder(char, -sum_n), nrow = 1) +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "#02071D", colour = "#02071D"),
          plot.background = element_rect(fill = "#02071D", colour = "#02071D"),
          strip.text = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          # legend.background = element_rect(fill = "#02071D", colour = "#02071D"),
          # legend.key = element_rect(fill = NA, colour = NA),
          text = element_text(colour = "white", family = "rambla"))
)

# Legend plots

legend_df <- tribble(
  ~ type, ~ n,
  "Password", 8,
  "Comment", 16,
  "Conversation", 24,
  "Statement", 32,
  "Question", 40,
  "Wake Word", 48,
  "Command", 56
) %>% 
  mutate(type = fct_reorder(type, n))

(legend_plot <- legend_df %>% 
    ggplot(aes(x = as.numeric(type), y = n)) +
    geom_segment(mapping = aes(x = as.numeric(type), xend = as.numeric(type), y = 0, yend = 130),
                 color = "grey90", linetype = "dotted", size = 0.2) +
    geom_sigmoid(aes(x = as.numeric(type), y = -10, xend = 4, yend = -77, group = factor(type)),
                 direction = "y", color = "grey90", smooth = 5,
                 size = 0.2) + 
    geom_richtext(aes(y = n + 1, label = type), hjust = 0,
                  colour = "grey90", size = 2, angle = 90,
                  fill = "#02071D", label.colour = NA, family = "rambla") +
    geom_segment(mapping = aes(x = as.numeric(type)[1], xend = 9, y = 130, yend = 130),
                 color = "grey90", linetype = "dashed", size = 0.2) +
    geom_text(label = "Type of \ndialogue", aes(x = 9, y = 130),
              colour = "grey90", size = 2, hjust = 0, family = "rambla") +
    geom_segment(mapping = aes(x = 7.5, xend = 8.1, y = 85, yend = 85),
                 color = "grey90", linetype = "dashed", size = 0.2) +
    geom_segment(mapping = aes(x = 7.5, xend = 8.1, y = 0, yend = 0),
                 color = "grey90", linetype = "dashed", size = 0.2) +
    geom_segment(mapping = aes(x = 8.1, xend = 8.1, y = 0, yend = 85),
                 color = "grey90", linetype = "dashed", size = 0.2) +
    geom_segment(mapping = aes(x = 8.1, xend = 9, y = 39, yend = 39),
                 color = "grey90", linetype = "dashed", size = 0.2) +
    geom_text(label = "Length represents \nnumber of lines", aes(x = 9, y = 39),
              colour = "grey90", size = 2, hjust = 0, family = "rambla") +
    geom_segment(mapping = aes(x = 0, xend = 13, y = -130, yend = -130),
                 color = "grey90", size = 0.2) +
    lims(x = c(0, 13),
         y = c(-250, 130)) +
    coord_cartesian(clip = 'off') +
    theme_dark() +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "#02071D", colour = "#02071D"),
          plot.background = element_rect(fill = "#02071D", colour = "#02071D"),
          strip.text = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          # legend.background = element_rect(fill = "#02071D", colour = "#02071D"),
          # legend.key = element_rect(fill = NA, colour = NA),
          text = element_text(colour = "white"))
)

(circle_legend <- ggplot() +
    geom_circle(aes(x0 = 1, y0 = 1, r = 2.5),
                fill = "#C1C730", colour = NA,
                alpha = 0.5) +
    coord_fixed() +
    geom_text(label = "300", aes(x = 1, y = 4.6),
              colour = "grey90", vjust = 1, size = 2.2, family = "rambla") +
    geom_text(label = "Total number \nof lines \nspoken to the \nEnterprise computer", aes(x = -10, y = 4.6),
              colour = "grey90", vjust = 1, size = 2.2, family = "rambla") +
    geom_segment(mapping = aes(x = -6.5, xend = -0.5, y = 4.2, yend = 4.2),
                 color = "grey90", linetype = "dashed", size = 0.4) +
    lims(x = c(-14, 10),
         y = c(-2, 10)) +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          plot.background = element_rect(fill = NA, colour = NA),
          strip.text = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          text = element_text(family = "rambla", colour = "white"))
)

layout <- c(
  area(t = 3, l = 1, b = 7, r = 3
  ),
  area(t = 6, l = 1, b = 7, r = 3
  ),
  area(t = 1, l = 4, b = 8, r = 12
  ))

# plot(layout)

legend_plot + circle_legend + plot +
  plot_layout(design = layout) +
  plot_annotation(title = 'Beam me up, Alexa?',
                  subtitle = "The number of different voice interactions between the Starship Enterprise's computer and characters from Star Trek: The Next Generation.  \nThe six non-computer characters with the most interactions were <span style = 'color:#C1C730;'>Geordie</span>, <span style = 'color:#85110f;'>Picard</span>, <span style = 'color:#9F9DA6;'>Data</span>, <span style = 'color:#D6A444;'>Riker</span>, <span style = 'color:#81A7A8;'>Beverly</span>, and <span style = 'color:#2B53A6;'>Troi</span>.",
                  caption = "@jamie_bio | source:  SpeechInteraction.org",
                  theme = theme(panel.background = element_rect(fill = "#02071D"),
                                plot.background = element_rect(fill = "#02071D", colour = "#02071D"),
                                plot.title = element_text(size = 28,
                                                          colour = "white", family = "rambla",
                                                          face = "bold"),
                                plot.subtitle = element_markdown(family = "rambla", size = 10, 
                                                                 colour = "white",
                                                                 lineheight = 1.2),
                                plot.caption = element_text(family = "rambla", size = 6,
                                                            colour = "white")))

ggsave(paste0("star_trek_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 10,
       height = 6)

