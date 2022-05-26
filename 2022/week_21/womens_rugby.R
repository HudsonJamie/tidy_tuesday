# womens_rugby.R
# Jamie Hudson
# Created: 25 May 2022
# Edited: 25 May 2022
# Data: Women's rugby data via ScrumQueens by way of Jacquie Tran

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(patchwork)
library(showtext)
font_add_google("Playfair Display")
font_add_google("Poppins")
showtext_opts(dpi = 320)
showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2022-05-24')

sevens <- tuesdata$sevens

# wrangle data ------------------------------------------------------------

sevs <- sevens %>%
  mutate(winner = case_when(str_detect(winner, "New Zealand") ~ "New Zealand",
                            TRUE ~ winner))

winners <- sevs %>%
  group_by(winner) %>%
  count(winner) %>%
  rename(team = winner,
         total_wins = n) %>%
  ungroup()

losers <- sevs %>%
  group_by(loser) %>%
  count(loser) %>%
  rename(team = loser,
         total_loss = n) %>%
  ungroup()

winlos <- full_join(winners, losers, by = "team") %>%
  mutate(total_games = total_wins + total_loss)

winlos_top10 <- winlos %>%
  arrange(desc(total_games)) %>%
  mutate(team = factor(team)) %>%
  dplyr::slice_head(n = 10)


legend_df <- winlos %>%
  arrange(desc(total_games)) %>%
  mutate(team = factor(team)) %>%
  dplyr::slice_head(n = 10) %>%
  filter(team == "France")

# plot ------------------------------------------------------------

(
  goalposts <- ggplot(winlos_top10) +
    geom_segment(
      aes(yend = total_games),
      x = 0.65,
      xend = 0.65,
      y = 0,
      size = 1.5,
      colour = "white",
      lineend = "round"
    ) +
    geom_segment(
      aes(yend = total_games),
      x = 1.35,
      xend = 1.35,
      y = 0,
      size = 1.5,
      colour = "white",
      lineend = "round"
    ) +
    geom_segment(
      aes(y = total_loss, yend = total_loss),
      x = 0.65,
      xend = 1.35,
      size = 1.5,
      colour = "white",
      lineend = "round"
    ) +
    geom_text(
      mapping = aes(x = 0.67, y = total_loss + 30, label = team),
      size = 5,
      hjust = 0,
      family = "Playfair Display",
      colour = "white"
    ) +
    lims(x = c(0.6, 1.4),
         y = c(0, 530)) +
    theme_void() +
    facet_wrap( ~ factor(team, levels = unique(winlos_top10$team)), nrow = 2) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      plot.background = element_rect(fill = "#3B4737",
                                     colour = "#3B4737")
    )
)


(
  legend <- ggplot(legend_df) +
    geom_segment(
      aes(yend = total_games),
      x = 0.55,
      xend = 0.55,
      y = 0,
      size = 1,
      colour = "grey80",
      lineend = "round"
    ) +
    geom_segment(
      aes(yend = total_games),
      x = 1.45,
      xend = 1.45,
      y = 0,
      size = 1,
      colour = "grey80",
      lineend = "round"
    ) +
    geom_segment(
      aes(y = total_loss, yend = total_loss),
      x = 0.55,
      xend = 1.45,
      size = 1,
      colour = "grey80",
      lineend = "round"
    ) +
    geom_text(
      mapping = aes(x = 0.585, y = total_loss + 40),
      label = "Team",
      size = 5,
      hjust = 0,
      family = "Playfair Display",
      colour = "grey80"
    ) +
    geom_text(
      mapping = aes(x = 1.2, y = total_loss + 0.5 * total_wins),
      label = "Games  \nwon",
      colour = "grey60",
      size = 3,
      hjust = 0.5,
      angle = 90,
      family = "Poppins"
    ) +
    geom_text(
      mapping = aes(x = 0.8, y = total_loss / 2),
      label = "Games  \nlost",
      colour = "grey60",
      size = 3,
      hjust = 0.5,
      angle = 90,
      family = "Poppins"
    ) +
    geom_text(
      mapping = aes(x = 1.65, y = total_games / 2),
      label = "Games played",
      colour = "grey60",
      size = 3,
      hjust = 0.5,
      angle = 90,
      family = "Poppins"
    ) +
    geom_segment(
      aes(yend = total_games),
      x = 1.55,
      xend = 1.55,
      y = 0,
      size = 0.5,
      colour = "grey60",
      arrow = arrow(length = unit(0.15, "cm"),
                    ends = "both")
    ) +
    geom_segment(
      aes(yend = total_loss - 20),
      x = 0.65,
      xend = 0.65,
      y = 0,
      size = 0.4,
      colour = "grey60",
      arrow = arrow(length = unit(0.15, "cm"),
                    ends = "both")
    ) +
    geom_segment(
      aes(y = total_loss + 20, yend = total_games),
      x = 1.35,
      xend = 1.35,
      size = 0.4,
      colour = "grey60",
      arrow = arrow(length = unit(0.15, "cm"),
                    ends = "both")
    ) +
    lims(x = c(0.5, 1.65),
         y = c(0, 530)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "#3B4737",
                                         colour = "#3B4737"))
)

(
  final_plot <- goalposts +
    plot_annotation(
      title = "Women's Rugby Sevens",
      subtitle = "Performance of the 10 international teams that have played the most number of rugby sevens matches. \nGoal posts are arranged by total matches played, with lower crossbars represent teams that have a higher \nwin ratio. Matches that ended in a draw are excluded from the visualisation",
      caption = "@jamie_bio | source: ScrumQueens by way of Jacquie Tran",
      theme = theme(
        plot.title = element_text(
          size = 40,
          family = "Playfair Display",
          colour = "white",
          margin = margin(0, 0, 15, 0)
        ),
        plot.caption = element_text(
          family = "Poppins",
          colour = "white",
          size = 7
        ),
        plot.subtitle = element_text(
          family = "Poppins",
          size = 10,
          lineheight = 1.1,
          colour = "grey80"
        ),
        panel.background = element_rect(colour = "#3B4737",
                                        fill = "#3B4737"),
        plot.background = element_rect(colour = "#3B4737",
                                       fill = "#3B4737"),
        plot.margin = margin(35, 10, 0, 10)
      )
    ) +
    inset_element(
      legend,
      left = 0.82,
      bottom = 1,
      right = 0.97,
      top = 1.32,
      align_to = 'full',
      clip = F
    )
)

ggsave(
  paste0("womens_rugby_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 10,
  height = 7
)
