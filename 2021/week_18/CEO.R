# CEO.R
# Jamie Hudson
# Created: 27 Apr 2021
# Edited: 27 Apr 2021
# Data: Gentry et al. 2021 (https://onlinelibrary.wiley.com/doi/full/10.1002/smj.3278) and DataIsPlural https://www.data-is-plural.com/archive/2021-04-21-edition/

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(ggthemes)
library(ggtext)
library(glue)

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 18)
departures <- tuesdata$departures

# Prepare data ------------------------------------------------------------

data <- departures %>%
  filter(fyear_gone > 1990,
         fyear_gone < 2020) %>%
  mutate(
    reason = case_when(
      departure_code == 3 ~ "performance",
      departure_code == 4 ~ "dismissed",
      departure_code == 5 ~ "retired"
    )
  ) %>%
  drop_na(reason) %>%
  group_by(fyear_gone, reason) %>%
  count(fyear_gone, reason)


perf <- as.data.frame(data) %>%
  filter(reason == "performance") %>%
  slice(which.max(n))

arrow_2008 <-
  tibble(x1 = 2008,
         x2 = 2010.5,
         y1 = 78.5,
         y2 = 90)


# Plot data ---------------------------------------------------------------

ggplot(data, aes(x = fyear_gone, y = n, colour = reason)) +
  geom_line(aes(group = fyear_gone), colour = "grey") +
  geom_point(size = 3.5, alpha = 0.8) +
  coord_flip() +
  labs(
    title = "CEO departures from S&P 1500 firms",
    subtitle = "The number of CEOs that departed because they were
    dismissed for  \n<span style = 'color:#C72E28;'><b>legal violations</b></span>,
    <span style = 'color:#1F6392;'><b>job performance</b></span>, or simply
    <span style = 'color:#BE9C2C;'><b>retired</b></span> between 1990-2020.",
    caption = "@jamie_bio | source: Gentry et al. (2021) & DataIsPlural"
  ) +
  annotate(
    "richtext",
    x = 2013,
    y = 90,
    size = 3.5,
    colour = "black",
    family = "mono",
    label = glue::glue(
      "<b>{perf$fyear_gone}</b>, the year of the Global  \nFinancial Crisis had the most  \nnumber of CEOs dismissed  \ndue to job performance: <b>{perf$n}<b>"
    ),
    label.colour = NA,
    fill =  after_scale(alpha("brown", 0.15))
  ) +
  geom_curve(
    data = arrow_2008,
    aes(
      x = x1,
      y = y1,
      xend = x2,
      yend = y2
    ),
    arrow = arrow(length = unit(0.1, "inch")),
    size = 0.5,
    curvature = +0.2,
    color = "black"
  ) +
  scale_colour_wsj() +
  theme_wsj() +
  theme(
    plot.title = element_markdown(size = 30),
    plot.subtitle = element_markdown(size = 15),
    plot.caption = element_markdown(size = 10),
    legend.position = "none"
  )

ggsave(
  paste0("CEO_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 10,
  height = 7,
  type = "cairo-png"
)
