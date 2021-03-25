
# Load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(gggibbous)
library(patchwork)
library(ggtext)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 13)

unvotes <- tuesdata$unvotes
roll_calls <- tuesdata$roll_calls
issues <- tuesdata$issues


# Data prep ---------------------------------------------------------------

tt_un <- left_join(unvotes, issues, by = "rcid")

tt_un <- left_join(tt_un, roll_calls, by = "rcid")


tt_un %>%
  group_by(date, issue) %>%
  count(vote) %>%
  na.omit() %>%
  mutate(perc_yes = prop.table(n) * 100) %>%
  ggplot(aes(x = date, y = perc_yes, colour = vote)) +
  geom_line() +
  facet_wrap( ~ issue)

countries_2019 <- tt_un %>%
  filter(date > "2019-01-01")

countries_2019 <- unique(countries_2019$country)

# UK all votes ------------------------------------------------------------

UK_similarity_all <- tt_un %>%
  group_by(rcid) %>%
  filter(any(country == "United Kingdom")) %>%
  select(rcid, country_code, vote, country) %>%
  mutate(vote = as.numeric(as.factor(vote))) %>%
  na.omit() %>%
  filter(country %in% countries_2019) %>%
  group_by(rcid) %>%
  group_modify( ~ {
    .x %>% mutate(
      sim = vote - filter(., country_code == 'GB') %>% pull(vote),
      sim = ifelse(sim != 0, 0, 100)
    )
  }) %>%
  group_by(country) %>%
  summarise(mean = mean(sim))

UK_all_head <- UK_similarity_all %>%
  filter(country != "United Kingdom") %>%
  arrange(desc(mean)) %>%
  head(5)

UK_all_tail <- UK_similarity_all %>%
  filter(country != "United Kingdom") %>%
  arrange(desc(mean)) %>%
  tail(5)

UK_all <- rbind(UK_all_head, UK_all_tail)

# prepare circle coords

get_circle_coords <- function(r = 1, ...) {
  data_frame(
    theta = seq(0, 2 * pi, ...),
    x     = cos(theta) * r,
    y     = sin(theta) * r
  )
} ### Function taken from "Eric"'s blog https://eric.netlify.app/2017/08/07/drawing-an-annotated-unit-circle-with-ggplot2/#fn1

coords_pi <- get_circle_coords(by = pi / 5) %>% head(-1)

plot_cords <-
  as.data.frame(cbind(coords_pi$x, coords_pi$y, c(3, 2, 1, 10, 9, 8, 7, 6, 5, 4))) %>%
  rename(x = V1,
         y = V2,
         order = V3) %>%
  arrange(order)

(
  UK_all_plot <- UK_all %>%
    ggplot(aes(x = plot_cords$x, y = plot_cords$y)) +
    geom_point(colour = "#2B4E77",
               size = 18.7) +
    geom_moon(
      aes(ratio = (mean / 100)),
      right = FALSE,
      fill = "white",
      color = "white",
      key_glyph = draw_key_moon_left,
      size = 17
    ) +
    geom_moon(
      aes(ratio = 1 - ((mean / 100))),
      fill = "#418FDE",
      color = "#418FDE",
      size = 17
    ) +
    geom_text(
      aes(x = plot_cords$x, y = plot_cords$y, label = country),
      vjust = -4,
      colour = "#2B4E77"
    ) +
    annotate(
      geom = "richtext",
      label = "**All votes**",
      x = 0,
      y = 0,
      colour = "#418FDE",
      size = 10,
      fill = NA,
      label.color = NA
    ) +
    xlim(-1.2, 1.2) +
    ylim(-1.2, 1.5) +
    coord_equal() +
    theme_void()
)


# UK HR --------------------------------------------------------

UK_similarity_HR <- tt_un %>%
  group_by(rcid) %>%
  filter(any(country == "United Kingdom")) %>%
  filter(issue == "Human rights") %>%
  select(rcid, country_code, vote, country) %>%
  mutate(vote = as.numeric(as.factor(vote))) %>%
  na.omit() %>%
  filter(country %in% countries_2019) %>%
  group_by(rcid) %>%
  group_modify( ~ {
    .x %>% mutate(
      sim = vote - filter(., country_code == 'GB') %>% pull(vote),
      sim = ifelse(sim != 0, 0, 100)
    )
  }) %>%
  group_by(country, country_code) %>%
  summarise(mean = mean(sim))

UK_HR_head <- UK_similarity_HR %>%
  filter(country != "United Kingdom") %>%
  arrange(desc(mean)) %>%
  head(5)

UK_HR_tail <- UK_similarity_HR %>%
  filter(country != "United Kingdom") %>%
  arrange(desc(mean)) %>%
  tail(5)

UK_HR_all <- rbind(UK_HR_head, UK_HR_tail)

UK_HR <- UK_HR_all %>%
  ggplot(aes(x = plot_cords$x, y = plot_cords$y)) +
  geom_point(colour = "#2B4E77",
             size = 12.5) +
  geom_moon(
    aes(ratio = (mean / 100)),
    right = FALSE,
    fill = "white",
    color = "white",
    key_glyph = draw_key_moon_left,
    size = 11
  ) +
  geom_moon(aes(ratio = 1 - ((mean / 100))),
            fill = "#418FDE",
            color = "#418FDE",
            size = 11) +
  geom_text(
    aes(x = plot_cords$x, y = plot_cords$y, label = country_code),
    vjust = 3,
    colour = "#2B4E77"
  ) +
  annotate(
    geom = "richtext",
    label = "**Human <br>rights**",
    x = 0 ,
    y = 0,
    size = 5,
    fill = NA,
    label.color = NA,
    colour = "#418FDE"
  ) +
  xlim(-1.2, 1.2) +
  ylim(-1.2, 1.2) +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_markdown(
    size = 14,
    colour = "#418FDE",
    hjust = 0.5
  ))

# UK Economic development -------------------------------------------------

UK_similarity_ED <- tt_un %>%
  group_by(rcid) %>%
  filter(any(country == "United Kingdom")) %>%
  filter(issue == "Economic development") %>%
  select(rcid, country_code, vote, country) %>%
  mutate(vote = as.numeric(as.factor(vote))) %>%
  na.omit() %>%
  filter(country %in% countries_2019) %>%
  group_by(rcid) %>%
  group_modify( ~ {
    .x %>% mutate(
      sim = vote - filter(., country_code == 'GB') %>% pull(vote),
      sim = ifelse(sim != 0, 0, 100)
    )
  }) %>%
  group_by(country, country_code) %>%
  summarise(mean = mean(sim))

UK_ED_head <- UK_similarity_ED %>%
  filter(country != "United Kingdom") %>%
  arrange(desc(mean)) %>%
  head(5)

UK_ED_tail <- UK_similarity_ED %>%
  filter(country != "United Kingdom") %>%
  arrange(desc(mean)) %>%
  tail(5)

UK_ED_all <- rbind(UK_ED_head, UK_ED_tail)

UK_ED <- UK_ED_all %>%
  ggplot(aes(x = plot_cords$x, y = plot_cords$y)) +
  geom_point(colour = "#2B4E77",
             size = 12.5) +
  geom_moon(
    aes(ratio = (mean / 100)),
    right = FALSE,
    fill = "white",
    color = "white",
    key_glyph = draw_key_moon_left,
    size = 11
  ) +
  geom_moon(aes(ratio = 1 - ((mean / 100))),
            fill = "#418FDE",
            color = "#418FDE",
            size = 11) +
  geom_text(
    aes(x = plot_cords$x, y = plot_cords$y, label = country_code),
    vjust = 3,
    colour = "#2B4E77"
  ) +
  annotate(
    geom = "richtext",
    label = "**Economic <br>development**",
    x = 0 ,
    y = 0,
    size = 5,
    fill = NA,
    label.color = NA,
    colour = "#418FDE"
  ) +
  xlim(-1.2, 1.2) +
  ylim(-1.2, 1.2) +
  coord_equal() +
  theme_void()


# UK Coloniasim -----------------------------------------------------------

UK_similarity_COL <- tt_un %>%
  group_by(rcid) %>%
  filter(any(country == "United Kingdom")) %>%
  filter(issue == "Colonialism") %>%
  select(rcid, country_code, vote, country) %>%
  mutate(vote = as.numeric(as.factor(vote))) %>%
  na.omit() %>%
  filter(country %in% countries_2019) %>%
  group_by(rcid) %>%
  group_modify( ~ {
    .x %>% mutate(
      sim = vote - filter(., country_code == 'GB') %>% pull(vote),
      sim = ifelse(sim != 0, 0, 100)
    )
  }) %>%
  group_by(country, country_code) %>%
  summarise(mean = mean(sim))

UK_COL_head <- UK_similarity_COL %>%
  filter(country != "United Kingdom") %>%
  arrange(desc(mean)) %>%
  head(5)

UK_COL_tail <- UK_similarity_COL %>%
  filter(country != "United Kingdom") %>%
  arrange(desc(mean)) %>%
  tail(5)

UK_COL_all <- rbind(UK_COL_head, UK_COL_tail)

(
  UK_COL <- UK_COL_all %>%
    ggplot(aes(x = plot_cords$x, y = plot_cords$y)) +
    geom_point(colour = "#2B4E77",
               size = 12.5) +
    geom_moon(
      aes(ratio = (mean / 100)),
      right = FALSE,
      fill = "white",
      color = "white",
      key_glyph = draw_key_moon_left,
      size = 11
    ) +
    geom_moon(
      aes(ratio = 1 - ((mean / 100))),
      fill = "#418FDE",
      color = "#418FDE",
      size = 11
    ) +
    geom_text(
      aes(x = plot_cords$x, y = plot_cords$y, label = country_code),
      vjust = 3,
      colour = "#2B4E77"
    ) +
    annotate(
      geom = "richtext",
      label = "**Colonialism**",
      x = 0 ,
      y = 0,
      size = 5,
      fill = NA,
      label.color = NA,
      colour = "#418FDE"
    ) +
    xlim(-1.2, 1.2) +
    ylim(-1.2, 1.2) +
    coord_equal() +
    theme_void() +
    theme(plot.title = element_markdown(
      size = 14,
      colour = "#418FDE",
      hjust = 0.5
    ))
)


# UK Palestinian conflict -------------------------------------------------

UK_similarity_PC <- tt_un %>%
  group_by(rcid) %>%
  filter(any(country == "United Kingdom")) %>%
  filter(issue == "Palestinian conflict") %>%
  select(rcid, country_code, vote, country) %>%
  mutate(vote = as.numeric(as.factor(vote))) %>%
  na.omit() %>%
  filter(country %in% countries_2019) %>%
  group_by(rcid) %>%
  group_modify( ~ {
    .x %>% mutate(
      sim = vote - filter(., country_code == 'GB') %>% pull(vote),
      sim = ifelse(sim != 0, 0, 100)
    )
  }) %>%
  group_by(country, country_code) %>%
  summarise(mean = mean(sim))

UK_PC_head <- UK_similarity_PC %>%
  filter(country != "United Kingdom") %>%
  arrange(desc(mean)) %>%
  head(5)

UK_PC_tail <- UK_similarity_PC %>%
  filter(country != "United Kingdom") %>%
  arrange(desc(mean)) %>%
  tail(5)

UK_PC_all <- rbind(UK_PC_head, UK_PC_tail)

UK_PC <- UK_PC_all %>%
  ggplot(aes(x = plot_cords$x, y = plot_cords$y)) +
  geom_point(colour = "#2B4E77",
             size = 12.5) +
  geom_moon(
    aes(ratio = (mean / 100)),
    right = FALSE,
    fill = "white",
    color = "white",
    key_glyph = draw_key_moon_left,
    size = 11
  ) +
  geom_moon(aes(ratio = 1 - ((mean / 100))),
            fill = "#418FDE",
            color = "#418FDE",
            size = 11) +
  geom_text(
    aes(x = plot_cords$x, y = plot_cords$y, label = country_code),
    vjust = 3,
    colour = "#2B4E77"
  ) +
  annotate(
    geom = "richtext",
    label = "**Palestinian <br>conflict**",
    x = 0 ,
    y = 0,
    size = 5,
    fill = NA,
    label.color = NA,
    colour = "#418FDE"
  ) +
  xlim(-1.2, 1.2) +
  ylim(-1.2, 1.2) +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_markdown(
    size = 14,
    colour = "#418FDE",
    hjust = 0.5
  ))



# UK Arms -----------------------------------------------------------------

UK_similarity_AC <- tt_un %>%
  group_by(rcid) %>%
  filter(any(country == "United Kingdom")) %>%
  filter(issue == "Arms control and disarmament") %>%
  select(rcid, country_code, vote, country) %>%
  mutate(vote = as.numeric(as.factor(vote))) %>%
  na.omit() %>%
  filter(country %in% countries_2019) %>%
  group_by(rcid) %>%
  group_modify( ~ {
    .x %>% mutate(
      sim = vote - filter(., country_code == 'GB') %>% pull(vote),
      sim = ifelse(sim != 0, 0, 100)
    )
  }) %>%
  group_by(country, country_code) %>%
  summarise(mean = mean(sim))

UK_AC_head <- UK_similarity_AC %>%
  filter(country != "United Kingdom") %>%
  arrange(desc(mean)) %>%
  head(5)

UK_AC_tail <- UK_similarity_AC %>%
  filter(country != "United Kingdom") %>%
  arrange(desc(mean)) %>%
  tail(5)

UK_AC_all <- rbind(UK_AC_head, UK_AC_tail)

UK_AC <- UK_AC_all %>%
  ggplot(aes(x = plot_cords$x, y = plot_cords$y)) +
  geom_point(colour = "#2B4E77",
             size = 12.5) +
  geom_moon(
    aes(ratio = (mean / 100)),
    right = FALSE,
    fill = "white",
    color = "white",
    key_glyph = draw_key_moon_left,
    size = 11
  ) +
  geom_moon(aes(ratio = 1 - ((mean / 100))),
            fill = "#418FDE",
            color = "#418FDE",
            size = 11) +
  geom_text(
    aes(x = plot_cords$x, y = plot_cords$y, label = country_code),
    vjust = 3,
    colour = "#2B4E77"
  ) +
  annotate(
    geom = "richtext",
    label = "**Arms control <br>and disarmament**",
    x = 0 ,
    y = 0,
    size = 5,
    fill = NA,
    label.color = NA,
    colour = "#418FDE"
  ) +
  xlim(-1.2, 1.2) +
  ylim(-1.2, 1.2) +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_markdown(
    size = 14,
    colour = "#418FDE",
    hjust = 0.5
  ))



# UK Nuclear --------------------------------------------------------------

UK_similarity_NUC <- tt_un %>%
  group_by(rcid) %>%
  filter(any(country == "United Kingdom")) %>%
  filter(issue == "Nuclear weapons and nuclear material") %>%
  select(rcid, country_code, vote, country) %>%
  mutate(vote = as.numeric(as.factor(vote))) %>%
  na.omit() %>%
  filter(country %in% countries_2019) %>%
  group_by(rcid) %>%
  group_modify( ~ {
    .x %>% mutate(
      sim = vote - filter(., country_code == 'GB') %>% pull(vote),
      sim = ifelse(sim != 0, 0, 100)
    )
  }) %>%
  group_by(country, country_code) %>%
  summarise(mean = mean(sim))

UK_NUC_head <- UK_similarity_NUC %>%
  filter(country != "United Kingdom") %>%
  arrange(desc(mean)) %>%
  head(5)

UK_NUC_tail <- UK_similarity_NUC %>%
  filter(country != "United Kingdom") %>%
  arrange(desc(mean)) %>%
  tail(5)

UK_NUC_all <- rbind(UK_NUC_head, UK_NUC_tail)

(
  UK_NUC <- UK_NUC_all %>%
    ggplot(aes(x = plot_cords$x, y = plot_cords$y)) +
    geom_point(colour = "#2B4E77",
               size = 12.5) +
    geom_moon(
      aes(ratio = (mean / 100)),
      right = FALSE,
      fill = "white",
      color = "white",
      key_glyph = draw_key_moon_left,
      size = 11
    ) +
    geom_moon(
      aes(ratio = 1 - ((mean / 100))),
      fill = "#418FDE",
      color = "#418FDE",
      size = 11
    ) +
    geom_text(
      aes(x = plot_cords$x, y = plot_cords$y, label = country_code),
      vjust = 3,
      colour = "#2B4E77"
    ) +
    annotate(
      geom = "richtext",
      label = "**Nuclear weapons <br>and material**",
      x = 0 ,
      y = 0,
      size = 5,
      fill = NA,
      label.color = NA,
      colour = "#418FDE"
    ) +
    xlim(-1.2, 1.2) +
    ylim(-1.2, 1.2) +
    coord_equal() +
    theme_void() +
    theme(plot.title = element_markdown(
      size = 14,
      colour = "#418FDE",
      hjust = 0.5
    ))
)

# Legend ------------------------------------------------------------------
(
  legend <- ggplot() +
    geom_point(aes(x = 1, y = 1),
               colour = "#2B4E77",
               size = 33) +
    geom_moon(
      aes(x = 1, y = 1, ratio = (0.75)),
      right = FALSE,
      fill = "white",
      color = "white",
      key_glyph = draw_key_moon_left,
      size = 30
    ) +
    xlim(0.975, 1.025) +
    ylim(0.975, 1.025) +
    geom_moon(
      aes(x = 1, y = 1, ratio = 0.25),
      fill = "#418FDE",
      color = "#418FDE",
      size = 30
    ) +
    annotate(
      geom = "curve",
      x = 1.01,
      y = 1.01,
      xend = 1.0035,
      yend = 1,
      curvature = 0.1,
      arrow = arrow(length = unit(4, "mm")),
      size = 1
    ) +
    annotate(
      geom = "curve",
      x = (0.99),
      y = 1.01,
      xend = (0.998),
      yend = 1,
      curvature = -0.1,
      arrow = arrow(length = unit(4, "mm")),
      size = 1
    ) +
    annotate(
      geom = "richtext",
      label = "Votes **not** in agreement <br>with how the UK voted",
      x = (1.015),
      y = 1.01 + 0.007,
      size = 4,
      fill = NA,
      label.color = NA,
      colour = "#2B4E77"
    ) +
    annotate(
      geom = "richtext",
      label = "Votes in agreement <br>with how the UK voted",
      x = (0.985),
      y = 1.01 + 0.007,
      size = 4,
      fill = NA,
      label.color = NA,
      colour = "#2B4E77"
    ) +
    annotate(
      geom = "segment",
      x = 0.975,
      xend = 0.975,
      y = 0.975,
      yend = 1.025,
      colour = "#2B4E77"
    ) +
    annotate(
      geom = "segment",
      x = 1.025,
      xend = 1.025,
      y = 0.975,
      yend = 1.025,
      colour = "#2B4E77"
    ) +
    annotate(
      geom = "segment",
      x = 0.975,
      xend = 1.025,
      y = 1.025,
      yend = 1.025,
      colour = "#2B4E77"
    ) +
    theme_void()
)

(
  legend2 <- ggplot() +
    geom_point(aes(x = seq(1, 3, 0.5), y = 1),
               colour = "#2B4E77",
               size = 12.7) +
    geom_moon(
      aes(
        x = seq(1, 3, 0.5),
        y = 1,
        ratio = seq(0, 1, 0.25)
      ),
      right = FALSE,
      fill = "white",
      color = "white",
      key_glyph = draw_key_moon_left,
      size = 11
    ) +
    geom_moon(
      aes(
        x = seq(1, 3, 0.5),
        y = 1,
        ratio = seq(1, 0, -0.25)
      ),
      fill = "#418FDE",
      color = "#418FDE",
      size = 11
    ) +
    geom_text(
      aes(
        x = seq(1, 3, 0.5),
        y = 1.01,
        label = c("0%", "25%", "50%", "75%", "100%")
      ),
      vjust = -1.5,
      size = 5,
      colour = "#2B4E77"
    ) +
    annotate(
      geom = "segment",
      x = 0,
      xend = 0,
      y = 0.95,
      yend = 1.15,
      colour = "#2B4E77"
    ) +
    annotate(
      geom = "segment",
      x = 4,
      xend = 4,
      y = 0.95,
      yend = 1.15,
      colour = "#2B4E77"
    ) +
    annotate(
      geom = "segment",
      x = 0,
      xend = 4,
      y = 0.95,
      yend = 0.95,
      colour = "#2B4E77"
    ) +
    xlim(0, 4) +
    theme_void()
)


# Bring together ----------------------------------------------------------

layout <- c(
  area(
    t = 1,
    l = 1,
    b = 2,
    r = 4
  ),
  area(
    t = 2,
    l = 1,
    b = 3,
    r = 4
  ),
  area(
    t = 4,
    l = 1,
    b = 9,
    r = 4
  ),
  area(
    t = 1,
    l = 5,
    b = 3,
    r = 7
  ),
  area(
    t = 4,
    l = 5,
    b = 6,
    r = 7
  ),
  area(
    t = 7,
    l = 5,
    b = 9,
    r = 7
  ),
  area(
    t = 1,
    l = 8,
    b = 3,
    r = 10
  ),
  area(
    t = 4,
    l = 8,
    b = 6,
    r = 10
  ),
  area(
    t = 7,
    l = 8,
    b = 9,
    r = 10
  )
)

layout
plot(layout)

legend + legend2 + UK_all_plot + UK_HR + UK_ED + UK_COL + UK_PC + UK_AC + UK_NUC +
  plot_layout(design = layout) +
  plot_annotation(
    title = 'Voting history of countries in the UNGA compared to the United Kingdom',
    subtitle = 'The five countries with the highest and lowest voting similarity to the UK for all UN votes, and those broken down by issue',
    caption = '@jamie_bio | source: Harvard Dataverse',
    theme = theme(
      plot.title = element_text(
        size = 25,
        colour = "#418FDE",
        family = "sans",
        margin = margin(10, 0, 0, 0)
      ),
      plot.subtitle = element_markdown(
        size = 15,
        colour = "#418FDE",
        family = "sans"
      ),
      plot.caption = element_text(
        colour = "#418FDE",
        family = "sans",
        margin = margin(10, 0, 0, 0)
      )
    )
  )

ggsave(
  paste0("un_votes_", format(Sys.time(), "%d%m%Y"), ".png"),
  width = 31,
  height = 25,
  unit = "cm",
  dpi = 320,
  type = "cairo-png"
)
