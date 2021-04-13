library(tidytuesdayR)
library(tidyverse)
library(showtext)

tuesdata <- tidytuesdayR::tt_load(2021, week = 14)

allCategories <- tuesdata$allCategories

# brands ------------------------------------------------------------------

top_brands <- allCategories %>%
  group_by(brand) %>%
  summarise(count = n()) %>%
  slice_max(n = 10, order_by = count) %>%
  pull(brand)

brands_sorted <- allCategories %>%
  filter(brand %in% top_brands) %>%
  group_by(brand) %>%
  mutate(lightness_brand = mean(lightness)) %>%
  ungroup() %>%
  select(name, lightness, lightness_brand, hex, brand) %>%
  mutate(brand = fct_reorder(brand,-lightness_brand))

all_avg <-
  allCategories %>%
  summarize(avg = mean(lightness, na.rm = T)) %>%
  pull(avg)

arrows <-
  tibble(
    x1 = c(0.1, 4.3, 3.7, 4.3, 10.7, 0.1),
    x2 = c(0.2, 5, 3, 4.1, 10.15,-0.2),
    y1 = c(0.67, 0.25, 0.28, 0.28, 0.6, 0.95),
    y2 = c(all_avg + 0.005, 0.3, 0.3, 0.355, 0.6, 0.99)
  )

black_arrow <-
  tibble(x1 = 0.1,
         x2 = -0.2,
         y1 = 0.05,
         y2 = 0.01)

theme_set(theme_light(base_size = 18, base_family = "Source Sans Pro"))

set.seed(2021)
(
  g <-
    ggplot(brands_sorted, aes(
      x = brand, y = lightness, colour = hex
    )) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 1), expand = c(0.005, 0.005)) +
    scale_x_discrete(expand = expansion(mult = c(0.15, 0.15))) +
    scale_colour_identity() +
    labs(
      title = "Bias within cosmetic brands towards lighter skin tones",
      caption = "@jamiebio | source: The Pudding",
      x = NULL,
      y = "Lightness"
    ) +
    geom_hline(aes(yintercept = all_avg), color = "gray40", size = 0.6) +
    geom_jitter(
      size = 2,
      alpha = 0.8,
      width = 0.2
    ) +
    geom_point(
      aes(x = brand, y = lightness_brand),
      shape = 21,
      colour = "white",
      fill = "black",
      size = 3.5,
      stroke = 1
    ) +
    geom_segment(
      aes(
        x = brand,
        xend = brand,
        y = all_avg,
        yend = lightness_brand
      ),
      size = 0.8,
      colour = "black"
    )  +
    geom_curve(
      data = arrows,
      aes(
        x = x1,
        y = y1,
        xend = x2,
        yend = y2
      ),
      arrow = arrow(length = unit(0.07, "inch")),
      size = 0.4,
      color = "gray20",
      curvature = -0.1
    ) +
    geom_curve(
      data = black_arrow,
      aes(
        x = x1,
        y = y1,
        xend = x2,
        yend = y2
      ),
      arrow = arrow(length = unit(0.07, "inch")),
      size = 0.4,
      color = "gray20",
      curvature = +0.1
    ) +
    annotate(
      "text",
      x = 0.2,
      y = 0.73,
      family = "Source Sans Pro",
      size = 3.3,
      color = "gray20",
      lineheight = .9,
      label = glue::glue("Average lightness of \nall products: {round(all_avg, 2)}")
    ) +
    annotate(
      "text",
      x = 3.9,
      y = 0.24,
      family = "Source Sans Pro",
      size = 3.3,
      color = "gray20",
      lineheight = .9,
      label = "Individual \nproducts"
    ) +
    annotate(
      "text",
      x = 10.9,
      y = 0.58,
      family = "Source Sans Pro",
      size = 3.3,
      color = "gray20",
      lineheight = .9,
      label = "Brand average"
    ) +
    annotate(
      "text",
      x = 0.3,
      y = 0.05,
      family = "Source Sans Pro",
      size = 3.3,
      color = "gray20",
      lineheight = .9,
      label = "Pure black"
    ) +
    annotate(
      "text",
      x = 0.3,
      y = 0.95,
      family = "Source Sans Pro",
      size = 3.3,
      color = "gray20",
      lineheight = .9,
      label = "Pure white"
    ) +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 16),
      axis.text.x = element_text(family = "Source Sans Pro", size = 12),
      panel.grid = element_blank(),
      plot.subtitle = element_text(size = 10),
      plot.caption = element_text(size = 9, color = "gray50")
    )
)

ggsave(
  paste0("makeup_shades_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320
)
