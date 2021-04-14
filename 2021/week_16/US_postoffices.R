# US_postoffices.R
# Script adapted from Yan Holtz's hexbin map tutorial https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html
# Jamie Hudson
# Created: 14 Apr 2021
# Edited: 14 Apr 2021
# Data: Blevins, Cameron; Helbock, Richard W., 2021, "US Post Offices", https://doi.org/10.7910/DVN/NUKCNA, Harvard Dataverse, V1, UNF:6:8ROmiI5/4qA8jHrt62PpyA== [fileUNF]

# load libraries ----------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(geojsonio)
library(rgdal)
library(rgeos)
library(ggtext)
library(broom)
library(patchwork)


# load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 16)
post_offices <- tuesdata$post_offices

state_size <-
  read.csv(
    "https://raw.githubusercontent.com/jakevdp/data-USstates/master/state-areas.csv"
  ) # Read in state size data

state_size <- state_size %>%
  filter(!state %in% c("Puerto Rico")) %>%
  arrange(state)

# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.
spdf <- geojson_read("data/us_states_hexgrid.geojson",  what = "sp")

spdf@data = spdf@data %>%
  mutate(state = gsub(" \\(United States\\)", "", google_name))

state_size <- left_join(spdf@data, state_size, by = "state") %>%
  select(state, iso3166_2, area..sq..mi.) %>%
  rename(state_code = iso3166_2,
         size = area..sq..mi.)

# clean up data -----------------------------------------------------------

# remove some dodgy establish dates

post_offices_clean_2020 <- post_offices %>%
  select(state, established, discontinued, duration) %>%
  filter(
    established > 200,
    established <= 2020,
    established < discontinued | is.na(discontinued),
    is.na(discontinued),!state %in% c("VAy", "MI/OH")
  ) %>%
  mutate(established = as.numeric(established)) %>%
  filter(is.na(discontinued)) %>%
  group_by(state) %>%
  count()

spdf_fortified_size_post <- tidy(spdf, region = "iso3166_2") %>%
  arrange(id) %>%
  left_join(. , state_size, by = c("id" = "state_code")) %>%
  left_join(. , post_offices_clean_2020, by = c("id" = "state")) %>%
  mutate(density = n / size,
         rev_density = size / n)

# Calculate the centroid of each hexagon to add the label:
centers <-
  cbind.data.frame(data.frame(gCentroid(spdf, byid = TRUE), id = spdf@data$iso3166_2))

centers_join <- spdf_fortified_size_post %>%
  group_by(id) %>%
  slice(c(n())) %>%
  left_join(. , centers, by = c("id" = "id")) %>%
  select(x, y, id, density, rev_density)

NJ_density <- spdf_fortified_size_post %>%
  group_by(state) %>%
  slice(c(n())) %>%
  ungroup() %>%
  slice_max(density, n = 1)

AK_density <- spdf_fortified_size_post %>%
  group_by(state) %>%
  slice(c(n())) %>%
  ungroup() %>%
  slice_max(rev_density, n = 1)

NJ_arrow <-
  tibble(
    x1 = -70,
    x2 = -80,
    y1 = 32,
    y2 = 43
  )

AK_arrow <-
  tibble(
    x1 = -122,
    x2 = -131,
    y1 = 52,
    y2 = 53.4
  )


# produce plots -----------------------------------------------------------

# cut density data into bin
spdf_fortified_size_post$bin_den <-
  cut(
    spdf_fortified_size_post$density * 1000 ,
    breaks = c(seq(0, 50, 10), Inf),
    labels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51+"),
    include.lowest = TRUE
  )

(
  A <- ggplot() +
    geom_polygon(data = spdf_fortified_size_post, aes(
      fill =  bin_den,
      x = long,
      y = lat,
      group = group
    )) +
    scale_fill_viridis_d(
      option = "E",
      end = 0.9,
      guide = guide_legend(
        keyheight = unit(5, units = "mm"),
        keywidth = unit(10, units = "mm"),
        label.position = "right",
        title.position = 'top',
        nrow = 6
      )
    ) +
    geom_text(
      data = centers_join,
      aes(x = x, y = y, label = id),
      size = 3,
      colour = ifelse(centers_join$id == "NJ", "black", "white")
    ) +
    labs(fill = "Density of Post Offices  \n(# per 1000 sq. mi.)") +
    annotate(
      "richtext",
      x = -70,
      y = 30,
      family = "Source Sans Pro",
      size = 3.5,
      color = "black",
      lineheight = .9,
      label = glue::glue(
        "New Jersey had on average  \n**<span style='color:#CBBA69FF'>{round(NJ_density$density, 4)*1000}</span>** Post Offices per 1000 sq. mi."
      ),
      label.colour = NA,
      fill = NA
    ) +
    geom_segment(
      data = NJ_arrow,
      aes(
        x = x1,
        y = y1,
        xend = x2,
        yend = y2
      ),
      arrow = arrow(length = unit(0.07, "inch")),
      size = 0.8,
      color = "gray20"
    ) +
    theme_void() +
    theme(legend.position = "left",
          legend.title = element_markdown()) +
    coord_map(clip = 'off')
)

# cut rev_density data into bin
spdf_fortified_size_post$bin_rev <-
  cut(
    spdf_fortified_size_post$rev_density ,
    breaks = c(seq(0, 250, 50), Inf),
    labels = c("0-50", "51-100", "101-150", "151-200", "201-250", "251+"),
    include.lowest = TRUE
  )

(
  B <- ggplot() +
    geom_polygon(data = spdf_fortified_size_post, aes(
      fill =  bin_rev,
      x = long,
      y = lat,
      group = group
    )) +
    scale_fill_viridis_d(
      option = "E",
      end = 0.9,
      guide = guide_legend(
        keyheight = unit(5, units = "mm"),
        keywidth = unit(10, units = "mm"),
        label.position = "right",
        title.position = 'top',
        nrow = 6
      )
    ) +
    geom_text(
      data = centers_join,
      aes(x = x, y = y, label = id),
      size = 3,
      colour = ifelse(centers_join$rev_density > 250, "black", "white")
    ) +
    labs(fill = "Average area between  \nPost Offices (sq. mi.)") +
    annotate(
      "richtext",
      x = -108,
      y = 52,
      family = "Source Sans Pro",
      size = 3.5,
      color = "black",
      lineheight = .9,
      label = glue::glue(
        "Alaska only had on average  \none Post Office every **<span style='color:#CBBA69FF'>{round(AK_density$rev_density, 0)} miles</span>**"
      ),
      label.colour = NA,
      fill = NA
    ) +
    geom_segment(
      data = AK_arrow,
      aes(
        x = x1,
        y = y1,
        xend = x2,
        yend = y2
      ),
      arrow = arrow(length = unit(0.07, "inch")),
      size = 0.8,
      color = "gray20"
    ) +
    theme_void() +
    theme(legend.position = "left",
          legend.title = element_markdown()) +
    coord_map(clip = 'off')
)

A / B +
  plot_annotation(title = 'US Post Offices in 2000',
                  subtitle = 'If you found yourself lost in the USA with a desperate need to post a letter in the year 2000, \nyou had better hope you were in New Jersey and not Alaska',
                  caption = '@jamiebio | source: Blevins & Helbock, 2021, "US Post Offices", Harvard Dataverse') &
  theme(
    plot.title = element_text(size = 22),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(colour = "grey40"),
    legend.position = 'left',
    text = element_text("Source Sans Pro")
  )

ggsave(
  paste0("us_post", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 10,
  height = 8.36,
  type = "cairo-png"
)
