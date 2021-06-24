# parks.R
# Jamie Hudson
# Created: 24 June 2021
# Edited: 24 June 2021
# Data: From The Trust for Public Land


# load libraries ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(maps)
library(showtext)
library(grid)
library(RColorBrewer)
library(ggchicklet)
library(patchwork)
library(ggtext)
font_add_google("Abril Fatface", "abril")
font_add_google("Poiret One", "poiret")
font_add_google("Limelight", "lime")
showtext_auto()

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 26)
parks <- tuesdata$parks

# tidy some city names - was part of exploratory process which I didn't need in the end

us.cities$name <- us.cities$name %>% str_sub(.,1,nchar(.)-3)

parks_states <- parks %>% 
  left_join(., us.cities, by = c("city" = "name")) %>% 
  rename(region = "country.etc")

# Function for background gradient from Kamil Slowikowski (https://stackoverflow.com/questions/30136725/plot-background-colour-in-gradient) 

make_gradient <- function(deg = 45, n = 100, cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (180 / pi)
  mat <- matrix(
    data = rep(seq(0, 1, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 1, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"), 
    interpolate = TRUE
  )
}


# NY plot -----------------------------------------------------------------

g_ny <- make_gradient(
  deg = 270, n = 900, cols = c("#FEFFA5", "#FFF3A4", "#CEE2B5", "#7DD4CE", "#40CFE9")
)

(buff_cloud <- parks_states %>% 
    filter(region == "NY",
           city == "Buffalo") %>% 
    ggplot(aes(x = year, y = total_points)) +
    stat_smooth(
      geom = 'area', method = 'loess', span = 0.4,
      alpha = 1/2, fill = "white") +
    annotate("text", x = 2018.8, y = 17, label = "BUFFALO",
             colour = "#1A4036", size = 3, family = "abril") +
    theme_void())

(NY_cloud <- parks_states %>% 
    filter(region == "NY",
           city == "New York") %>% 
    ggplot(aes(x = year, y = total_points)) +
    stat_smooth(
      geom = 'area', method = 'loess', span = 0.3,
      alpha = 1/2, fill = "white") +
    annotate("text", x = 2017.8, y = 23, label = "NEW YORK",
             colour = "#1A4036", size = 3, family = "abril") +
    theme_void())


(ny_poster <- parks_states %>% 
  filter(region == "NY") %>% 
  ggplot(aes(x = interaction(city, year))) +
  annotation_custom(
    grob = g_ny, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) + 
  geom_col(aes(y = pct_near_park_points),  fill = "#8BC4A8",
           width = 1, position="dodge") +
  geom_col(aes(y = spend_per_resident_points), fill = "#E3A868",
           width = 1, position = "dodge") +
  geom_text(aes(y = pct_near_park_points, 
                  label = ifelse(year=="2020" & city == "New York", 
                                 paste0(pct_near_park_points, "\npoints"),"")), 
            vjust = 0.3, family = "abril", colour = "white") +
  geom_text(aes(y = spend_per_resident_points, 
                label = ifelse(year=="2012" & city == "New York", 
                               paste0(pct_near_park_points, "\npoints"),"")), 
            family = "abril", colour = "white") +
  geom_chicklet(aes(y = park_benches), fill = "#739373",
                colour = "#739373", width = 0.5,
                radius = grid::unit(1, 'mm')) +
  annotate("text", x = 8, y = 160, label = "New York State", size = 15,
           family = "lime", colour = "#1A4036") +
  annotate("text", x = 8, y = 142, label = "Parks", size = 12,
           family = "lime", colour = "#1A4036") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 180)) +
  labs(caption = "N.B. This datavis was built with aesthetics in mind rather than true data storytelling.  \nTime travels from 2012 to 2020 from left to right.  \nBuildings represent <span style = 'color:#E3A868;'>$ spent per resident (points)</span> and <span style = 'color:#8BC4A8;'>% residents within a 10 min walk to a park (points)</span>.  \nShrubs represent the <span style = 'color:#739373;'>number of park benches</span> (stopped measuring after 2017).  \nClouds are smoothed area plots of the total points with time for each individual city.  \n@jamiebio | source: The Trust for Public Land") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#FEFFA5"),
        plot.caption = element_markdown(lineheight = 1.1,
                                        family = "poiret")) +
  inset_element(buff_cloud, 0.7, 0.65, 0.9, 0.75) +
  inset_element(NY_cloud, 0.2, 0.4, 0.4, 0.5))

ggsave(paste0("ny_park_access_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 7,
       height = 8)


# IL plot -----------------------------------------------------------------

g_il <- make_gradient(
  deg = 270, n = 900, cols = c("#BB7295", "#F2B289", "#81689D", "#3F5EA7", "#1E4D98", "#023A81")
)

(aur_cloud <- parks_states %>% 
    filter(region == "IL",
           city == "Aurora") %>% 
    ggplot(aes(x = year, y = total_points)) +
    stat_smooth(
      geom = 'area', method = 'loess', span = 0.4,
      alpha = 0.8, fill = "#A26B9F") +
    annotate("text", x = 2018.8, y = 20, label = "AURORA",
             colour = "#FADCA5", size = 3.5, family = "poiret") +
    theme_void())

(chic_cloud <- parks_states %>% 
    filter(region == "IL",
           city == "Chicago") %>% 
    ggplot(aes(x = year, y = total_points)) +
    stat_smooth(
      geom = 'area', method = 'loess', span = 0.3,
      alpha = 0.8, fill = "#A26B9F") +
    annotate("text", x = 2017.8, y = 25, label = "CHICAGO",
             colour = "#FADCA5", size = 3.5, family = "poiret") +
    theme_void())

(il_poster <- parks_states %>% 
  filter(region == "IL") %>% 
  ggplot(aes(x = interaction(city, year))) +
  annotation_custom(
    grob = g_il, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) + 
  geom_col(aes(y = pct_near_park_points),  fill = "#81689A",
           width = 1, position="dodge") +
  geom_col(aes(y = spend_per_resident_points), fill = "#9B6398",
           width = 1, position = "dodge") +
    geom_text(aes(y = pct_near_park_points, 
                  label = ifelse(year=="2020" & city == "Chicago", 
                                 paste0(pct_near_park_points, "\npoints"),"")),
              vjust = 0.3, family = "poiret", colour = "white") +
  geom_text(aes(y = spend_per_resident_points, 
                label = ifelse(year=="2012" & city == "Chicago", 
                               paste0(pct_near_park_points, "\npoints"),"")),
            vjust = 0.5, family = "poiret", colour = "white") +
  geom_chicklet(aes(y = park_benches), fill = "#E7CCA2",
                colour = "#E7CCA2", width = 0.5,
                radius = grid::unit(1, 'mm')) +
  annotate("text", x = 8, y = 160, label = "Illinois Parks", size = 15,
           family = "lime", colour = "#FFCD73") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 180)) +
  labs(caption = "N.B. This datavis was built with aesthetics in mind rather than true data storytelling.  \nTime travels from 2012 to 2020 from left to right.  \nBuildings represent <span style = 'color:#9B6398;'>$ spent per resident (points)</span> and <span style = 'color:#81689A;'>% residents within a 10 min walk to a park (points)</span>.  \nStreet lights represent the <span style = 'color:#E7CCA2;'>number of park benches</span> (stopped measuring after 2017).  \nClouds are smoothed area plots of the total points with time for each individual city.  \n@jamiebio | source: The Trust for Public Land") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#FEFFA5"),
        plot.caption = element_markdown(lineheight = 1.1,
                                        family = "poiret")) +
  inset_element(aur_cloud, 0.5, 0.45, 0.7, 0.55) +
  inset_element(chic_cloud, 0.1, 0.7, 0.3, 0.8))

ggsave(paste0("il_park_access_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 7,
       height = 8)


