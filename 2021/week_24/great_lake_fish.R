# great_lake_fish.R
# Jamie Hudson
# Created: 08 June 2021
# Edited: 08 May 2021
# Data: From Great Lake Fishery Commission


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(patchwork)
library(ggtext)
library(showtext)
library(janitor)

font_add_google("IM Fell English", "imfell")
showtext_auto()


# Load data ---------------------------------------------------------------

fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')
stocked <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/stocked.csv')

# Some dodgy duplications
fishing %>% 
  count(species)

# tidy up the data including sorting out some names- didn't matter in the end...
fishing <- mutate_all(fishing, .funs = tolower)

fishing_2 <- fishing %>% 
  mutate(species = case_when(species == "amercian eel" ~ "american eel",
                             species == "bullhead" ~ "channel catfish and bullheads",
                             species == "channel catfish" ~ "channel catfish and bullheads",
                             species == "chubs" ~ "cisco and chubs",
                             species == "cisco" ~ "cisco and chubs",
                             species == "cisco and chub" ~ "cisco and chubs",
                             species == "crappie" ~ "rock bass and crappie",
                             species == "crappies" ~ "rock bass and crappie",
                             species == "lake trout - siscowet" ~ "lake trout",
                             species == "rock bass" ~ "rock bass and crappie",
                             species == "walleye" ~ "walleye and blue pike",
                             species == "blue pike" ~ "walleye and blue pike",
                             TRUE ~ as.character(species)),
         values = parse_number(values)) %>% 
  filter(region == "u.s. total") %>% 
  group_by(year, species, lake) %>% 
  mutate(value = sum(values)) %>% 
  ungroup()

stocked_1 <- stocked %>% 
  clean_names() %>% 
  mutate(year = make_date(year))


# Produce plots -----------------------------------------------------------

(Fished <- fishing_2 %>% 
    filter(lake == "michigan",
           region == "u.s. total",
           species %in% c("lake trout", "alewife")) %>% 
    drop_na(values) %>% 
    ggplot(aes(x = as.Date(year, "%Y"), y = as.numeric(value), col = species)) +
    geom_line(size = 0.8) +
    scale_color_manual(values=c("#1f5514", "#5b2f13")) +
    scale_x_date(date_labels = "%Y", limits = as.Date(c("1879-01-01", "2020-01-01"))) +
    annotate("text", x = as.Date("1880-01-01"), y = 9000, label = "lake trout",
             family = "imfell", colour = "#5b2f13") +
    annotate("text", x = as.Date("1957-01-01"), y = 5000, label = "alewives",
             family = "imfell", colour = "#1f5514") +
    labs(y = "Production amount \n(to nearest 000 pound)") +
    annotate("text", x = as.Date("1885-01-01"), y = 45000, label = "Fished", size = 10,
             family = "imfell") +
    geom_curve(aes(x = as.Date("1915-01-01"), y = 19000, xend = as.Date("1932-01-01"), yend = 0),
               arrow = arrow(length = unit(0.1, "inch")), size = 0.2, curvature = 0.2, color = "black"
    ) +
    annotate("text", x = as.Date("1942-01-01"), y = 38000, label = "Lake trout stocks plummet",
             family = "imfell") +
    geom_curve(aes(x = as.Date("1945-01-01"), y = 33000, xend = as.Date("1948-01-01"), yend = 5000),
               arrow = arrow(length = unit(0.1, "inch")), size = 0.2, curvature = -0.2, color = "black"
    ) +
    annotate("text", x = as.Date("1915-01-01"), y = 25000, label = "Opening of the Welland Canal \n(invasion of alewives starts)",
             family = "imfell") +
    scale_y_continuous(labels = scales::comma) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), 
          axis.line = element_line(),
          plot.background = element_rect(fill = "#ffebcd", colour = "#ffebcd"),
          panel.background = element_rect(fill = "#ffebcd"),
          legend.background = element_rect(fill = "#ffebcd"),
          legend.key = element_rect(fill = "#ffebcd"),
          legend.position = "none"))

(Stocked <- stocked_1 %>%
    mutate(species = case_when(species == "CHS" ~ "Chinook Salmon",
                               species == "COS" ~ "Coho Salmon",
                               species == "LAT" ~ "Lake Trout",
                               TRUE ~ as.character(species))) %>% 
    filter(lake == "MI",
           species %in% c("Chinook Salmon", "Coho Salmon", "Lake Trout")) %>% 
    group_by(year, lake, species) %>% 
    mutate(stocked_sum = sum(no_stocked)) %>% 
    ggplot(aes(x = year, y = stocked_sum, col = species)) + 
    geom_line(size = 0.8) +
    scale_color_manual(values=c( "#cdc673", "#ffc125", "#5b2f13")) +
    labs(x = "Year", y = "Number of fish stocked") +
    annotate("text", x = as.Date("1955-01-01"), y = 600000, label = "lake trout",
             family = "imfell", colour = "#5b2f13") +
    annotate("text", x = as.Date("1998-01-01"), y = 7500000, label = "chinook salmon",
             family = "imfell", colour = "#cdc673") +
    annotate("text", x = as.Date("1968-01-01"), y = 4000000, label = "coho salmon",
             family = "imfell", colour = "#ffc125") +
    annotate("text", x = as.Date("1885-01-01"), y = 7300000, label = "Stocked", size = 10,
             family = "imfell") +
    scale_x_date(limits = as.Date(c("1879-01-01", "2020-01-01"))) +
    geom_curve(aes(x = as.Date("1981-01-01"), y = 7000000, xend = as.Date("1951-01-01"), yend = 4500000),
               arrow = arrow(length = unit(0.1, "inch")), size = 0.2, curvature = -0.1, color = "black"
    ) +
    annotate("text", x = as.Date("1930-01-01"), y = 4000000, label = "As the number of stocked salmon increased, \nthe number of alewives caught decreased.  \nPossibly due to either predation, or a change in fishing pratices?",
             family = "imfell") +
    geom_curve(aes(x = as.Date("1987-01-01"), y = 1300000, xend = as.Date("1985-06-01"), yend = 4000000),
               arrow = arrow(length = unit(0.1, "inch")), size = 0.2, curvature = -0.05, color = "black"
    ) +
    annotate("text", x = as.Date("2000-01-01"), y = 450000, label = "Stocking of lake trout did not \nincrease the number caught",
             family = "imfell") +
    theme_bw() +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), 
          axis.line = element_line(),
          plot.background = element_rect(fill = "#ffebcd", colour = "#ffebcd"),
          panel.background = element_rect(fill = "#ffebcd"),
          legend.background = element_rect(fill = "#ffebcd"),
          legend.key = element_rect(fill = "#ffebcd"),
          legend.position = "none"))

Fished / Stocked +
  plot_annotation(title = "Lake Michigan stock market",
                  subtitle = "For years, <span style = 'color:#5b2f13;'>lake trout</span> was the dominant *native* predator in the Great Lakes, until the stock collapsed in the 1940s - 1950s. This was likely caused by a mixture of overfishing,  \nthe *invasive* sea lamprey, and the *invasion* of <span style = 'color:#1f5514;'>alewives</span>. <span style = 'color:#1f5514;'>Alewives</span> invaded the lakes upon the opening of the Welland Canal in 1932, enabling them to bypass the natural barrier  \nof Niagra Falls. They predate on the eggs and young of <span style = 'color:#5b2f13;'>lake trout</span>, but when eaten themselves, they cause a nutrient deficiency in <span style = 'color:#5b2f13;'>lake trout</span>, that disrupts early  \ndevelopment. In an attempt to restore <span style = 'color:#5b2f13;'>lake trout</span>, fishery managers stocked <span style = 'color:#cdc673;'>chinook</span> and <span style = 'color:#ffc125;'>coho salmon</span> from the Pacific, with the hope these would predate on <span style = 'color:#1f5514;'>alewives</span>. At a  \nsimilar time, stocks of <span style = 'color:#5b2f13;'>lake trout</span> were also added to the lakes. It turns out that salmon are better fish to fish, and maintaining *invasive* <span style = 'color:#1f5514;'>alewife</span> numbers is important for the  \nmaintenance of a *$8 Billion a year* industry, most of which is from salmon fishing.",
                  caption = "@jamie_bio | source = Great Lakes Fishery Commission",
                  theme = theme(plot.caption.position = 'plot',
                                plot.caption = element_text(),
                                plot.title = element_text(size = 35),
                                plot.subtitle = element_markdown(size = 12, 
                                                                 lineheight = 1.1),
                                plot.background = element_rect(fill = "#ffebcd"),
                                panel.background = element_rect(fill = "#ffebcd")
                  )) & 
  theme(text = element_text(family = 'imfell'))

ggsave(
  paste0("great_lake_fish_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 12,
  height = 7.5
)
