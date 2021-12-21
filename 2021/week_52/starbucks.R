# starbucks.R
# Jamie Hudson
# Created: 21 Dec 2021
# Edited: 21 Dec 2021
# Data: PythonCoderUnicorn and Starbucks Coffee Company

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(showtext)
font_add_google("Lora")
font_add_google("Merriweather")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-12-21')
starbucks <- tuesdata$starbucks

# wrangle data ------------------------------------------------------------

starbucks_sugar <- starbucks %>% 
  mutate(sugar_cubes = sugar_g / 4,
         sugar_cubes_round = round(sugar_cubes),
         product_name= str_to_lower(product_name)) 

chosen_drinks <- c("java chip frappuccino blended",
                   "iced caffè mocha",
                   "java chip light frappuccino",
                   "iced coffee with milk",
                   "flat white",
                   "chai tea latte",
                   "hot chocolate",
                   "cappuccino"
)

chosen_drinks_2 <- c("brewed coffee - dark roast",
                     "espresso")

chosen_drinks_2_df <- starbucks_sugar %>% 
  select(product_name, sugar_cubes, sugar_cubes_round, size, milk, whip) %>% 
  filter(product_name %in% chosen_drinks_2) %>% 
  group_by(product_name) %>% 
  slice(1) %>% 
  mutate(count = 0,
         id = 0)

starbucks_sugar_df <- starbucks_sugar %>% 
  select(product_name, sugar_cubes, sugar_cubes_round, size, milk, whip) %>%
  filter(product_name %in% chosen_drinks) %>%  
  add_row(product_name = "can of coke", sugar_cubes = 39/4, sugar_cubes_round = round(39/4),
          size = NA, milk = NA, whip = NA) %>% 
  group_by(product_name) %>% 
  arrange(desc(sugar_cubes)) %>% 
  slice(1) %>% 
  mutate(count = sugar_cubes_round) %>% 
  uncount(count, .remove = F) %>%
  mutate(id = row_number() + row_number()*0.1) %>% 
  ungroup() %>% 
  rbind(., chosen_drinks_2_df) %>% 
  mutate(product_name = str_to_sentence(product_name),
         product_name = fct_reorder(product_name, sugar_cubes))

# plot ------------------------------------------------------------

coke <- starbucks_sugar_df %>% 
  filter(product_name == "Can of coke")

java_chip <- starbucks_sugar_df %>% 
  filter(product_name == "Java chip frappuccino blended")

starbucks_sugar_df %>% 
  ggplot(aes(x = product_name, y = id)) +
  geom_point(shape = 22, size = 9, fill = "grey95", colour = "grey50", stroke = 1.5) +
  geom_point(data = coke, mapping = aes(x = product_name, y = id),
             shape = 22, size = 9, fill = "#BD0314", stroke = 1.5) +
  geom_point(data = java_chip, mapping = aes(x = product_name, y = id),
             shape = 22, size = 9, fill = "#015F3E", stroke = 1.5) +
  coord_flip() +
  annotate("curve", x = 1.45, xend = 1.1,
           y = 6, yend = 1.1, size = 0.5, arrow = arrow(length = unit(0.3, "cm")),
           curvature = 0.05) +
  annotate("curve", x = 1.55, xend = 1.9,
           y = 6, yend = 1.1, size = 0.5, arrow = arrow(length = unit(0.3, "cm")),
           curvature = -0.05) +
  annotate("richtext", x = 1.5, y = 9.5,
           label = "Neither *Espressos* or *Brewed  \ncoffee* contain any sugar",
           size = 4, family = "Lora", fill = NA, label.color = NA) +
  annotate("curve", x = 10.6, xend = 8.9,
           y = 24.2, yend = 23, size = 0.5, arrow = arrow(length = unit(0.3, "cm")),
           curvature = -0.15) +
  annotate("richtext", x = 8.2, y = 22,
           label = "A venti sized *Java chip frappuccino  \nblended* contains a whopping  \n89g of sugar (~ 22 sugar cubes)",
           size = 4, family = "Lora", fill = NA, label.color = NA) +
  annotate("curve", x = 6, xend = 5.4,
           y = 13.8, yend = 16.5, size = 0.5, arrow = arrow(length = unit(0.3, "cm")),
           curvature = -0.1) +
  annotate("richtext", x = 5, y = 19.5,
           label = "Each square represents a  \nsingle sugar cube which  \ncontains ~4g of sugar",
           size = 4, family = "Lora", fill = NA, label.color = NA) +
  lims(y = c(1.1, 25)) +
  labs(title = "Whole Latte Sugar in your Starbucks Coffee",
       subtitle = "Some drinks from starbucks contain over **>80g of sugar** - the equivalent of **>20 sugar cubes**  \n... or over twice as much as a <span style='color:#BD0314;'>330ml can of Coke</span>.",
       caption = "@jamie_bio | source: PythonCoderUnicorn and Starbucks Coffee Company") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Lora", size = 10, 
                                   colour = c(rep("grey50", 4), "#BD0314", rep("grey50", 5), "#015F3E"),
                                   face = "italic", margin = margin(r = -13)),
        axis.ticks = element_blank(),
        plot.title = element_text(family = "Merriweather", size = 33, 
                                  margin = margin(10, 0, 0, 0), colour = "#015F3E"),
        plot.title.position = 'plot',
        plot.subtitle = element_markdown(family = "Lora", size = 15, margin = margin(10, 0, 0, 5),
                                         lineheight = 1.2),
        plot.caption = element_text(family = "Lora", size = 8, colour = "grey50"))


ggsave(paste0("starbucks_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 11,
       height =  7)

