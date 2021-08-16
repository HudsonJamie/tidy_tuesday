# BEA.R
# Jamie Hudson
# Created: 16 August 2021
# Edited: 16 August 2021
# Data: Bureau of Economic Analysis (https://www.bea.gov/system/files/papers/BEA-WP2020-12.pdf)

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(patchwork)
library(showtext)
font_add_google("Volkhov", "volk")
font_add_google("Libre Franklin", "franklin")
showtext_auto()


# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 33)
investment <- tuesdata$investment
chain_investment <- tuesdata$chain_investment
ipd <- tuesdata$ipd

# wrangle data ------------------------------------------------------------

unique(investment$meta_cat)
investment %>% filter(meta_cat == "Digital") %>% 
  pull(category) %>% unique()

# plot ------------------------------------------------------------

(plot_1 <- investment %>% filter(meta_cat == "Conservation and development") %>% 
   ggplot(aes(x = year, y = gross_inv, fill = category)) +
   geom_area() +
   labs(y = "Millions of USD",
        title = "Gross investment") +
   lims(y = c(0, 14500)) +
   scale_x_continuous(breaks = seq(1950, 2010, 20)) +
   scale_fill_manual(values = c("#E8E0CF", "#98B7B2", "#F47366")) +
   theme(panel.background = element_rect(fill = "#432d4a"),
         plot.background = element_rect(fill = "#432d4a",
                                        colour = NA),
         panel.grid = element_blank(),
         panel.grid.major.y = element_line(linetype = "dashed",
                                           colour = "white",
                                           size = 0.2),
         plot.title = element_text(hjust = 0.5,
                                   size = 10,
                                   family = "franklin", 
                                   face = "bold"),
         legend.background = element_rect(fill = "#432d4a"),
         legend.title = element_blank(),
         legend.direction = "horizontal",
         legend.margin=margin(c(-10,5,5,5)),
         legend.text = element_text(size = 7),
         text = element_text(colour = "white", family = "franklin"),
         axis.text.x = element_text(angle = 45),
         axis.title.x = element_blank(),
         axis.text = element_text(colour = "white",
                                  size = 7),
         axis.title = element_text(size = 10))
)

(plot_2 <- chain_investment %>% filter(meta_cat == "Conservation and development") %>% 
    ggplot(aes(x = year, y = gross_inv_chain, fill = category)) +
    geom_area() +
    labs(title = "Investment adjusted for inflation") +
    lims(y = c(0, 14500)) +
    scale_x_continuous(breaks = seq(1950, 2010, 20)) +
    scale_fill_manual(values = c("#E8E0CF", "#98B7B2", "#F47366")) +
    theme(panel.background = element_rect(fill = "#432d4a"),
          plot.background = element_rect(fill = "#432d4a",
                                         colour = NA),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(linetype = "dashed",
                                            colour = "white",
                                            size = 0.2),
          plot.title = element_text(hjust = 0.5,
                                    size = 10,
                                    family = "franklin", 
                                    face = "bold"),
          legend.background = element_rect(fill = "#432d4a"),
          legend.title = element_blank(),
          legend.direction = "horizontal",
          legend.text = element_text(size = 7),
          legend.margin= margin(c(-10,5,5,5)),
          text = element_text(colour = "white", family = "franklin"),
          axis.text = element_text(colour = "white"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45,
                                     size = 7),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank())
)

plot_1 + plot_2 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = 'U.S. Investment in Conservation and Development',
    subtitle = "On the 6th August 2021, the Intergovernmental Panel on Climate Change (IPCC) finalised its Sixth Assessment Report. \nDespite a damning assessment on the climate crisis affecting the world, investment in infrastructure relating to \nConservation and Development (dams, levees, sea walls, and related assets) is not increasing with inflation.",
    caption = "@jamie_bio | source:  Bureau of Economic Analysis",
    theme = theme(panel.background = element_rect(fill = "#432d4a"),
                  plot.background = element_rect(fill = "#432d4a"),
                  text = element_text(colour = "white",
                                      family = "volk"),
                  plot.title = element_text(size = 19,
                                            face = "bold"),
                  plot.subtitle = element_text(size = 8, 
                                               family = "franklin",
                                               lineheight = 1.1),
                  plot.caption = element_text(size = 6),
                  legend.position = "bottom",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 7)))

ggsave(
  paste0("BEA_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 7,
  height = 5
)
