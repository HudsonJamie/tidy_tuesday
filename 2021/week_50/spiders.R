# spiders.R
# Jamie Hudson
# Created: 07 December 2021
# Edited: 07 December 2021
# Data: World Spider Database (https://wsc.nmbe.ch/dataresources)

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(ggExtra)
library(showtext)
library(ggforce)
font_add_google("Offside")
font_add_google("Rambla")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-12-07')
spiders <- tuesdata$spiders

# wrangle data ------------------------------------------------------------

malaysia_spiders <- spiders %>%
  filter(stringr::str_detect(distribution, "Malaysia")) %>%
  group_by(family) %>%
  summarise(n_species = n()) %>%
  ungroup() %>%
  slice_max(order_by = n_species, n = 10) %>% 
  mutate(family = fct_reorder(family, -n_species),
         family.fac = factor(family),
         family.num = as.numeric(as.factor(family)))

# plot ------------------------------------------------------------

ggplot(malaysia_spiders) +
  geom_segment(mapping = aes(x = family.fac, y = n_species, xend = family.fac), yend = 0,
               linetype = "dashed", colour = "grey40",
               size = 0.5) +
  geom_ellipse(aes(x0 = family.num, y0 = n_species - 1.5, b = 2.2,
                   a = 0.12, angle = 0), 
               fill = "#40342A",
               colour = "#9C8150") +
  geom_curve(mapping = aes(x = family.num, xend = family.num + 0.3,
           y = n_species + 3, yend = n_species + 6), size = 0.4,
           curvature = -0.4, colour = "#9C8150") + # bottom right leg
  geom_curve(mapping = aes(x = family.num, xend = family.num + 0.3,
                           y = n_species + 2.5, yend = n_species + 4), size = 0.4,
             curvature = -0.25, colour = "#9C8150") + # 2nd bottom right leg
  geom_curve(mapping = aes(x = family.num, xend = family.num + 0.3,
                           y = n_species + 2, yend = n_species), size = 0.4,
             curvature = 0.25, colour = "#9C8150") + #2nd top right leg
  geom_curve(mapping = aes(x = family.num, xend = family.num + 0.3,
                           y = n_species + 1.5, yend = n_species - 3), size = 0.4,
             curvature = 0.4, colour = "#9C8150") + # top right leg
  geom_curve(mapping = aes(x = family.num, xend = family.num - 0.3,
                           y = n_species + 3, yend = n_species + 6), size = 0.4,
             curvature = 0.4, colour = "#9C8150") + # bottom left leg
  geom_curve(mapping = aes(x = family.num, xend = family.num - 0.3,
                           y = n_species + 2.5, yend = n_species + 4), size = 0.4,
             curvature = 0.25, colour = "#9C8150") + # 2nd bottom left leg
  geom_curve(mapping = aes(x = family.num, xend = family.num - 0.3,
                           y = n_species + 2, yend = n_species), size = 0.4,
             curvature = -0.25, colour = "#9C8150") + #2nd top left leg
  geom_curve(mapping = aes(x = family.num, xend = family.num - 0.3,
                           y = n_species + 1.5, yend = n_species - 3), size = 0.4,
             curvature = -0.4, colour = "#9C8150") + # top left leg
  geom_text(aes(x = family.num, y = n_species - 1.5, label = n_species),
           colour = "white", size = 2, 
           family = "Rambla") +
  geom_text(aes(x = family.num - 0.2, y = 0, label = toupper(family)),
            colour = "grey50", size = 2.5, angle = -90, hjust = 0,
            family = "Rambla") +
  geom_point(mapping = aes(x = family.fac, y = n_species + 2),
             size = 4, pch = 21,
             fill = "#40342A",
             colour = "#9C8150") +
  geom_text(x = 6.2, y = -75, label = "SPIDERS OF MALAYSIA",
            colour = "BLACK", size = 9, hjust = 0.5,
            family = "Offside") +
  geom_text(x = 6.3, y = -85, label = "The 10 families of       found in Malaysia with the highest number of individual species",
            colour = "BLACK", size = 3, hjust = 0.5,
            family = "Rambla") +
  geom_text(x = 4.15, y = -105, label = "spider",
            colour = "BLACK", size = 3, hjust = 0.5, angle = 20,
            family = "Rambla") +
  geom_segment(x = 4.15, y = -85, xend = 4.15, yend = -103,
               linetype = "dashed", colour = "grey50",
               size = 0.3) +
  geom_segment(x = 4.05, y = -85, xend = 4.15, yend = -88,
               linetype = "dashed", colour = "grey50",
               size = 0.3) +
  geom_segment(x = 4.25, y = -85, xend = 4.15, yend = -88,
               linetype = "dashed", colour = "grey50",
               size = 0.3) +
  labs(x = NULL,
       y = NULL,
       caption = "@jamie_bio | source = World Spider Database") +
  scale_y_reverse() +
  theme(panel.background = element_rect(fill = "#F8F3EA"),
        plot.background = element_rect(fill = "#F8F3EA"),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 25,
                                  family = "Offside"),
        plot.subtitle = element_text(size = 10,
                                  family = "Rambla"),
        plot.caption = element_text(size = 5,
                                     family = "Offside")) +
  removeGrid()

# save plot
ggsave(paste0("spiders_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 5.5,
       height = 7)
  
