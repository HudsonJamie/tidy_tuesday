# lemurs.R
# Jamie Hudson
# Created: 24 August 2021
# Edited: 24 August 2021
# Data: Zehr et al. 2014. Photo from https://www.flickr.com/photos/vladimir_buynevich/31624248028

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(patchwork)
library(showtext)
library(png)
library(grid)
font_add_google("Raleway", "raleway")
font_add_google("Montserrat", "mont")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# load dataset ------------------------------------------------------------

lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

# wrangle and plot data ------------------------------------------------------------

(p1 <- lemurs %>% 
   group_by(dlc_id) %>% 
   slice(1) %>% 
   ungroup() %>% 
   filter(taxon %in% c("LTAR", "NCOU", "NPYG")) %>% 
   ggplot(aes(x = dam_age_at_concep_y, y = age_max_live_or_dead_y)) +
   geom_point(alpha = 0.6, size = 1.2, colour = "#474741", shape = 16) +
   stat_smooth(method = lm, se = F, colour = "#474741", size = 0.5) +
   labs(x = "Age of female parent at conception (Years)",
        y = "Maximum age of individual (Years)") +
   annotate(geom = "richtext", fill = NA, label.color = NA, family = "mont", 
            label = "<span style = 'color:#474741;'>All three species of loris (**n = 161**)</span> ",
            x = 8, y = 24, hjust = 0, size = 1.2) +
   theme(panel.grid = element_blank(),
         panel.background = element_rect(fill = "#E8E5DA"),
         plot.background = element_rect(fill = "#EAEAEA", colour = "#EAEAEA"),
         axis.ticks = element_line(size = 0.15, colour = "grey30"),
         axis.title = element_text(family = "mont",
                                   size = 3.5, colour = "grey30"),
         axis.text = element_text(family = "mont",
                                  size = 3, colour = "grey30"),
         axis.line = element_line(colour = "grey30", 
                                  size = 0.2, linetype = "solid"))
)

loris <- readPNG("loris.png", T)
r <- rasterGrob(loris, interpolate = TRUE)

(p2 <- lemurs %>% 
    group_by(dlc_id) %>% 
    slice(1) %>% 
    ungroup() %>% 
    filter(taxon %in% c("LTAR", "NCOU", "NPYG")) %>% 
    ggplot(aes(x = dam_age_at_concep_y, y = age_max_live_or_dead_y)) +
    geom_point(alpha = 0.6, size = 1.2,
               aes(colour = taxon,
                   shape = taxon)) +
    stat_smooth(method = lm, aes(colour = taxon), se = F, size = 0.5) +
    scale_colour_manual(values = c("#E3863E", "#405C40", "#A07C39")) +
    scale_shape_manual(values=c(15, 17, 18)) +
    labs(x = "Age of female parent at conception (Years)",
         y = "Maximum age of individual (Years)") +
    annotate(geom = "richtext", fill = NA, label.color = NA, family = "mont", 
             label = "<span style = 'color:#E3863E;'>Slender loris (*Loris tardigradus*, **n = 44)**</span>  \n<span style = 'color:#405C40;'>Slow loris (*Nycticebus coucang*, **n = 67**)</span>  \n<span style = 'color:#A07C39;'>Pygmy slow loris (*Nycticebus pygmaeus*, **n = 50**)</span>",
             x = 6, y = 23.5, hjust = 0, size = 1.2) +
    annotation_custom(r, xmin = 10, xmax = 15, ymin = 26, ymax = 40) +
    coord_cartesian(clip="off") +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "#E8E5DA"),
          plot.background = element_rect(fill = "#EAEAEA", colour = "#EAEAEA"),
          legend.position = "none",
          axis.ticks = element_line(size = 0.15, colour = "grey30"),
          axis.title = element_text(family = "mont",
                                    size = 3.5, colour = "grey30"),
          axis.text = element_text(family = "mont",
                                   size = 3, colour = "grey30"),
          axis.line = element_line(colour = "grey30", 
                                   size = 0.2, linetype = "solid"))
)

p1 + p2 +
  plot_annotation(title = "Lorises and the Simpson's Paradox at Duke Lemur Center",
                  subtitle = "Lorises are strepsirrhine primates, a group containing lemurs, bushbabies, and pottos, with two genera: *Loris* and *Nycticebus*.  \nThis dataset exhibits a nice example of **Simpson's Paradox**: a phenomenon whereby an association between two variables in a population emerges  \n(i.e. a negative relationship between age of female parents at conception and the maximum age the offspring reaches), but disappears or reverses  \nwhen the population is divided into subpopulations (i.e. species).  \n  \n <span style = 'color:#E2595B;'>*Fun Fact*: *Slow lorises secret a toxin from glands on their elbows, which is activated by mixing with saliva, making them one of world's few  \nvenemous mammals*</span>",
                  caption = "@jamie_bio | source: Zehr et al. (2014) Sci. Data | Photo courtesy of Vladimir Buynevich on Flickr (CC BY 2.0), www.nocturama.org",
                  theme = theme(panel.background = element_rect(fill = "#EAEAEA"),
                                plot.background = element_rect(fill = "#EAEAEA", colour = "#EAEAEA"),
                                plot.title = element_text(family = "raleway", size = 10, face = "bold", colour = "#46362A"),
                                plot.subtitle = element_markdown(family = "mont", size = 4, lineheight = 1.3, colour = "#46362A"),
                                plot.caption = element_text(family = "mont", size = 3, colour = "#46362A")))

ggsave(paste0("lemurs_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 5,
       height = 3)

