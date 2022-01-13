# bee_colonies.R
# Jamie Hudson
# Created: 12 Jan 2022
# Edited: 12 Jan 2022
# Data: USDA

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(cowplot)
library(ggforce)
library(patchwork)
library(usdata)
library(png)
library(grid)
library(showtext)
font_add_google("Raleway Dots")
showtext_opts(dpi = 320)
showtext_auto()


# load dataset ------------------------------------------------------------

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

# wrangle data ------------------------------------------------------------

join_df <- colony %>% full_join(stressor) %>% 
  mutate(stressor_cut = cut(stress_pct, 
                            breaks=c(0, 10, 20, 30, 40, 50, 150), 
                            include.lowest=TRUE, 
                            right=FALSE, 
                            labels=c("10", "20", "30", "40", "50", "60"))) %>% 
  pivot_wider(names_from = stressor, values_from = stressor_cut)

join_df <- colony %>% full_join(stressor) %>%
  pivot_wider(names_from = stressor, values_from = stress_pct)

join_df_17 <- join_df %>% 
  filter(state %in% c("California", "Colorado", "Iowa", "Michigan", "New York", "Maine"),
         year == 2017) %>% 
  janitor::clean_names() %>% 
  mutate(state_abb = state2abbr(state),
         state_abb_num = as.numeric(factor(state_abb, levels = c("CA", "CO", "IA", "MI", "NY", "ME"))))


# plot ------------------------------------------------------------

purd <- RColorBrewer::brewer.pal(6, 'PuRd')

bee <- readPNG("img/bee.png", T)
bee_r <- rasterGrob(bee, interpolate = TRUE)

(legend <- ggplot() +
    geom_hline(aes(yintercept = 0), colour = "tan4") +
    annotate("curve", x = 1, xend = 1, y = 1 + 10, yend = -10 + 10,
             colour = "forestgreen", size = 3,
             curvature = -0.05) +
  geom_regon(aes(x0 = 1, y0 = 1 + 10, sides = 6, angle = pi/2, r = 1.5),
             fill = "goldenrod1") +
    geom_regon(aes(x0 = -2.75, y0 = 15, sides = 6, angle = pi/2, r = 0.65),
               fill = purd[1]) +
    geom_regon(aes(x0 = -1.5, y0 = 15, sides = 6, angle = pi/2, r = 0.65),
               fill = purd[2]) +
    geom_regon(aes(x0 = -0.25, y0 = 15, sides = 6, angle = pi/2, r = 0.65),
               fill = purd[3]) +
    geom_regon(aes(x0 = 1, y0 = 15, sides = 6, angle = pi/2, r = 0.65),
               fill = purd[4]) +
    geom_regon(aes(x0 = 2.25, y0 = 15, sides = 6, angle = pi/2, r = 0.65),
               fill = purd[5]) +
    geom_regon(aes(x0 = 3.5, y0 = 15, sides = 6, angle = pi/2, r = 0.65),
               fill = purd[6]) +
    geom_regon(aes(x0 = 4.75, y0 = 15, sides = 6, angle = pi/2, r = 0.65),
               fill = "grey50") +
    annotate(geom = "text", label = c("<10%", "10-20%", "20-30%", "30-40%", "40-50%", ">50%", "NA"),
             x = c(-2.75, -1.5, -0.25, 1, 2.25, 3.5, 4.75), y = 15, size = 2, hjust = 0.5, family = "mono", colour = c(rep("black", 4), rep("white", 3))) +
    annotate(geom = "text", label = "% of colonies affected by stressor",
             x = 1, y = 16, size = 3, hjust = 0.5, family = "mono", colour = "black") +
  geom_regon(aes(x0 = 1.866, y0 = 2.5 + 10, sides = 6, angle = pi/2, r = 1),
             fill = "white", colour = "black") +
    annotate(geom = "text", label = "Varroa\nmites",
             x = 1.866, y = 2.5 + 10, size = 1.9, hjust = 0.5, family = "mono", colour = "black") +
  geom_regon(aes(x0 = 2.732, y0 = 1 + 10, sides = 6, angle = pi/2, r = 1),
             fill = "white", colour = "black") +
    annotate(geom = "text", label = "Other\nparasites",
             x = 2.732, y = 1 + 10, size = 1.9, hjust = 0.5, family = "mono", colour = "black") +
  geom_regon(aes(x0 = 1.866, y0 = -0.5 + 10, sides = 6, angle = pi/2, r = 1),
             fill = "white", colour = "black") +
    annotate(geom = "text", label = "Diseases",
             x = 1.866, y = -0.5 + 10, size = 1.9, hjust = 0.5, family = "mono", colour = "black") +
  geom_regon(aes(x0 = 0.134, y0 = -0.5 + 10, sides = 6, angle = pi/2, r = 1),
             fill = "white", colour = "black") +
    annotate(geom = "text", label = "Pesticides",
             x = 0.134, y = -0.5 + 10, size = 1.9, hjust = 0.5, family = "mono", colour = "black") +
  geom_regon(aes(x0 = -0.732, y0 = 1 + 10, sides = 6, angle = pi/2, r = 1),
             fill = "white", colour = "black") +
    annotate(geom = "text", label = "Other",
             x = -0.732, y = 1 + 10, size = 1.9, hjust = 0.5, family = "mono", colour = "black") +
  geom_regon(aes(x0 = 0.134, y0 = 2.5 + 10, sides = 6, angle = pi/2, r = 1),
             fill = "white", colour = "black") +
    annotate(geom = "text", label = "Unknown",
             x = 0.134, y = 2.5 + 10, size = 1.9, hjust = 0.5, family = "mono", colour = "black") +
  annotate(geom = "text", label = "State\nname",
           x = 1, y = 11, size = 3, hjust = 0.5, family = "mono", colour = "black") +
    geom_segment(aes(x = 5, xend = 5, y = 0, yend = 11), arrow = arrow(length = unit(0.3,"cm"),
                                                                       ends = "both")) +
    annotate(geom = "text", label = "Height to centre of flower = % colony lost",
             x = 4.6, y = 5.5, size = 2.5, hjust = 0.5, angle = 90,
             family = "mono", colour = "black") +
    annotate(geom = "text", label = "Petals represent\ndifferent stressors",
             x = -1, y = 7, size = 2.5, hjust = 0.5, family = "mono", colour = "black") +
    geom_curve(aes(x = -0.732, xend = 0, y = 7.5, yend = 9),
               colour = "black", size = 0.5, arrow = arrow(length = unit(0.15, "cm")),
               curvature = -0.15) +  
    annotation_custom(bee_r, xmin = 3.5, xmax = 7.5, ymin = 16, ymax = 18.4) +
    lims(x = c(-3.5, 5.5)) +
  coord_fixed(clip = "off") +
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_blank(),
          panel.background = element_rect(fill = "#EECFC0", colour = "grey20",
                                          linetype = "dashed"))
)

(p1 <- join_df_17 %>% 
    filter(months == "January-March") %>% 
    ggplot() +
    geom_hline(aes(yintercept = 0), colour = "tan4") +
  geom_curve(aes(x = (state_abb_num * 10), xend = (state_abb_num * 10), y = colony_lost_pct, yend = 0),
             colour = "forestgreen", size = 1.5,
             curvature = -0.05) +
  geom_regon(aes(x0 = (state_abb_num * 10), y0 = colony_lost_pct, sides = 6, angle = pi/2, r = 1.5),
             fill = "orange", colour = "black", size = 0.3) +
  geom_regon(aes(x0 = (state_abb_num * 10) + 1.299, y0 = 2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                 fill = varroa_mites), colour = "black", size = 0.3) +
  geom_regon(aes(x0 = (state_abb_num * 10) + 1.299*2, y0 = colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                 fill = other_pests_parasites), colour = "black", size = 0.3) +
  geom_regon(aes(x0 = (state_abb_num * 10) + 1.299, y0 = -2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                 fill = disesases), colour = "black", size = 0.3) +
  geom_regon(aes(x0 = (state_abb_num * 10) - 1.299, y0 = -2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                 fill = pesticides), colour = "black", size = 0.3) +
  geom_regon(aes(x0 = (state_abb_num * 10) - 1.299*2, y0 = colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                 fill = other), colour = "black", size = 0.3) +
  geom_regon(aes(x0 = (state_abb_num * 10) - 1.299, y0 = 2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                 fill = unknown), colour = "black", size = 0.3) +
  geom_text(aes(x = (state_abb_num * 10), y = colony_lost_pct, label = state_abb),
            size = 2.5, hjust = 0.5, family = "mono", colour = "black") +
    geom_text(aes(x = 10, y = -2, label = "South West"),
              size = 3, hjust = 0.5, family = "mono", colour = "black") +
    geom_text(aes(x = 60, y = -2, label = "North East"),
              size = 3, hjust = 0.5, family = "mono", colour = "black") +
    geom_text(aes(x = 7, y = 27), label = "January - March 2017",
              size = 3.5, hjust = 0, family = "mono", colour = "grey20") +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       limits = c(-2.8, 35.8)) +
    scale_fill_fermenter(limits = c(0,60),
                         breaks = c(0,10,20,30,40,50,60), palette = "PuRd",
                         direction = 1, na.value = "grey50") +
    lims(x = c(6.1, 64)) +
  coord_fixed() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_text(family = "mono", colour = "black",
                                     size = 7),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(linetype = "dashed", size = 0.2,
                                            colour = "black"),
          plot.background = element_blank(),
          panel.background = element_rect(fill = "#EECFC0"),
          title = element_text(size = 8))
)

(p2 <- join_df_17 %>% 
  filter(months == "April-June") %>% 
  ggplot() +
    geom_hline(aes(yintercept = 0), colour = "tan4") +
  geom_curve(aes(x = (state_abb_num * 10), xend = (state_abb_num * 10), y = colony_lost_pct, yend = 0),
             colour = "forestgreen", size = 1,
             curvature = -0.05) +
    geom_regon(aes(x0 = (state_abb_num * 10), y0 = colony_lost_pct, sides = 6, angle = pi/2, r = 1.5),
               fill = "orange", colour = "black", size = 0.3) +
    geom_regon(aes(x0 = (state_abb_num * 10) + 1.299, y0 = 2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                   fill = varroa_mites), colour = "black", size = 0.3) +
    geom_regon(aes(x0 = (state_abb_num * 10) + 1.299*2, y0 = colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                   fill = other_pests_parasites), colour = "black", size = 0.3) +
    geom_regon(aes(x0 = (state_abb_num * 10) + 1.299, y0 = -2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                   fill = disesases), colour = "black", size = 0.3) +
    geom_regon(aes(x0 = (state_abb_num * 10) - 1.299, y0 = -2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                   fill = pesticides), colour = "black", size = 0.3) +
    geom_regon(aes(x0 = (state_abb_num * 10) - 1.299*2, y0 = colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                   fill = other), colour = "black", size = 0.3) +
    geom_regon(aes(x0 = (state_abb_num * 10) - 1.299, y0 = 2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                   fill = unknown), colour = "black", size = 0.3) +
    geom_text(aes(x = (state_abb_num * 10), y = colony_lost_pct, label = state_abb),
              size = 2.5, hjust = 0.5, family = "mono", colour = "black") +
    geom_text(aes(x = 7, y = 27), label = "April - June 2017",
              size = 3.5, hjust = 0, family = "mono", colour = "grey20") +
    scale_fill_fermenter(limits = c(0,60),
                         breaks = c(0,10,20,30,40,50,60), palette = "PuRd",
                         direction = 1) +
    lims(y = c(-2.8, 35.8),
         x = c(6.1, 64)) +
    coord_fixed() +
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(linetype = "dashed", size = 0.2,
                                            colour = "black"),
          plot.background = element_blank(),
          panel.background = element_rect(fill = "#EECFC0"),
          title = element_text(size = 8))
)

(p3 <- join_df_17 %>% 
  filter(months == "July-September") %>% 
  ggplot() +
    geom_hline(aes(yintercept = 0), colour = "tan4") +
  geom_curve(aes(x = (state_abb_num * 10), xend = (state_abb_num * 10), y = colony_lost_pct, yend = 0),
             colour = "forestgreen", size = 1,
             curvature = -0.05) +
    geom_regon(aes(x0 = (state_abb_num * 10), y0 = colony_lost_pct, sides = 6, angle = pi/2, r = 1.5),
               fill = "orange", colour = "black", size = 0.3) +
    geom_regon(aes(x0 = (state_abb_num * 10) + 1.299, y0 = 2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                   fill = varroa_mites), colour = "black", size = 0.3) +
    geom_regon(aes(x0 = (state_abb_num * 10) + 1.299*2, y0 = colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                   fill = other_pests_parasites), colour = "black", size = 0.3) +
    geom_regon(aes(x0 = (state_abb_num * 10) + 1.299, y0 = -2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                   fill = disesases), colour = "black", size = 0.3) +
    geom_regon(aes(x0 = (state_abb_num * 10) - 1.299, y0 = -2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                   fill = pesticides), colour = "black", size = 0.3) +
    geom_regon(aes(x0 = (state_abb_num * 10) - 1.299*2, y0 = colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                   fill = other), colour = "black", size = 0.3) +
    geom_regon(aes(x0 = (state_abb_num * 10) - 1.299, y0 = 2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                   fill = unknown), colour = "black", size = 0.3) +
    geom_text(aes(x = (state_abb_num * 10), y = colony_lost_pct, label = state_abb),
              size = 2.5, hjust = 0.5, family = "mono", colour = "black") +
    geom_text(aes(x = 7, y = 27), label = "July - September 2017",
              size = 3.5, hjust = 0, family = "mono", colour = "grey20") +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       limits = c(-2.8, 35.8)) +
    scale_fill_fermenter(limits = c(0,60),
                         breaks = c(0,10,20,30,40,50,60), palette = "PuRd",
                         direction = 1, na.value = "grey50") +
    lims(x = c(6.1, 64)) +
    coord_fixed() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_text(family = "mono", colour = "black",
                                     size = 7),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(linetype = "dashed", size = 0.2,
                                            colour = "black"),
          plot.background = element_blank(),
          panel.background = element_rect(fill = "#EECFC0"),
          title = element_text(size = 8))
)

(p4 <- join_df_17 %>% 
  filter(months == "October-December") %>% 
  ggplot() +
    geom_hline(aes(yintercept = 0), colour = "tan4") +
  geom_curve(aes(x = (state_abb_num * 10), xend = (state_abb_num * 10), y = colony_lost_pct, yend = 0),
             colour = "forestgreen", size = 1,
             curvature = -0.05) +
  geom_regon(aes(x0 = (state_abb_num * 10), y0 = colony_lost_pct, sides = 6, angle = pi/2, r = 1.5),
             fill = "orange", colour = "black", size = 0.3) +
  geom_regon(aes(x0 = (state_abb_num * 10) + 1.299, y0 = 2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                 fill = varroa_mites), colour = "black", size = 0.3) +
  geom_regon(aes(x0 = (state_abb_num * 10) + 1.299*2, y0 = colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                 fill = other_pests_parasites), colour = "black", size = 0.3) +
  geom_regon(aes(x0 = (state_abb_num * 10) + 1.299, y0 = -2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                 fill = disesases), colour = "black", size = 0.3) +
  geom_regon(aes(x0 = (state_abb_num * 10) - 1.299, y0 = -2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                 fill = pesticides), colour = "black", size = 0.3) +
  geom_regon(aes(x0 = (state_abb_num * 10) - 1.299*2, y0 = colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                 fill = other), colour = "black", size = 0.3) +
  geom_regon(aes(x0 = (state_abb_num * 10) - 1.299, y0 = 2.25 + colony_lost_pct, sides = 6, angle = pi/2, r = 1.5,
                 fill = unknown), colour = "black", size = 0.3) +
  geom_text(aes(x = (state_abb_num * 10), y = colony_lost_pct, label = state_abb),
            size = 2.5, hjust = 0.5, family = "mono", colour = "black") +
    geom_text(aes(x = 7, y = 27), label = "October - December 2017",
              size = 3.5, hjust = 0, family = "mono", colour = "grey20") +
  scale_fill_fermenter(limits = c(0,60),
                       breaks = c(0,10,20,30,40,50,60), palette = "PuRd",
                       direction = 1) +
    lims(y = c(-2.8, 35.8),
         x = c(6.1, 64)) +
  coord_fixed() +
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(linetype = "dashed", size = 0.2,
                                            colour = "black"),
          plot.background = element_blank(),
          panel.background = element_rect(fill = "#EECFC0"),
          title = element_text(size = 8))
)

layout <- c(
  area(t = 1, l = 1, b = 3, r = 7
  ),
  area(t = 1, l = 8, b = 3, r = 14
  ),
  area(t = 4, l = 1, b = 6, r = 7
  ),
  area(t = 4, l = 8, b = 6, r = 14
  ),
  area(t = 1, l = 15, b = 6, r = 20
  ))

plot(layout)

(p1 + p2 + p3 + p4) + 
  legend +
  plot_layout(design = layout) + 
  plot_annotation(title = "Honey Bee Colony Loss in the US",
                  subtitle = "Bee colonies within six states across the US exhibited varied responses to stressors in 2017.",
                  caption = "@jamie_bio | source: USDA",
                  theme = theme(plot.title = element_text(size = 27, hjust = 0.5, 
                                                          colour = "black", family = "mono"),
                                plot.subtitle = element_text(family = "mono", size = 11, colour = "grey20", hjust = 0.5,
                                                             lineheight = 1.2),
                                plot.caption = element_text(family = "mono", size = 7,
                                                            colour = "grey20"),
                                plot.background = element_rect(fill = "#EECFC0", colour = "#EECFC0"),
                                panel.background = element_rect(fill = "#EECFC0", colour = "#EECFC0")))

ggsave(
  paste0("bee_colonies_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 11,
  height = 6
)
