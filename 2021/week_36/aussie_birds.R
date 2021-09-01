# aussie_birds.R
# Jamie Hudson
# Created: 01 September 2021
# Edited: 01 September 2021
# Data: Cleary *et al*., (2016) PLOS ONE 11(3): e0150899

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(rgdal)
library(sf)
library(rmapshaper)
library(ggforce)
library(cowplot)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggtext)
library(showtext)

font_add_google("Pacifico", "pacifico")
font_add_google("Source source Pro", "source")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 36)
bird_baths <- tuesdata$bird_baths

# wrangle data ------------------------------------------------------------

bioregs <- bird_baths %>% distinct(bioregions) %>% as_tibble()

# Prepare shape file for map (downloaded from http://www.environment.gov.au/fed/catalog/main/home.page)
ibra61_shp <- st_read(dsn="data/ibra61_reg_shape/", layer="ibra61_reg_shape")
ibra61 <- st_transform(ibra61_shp, "+init=epsg:4326") 
ibra61 <- ms_simplify(ibra61) # Simplify as otherwise is massive and laptop gets sad

# Prepare australia outline
aus <- ne_countries(scale = "medium", returnclass = "sf", country = "Australia")

ibra_regs <- ibra61 %>% 
  filter(REG_NAME %in% bioregs$bioregions)

regs <- bioregs %>% filter(!bioregions %in% c("Brigalow Belt South", 
                                              "NSW South Western Slopes", 
                                              "Flinders Lofty Block",
                                              "Victorian Midlands",
                                              "Victorian Volcanic Plain",
                                              NA)) %>% pull(bioregions)

ibra_regs_season <- ibra61 %>% 
  filter(REG_NAME %in% regs)

# Produce list for mini barplots. Probably better way of doing this
# but this was for a discarded draft design and I don't have the
# energy to write something new.
mylist <- list()
for(i in regs) {
  mylist[[i]] <- bird_baths %>% 
    drop_na() %>% 
    filter(bioregions == i)  %>% 
    mutate(season = case_when(survey_year == 2014 ~ "winter",
                              survey_year == 2015 ~ "summer")) %>% 
    group_by(bird_type, season) %>% 
    filter(bird_count > 0) %>% 
    count(bird_count) %>% 
    ungroup() %>% 
    pivot_wider(names_from = season, values_from = n) %>% 
    mutate(colour = case_when(summer > winter ~ "orange",
                              TRUE ~ "blue"),
           summer = ifelse(is.na(summer), 0, summer),
           winter = ifelse(is.na(winter), 0, winter))
}

# plot ------------------------------------------------------------

# map
(map <- ggplot() +
   geom_sf(data = aus, fill = NA) +
   geom_sf(data=ibra_regs, fill = "#E1E3D4")+
   geom_sf(data=ibra_regs_season, fill = c("#B9C5CC", "#918DBA", "#9FB6CD", "#A4D3EE", "#A4D3EE", "#A4D3EE", "#B1D285", "#B1D285"))+
   xlab("Longitude") +
   xlim(135, 155)+
   ylim(-40, -22) +
   ylab("Latitude") + 
   theme_bw() +
   theme(plot.background = element_rect(fill = "#FAFAFA"),
         panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"),
         axis.title = element_blank(),
         axis.text = element_blank(),
         panel.grid = element_blank(),
         axis.ticks = element_blank(),
         panel.border = element_blank())
)

# Circles
SEQ_circle <- ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2),
              colour = "#B1D285", size = 0.8, fill = "grey99") +
  coord_fixed() +
  theme_void()

NNC_circle <- ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2),
              colour = "#B9C5CC", size = 0.8, fill = "grey99") +
  coord_fixed() +
  theme_void()

SYB_circle <- ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2),
              colour = "#918DBA", size = 0.8, fill = "grey99") +
  coord_fixed() +
  theme_void()

SEH_circle <- ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2),
              colour = "#A4D3EE", size = 0.8, fill = "grey99") +
  coord_fixed() +
  theme_void()

SCP_circle <- ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2),
              colour = "#9FB6CD", size = 0.8, fill = "grey99") +
  coord_fixed() +
  theme_void()

# mini barplots
SEQ <- mylist[[1]] %>% 
  count(colour) %>% 
  ggplot(aes(x = colour, y = n, fill = colour)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("W","S")) +
  scale_fill_manual(values = c("#597a2d", "#cce2b0")) +
  scale_y_continuous(limits = c(0, 115),
                     labels = c(0, 50, 100),
                     breaks = c(0, 50, 100)) +
  annotate("text", label = "SEQ",
           x = 2, y = 100, size = 3, fontface = "bold", 
           colour = "#B1D285", family = "source") +
  theme_bw() +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(family = "source",
                                 size = 5),
        axis.ticks = element_line(size = 0.2))


NNC <- mylist[[2]] %>% 
  count(colour) %>% 
  ggplot(aes(x = colour, y = n, fill = colour)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("W","S")) +
  scale_fill_manual(values = c("#495a64", "#dbe1e5")) +
  scale_y_continuous(limits = c(0, 115),
                     labels = c(0, 50, 100),
                     breaks = c(0, 50, 100)) +
  annotate("text", label = "NNC",
           x = 2, y = 100, size = 3, fontface = "bold",
           colour = "#B9C5CC", family = "source") +
  theme_bw() +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(family = "source",
                                 size = 5),
        axis.ticks = element_line(size = 0.2))


SYB <- mylist[[3]] %>% 
  count(colour) %>% 
  ggplot(aes(x = colour, y = n, fill = colour)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("W","S")) +
  scale_fill_manual(values = c("#44406a", "#b4b2d0")) +
  scale_y_continuous(limits = c(0, 115),
                     labels = c(0, 50, 100),
                     breaks = c(0, 50, 100)) +
  annotate("text", label = "SYB",
           x = 2, y = 100, size = 3, fontface = "bold",
           colour = "#918DBA", family = "source") +
  theme_bw() +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(family = "source",
                                 size = 5),
        axis.ticks = element_line(size = 0.2))


SEH <- mylist[[4]] %>% 
  count(colour) %>% 
  ggplot(aes(x = colour, y = n, fill = colour)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("W","S")) +
  scale_fill_manual(values = c("#1a638c", "#b5dbf1")) +
  scale_y_continuous(limits = c(0, 115),
                     labels = c(0, 50, 100),
                     breaks = c(0, 50, 100)) +
  annotate("text", label = "SEH",
           x = 2, y = 100, size = 3, fontface = "bold",
           colour = "#A4D3EE", family = "source") +
  theme_bw() +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(family = "source",
                                 size = 5),
        axis.ticks = element_line(size = 0.2))


SCP <- mylist[[5]] %>% 
  count(colour) %>% 
  ggplot(aes(x = colour, y = n, fill = colour)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("W","S")) +
  scale_fill_manual(values = c("#39546e", "#c6d3e1")) +
  scale_y_continuous(limits = c(0, 115),
                     labels = c(0, 50, 100),
                     breaks = c(0, 50, 100)) +
  annotate("text", label = "SCP",
           x = 2, y = 100, size = 3, fontface = "bold",
           colour = "#9FB6CD", family = "source") +
  theme_bw() +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(family = "source",
                                 size = 5),
        axis.ticks = element_line(size = 0.2))

# bring map together
maps_2 <- ggdraw(xlim = c(5, 35), ylim = c(-7, 30)) +
  draw_plot(map, x = 0, y = 0, width = 20, height = 20) +
  geom_polygon(aes(x = c(13, 8.5, 12.4), y = c(15.5, 20.5, 24.6)),
               fill = "#B1D285", alpha = 0.6) +
  geom_polygon(aes(x = c(13.3, 13.6, 17.85), y = c(10.2, 23, 19.5)),
               fill = "#B9C5CC", alpha = 0.6) +
  geom_polygon(aes(x = c(12.7, 17.2, 19), y = c(8, 17.4, 9.7)),
               fill = "#918DBA", alpha = 0.6) +
  geom_polygon(aes(x = c(12, 16, 14.2), y = c(5.5, 7.8, 1)),
               fill = "#A4D3EE", alpha = 0.6) +
  geom_polygon(aes(x = c(10.8, 9.7, 12.6), y = c(3, -0.8, 2.5)),
               fill = "#9FB6CD", alpha = 0.6) +
  draw_plot(SEQ_circle, x = 5, y = 19.3, width = 10, height = 10) +
  draw_plot(NNC_circle, x = 11, y = 17.5, width = 10, height = 10) +
  draw_plot(SYB_circle, x = 14, y = 9.3, width = 10, height = 10) +
  draw_plot(SEH_circle, x = 11.2, y = -1.7, width = 10, height = 10) +
  draw_plot(SCP_circle, x = 7, y = -7, width = 10, height = 10) +
  draw_plot(SEQ, x = 8, y = 20, width = 4, height = 8) +
  draw_plot(NNC, x = 14, y = 18.2, width = 4, height = 8) +
  draw_plot(SYB, x = 17, y = 10, width = 4, height = 8) +
  draw_plot(SEH, x = 14.2, y = -0.7, width = 4, height = 8) +
  draw_plot(SCP, x = 10, y = -6.3, width = 4, height = 8) +
  annotate(geom = "richtext", label = "The barplot above displays the number of birds  \nspotted in rural and urban environments during the  \n2015 austral winter. The **<span style = 'color:#B1D285;'>South Eastern Queensland  \n(SEQ)</span>** bioregion had the highest number of birds spotted  \nacross environments, though more birds were spotted in  \nthe **<span style = 'color:#A4D3EE;'>South Eastern Highlands (SEH)</span>** rural environments,  \nand in **<span style = 'color:#918DBA;'>Sydney Basin (SYB)</span>** urban environments.  \nWhilst the Victorian Midlands (VIM) had the second  \nhighest reported number of **different** birds (54) and  \n**total** identified birds (191) in rural regions, it had the  \nlowest number of **different** (18) and **total** (26) birds  \identified in urban environments. <br><br>The map to the left shows the studied bioregions  \nwithin Australia. Only five bioregions were sampled  \nduring both seasons, and the number of **different**  \nbird species identified during winter (darker colour)  \nand summer (lighter colour) are displayed. The  \n**<span style = 'color:#A4D3EE;'>South Eastern Highlands (SEH)</span>** is the only  \nbioregion with a similar number of bird species  \nidentified during both seasons.",
           x = 22, y = 12, size = 3.2, hjust = 0, fill = "#FAFAFA",
           lineheight = 1.6, label.color = NA, family = "source") +
  annotate(geom = "text", label = "FLB", x = 8, y = 10, size = 3, hjust = 0, family = "source") +
  annotate(geom = "text", label = "VIM", x = 8.5, y = 7, size = 3, hjust = 0, family = "source") +
  annotate(geom = "text", label = "BBS", x = 10, y = 13.5, size = 3, hjust = 0, family = "source") +
  annotate(geom = "text", label = "NSS", x = 9.8, y = 9.3, size = 3, hjust = 0, family = "source") +
  annotate(geom = "text", label = "VVP", x = 7.8, y = 5.5, size = 3, hjust = 0, family = "source") +
  annotate("curve", x = 7.9, xend = 7.3, y = 9.8, yend = 8.5, 
           colour = "black", size = 0.4, arrow = arrow(length = unit(0.15, "cm")),
           curvature = 0.1) +
  annotate("curve", x = 9, xend = 9.3, y = 6.5, yend = 5.5, 
           colour = "black", size = 0.4, arrow = arrow(length = unit(0.15, "cm")),
           curvature = 0.1) +
  annotate("curve", x = 8.3, xend = 9.35, y = 5, yend = 3.5, 
           colour = "black", size = 0.4, arrow = arrow(length = unit(0.15, "cm")),
           curvature = 0.1) +
  annotate("curve", x = 10.4, xend = 11.2, y = 8.5, yend = 7, 
           colour = "black", size = 0.4, arrow = arrow(length = unit(0.15, "cm")),
           curvature = 0.1) +
  annotate("curve", x = 10.6, xend = 12.3, y = 14, yend = 15, 
           colour = "black", size = 0.4, arrow = arrow(length = unit(0.15, "cm")),
           curvature = -0.1)

# Bar plot
(bar <- bird_baths %>%
    mutate(reg = case_when(bioregions == "South Eastern Queensland" ~ "SEQ",
                           bioregions == "NSW North Coast" ~ "NNC",
                           bioregions == "Sydney Basin" ~ "SYB",
                           bioregions == "South Eastern Highlands" ~ "SEH",
                           bioregions == "South East Coastal Plain" ~ "SCP",
                           bioregions == "Brigalow Belt South" ~ "BBS",
                           bioregions == "NSW South Western Slopes" ~ "NSS",
                           bioregions == "Victorian Volcanic Plain" ~ "VVP",
                           bioregions == "Victorian Midlands" ~ "VIM",
                           bioregions == "Flinders Lofty Block" ~ "FLB")) %>% 
    group_by(reg, urban_rural) %>% 
    drop_na() %>% 
    filter(bird_count > 0,
           survey_year == 2015) %>%
    summarise(birds = n_distinct(bird_type),
              n = n()) %>% 
    mutate(n = case_when(urban_rural == "Urban" ~ -n,
                         TRUE ~ n)) %>% 
    ggplot(aes(x = reg, y = n, fill = urban_rural,
               width = birds/100)) +
    geom_bar(stat = "identity", position = "identity") +
    scale_fill_manual(values = c("#CFAA3D", "#85898C")) +
    annotate(geom = "text", label = "Rural", fontface = "bold",
             colour = "#CFAA3D", x = 1, y = 150,  size = 5, family = "source") +
    annotate(geom = "text", label = "Urban", fontface = "bold",
             colour = "#85898C", x = 1, y = -100,  size = 5, family = "source") +
    annotate(geom = "text", label = "Width of bars are \nproportional to number of \ndifferent bird species identified",
             x = 9.3, y = -200, size = 2.5, family = "source") +
    geom_curve(x = 8.7, y = -140, xend = 8.4, yend = -90,
               arrow = arrow(length = unit(0.25, "cm")), size = 0.2,
               curvature = 0.1) +
    geom_curve(x = 9.5, y = -120, xend = 9.8, yend = -70,
               arrow = arrow(length = unit(0.25, "cm")), size = 0.2,
               curvature = -0.1) +
    geom_hline(yintercept = 0) +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          panel.grid = element_blank(),
          panel.grid.minor.y = element_line(linetype = "dashed", size = 0.1, colour = "grey50"),
          panel.grid.major.y = element_line(linetype = "dashed", size = 0.1, colour = "grey50"),
          axis.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(colour = c("black", "black", "#B9C5CC", "black",
                                                "#9FB6CD", "#A4D3EE", "#B1D285", "#918DBA",
                                                "black", "black"), 
                                     face = c("plain", "plain", "bold", "plain",
                                              "bold", "bold", "bold", "bold",
                                              "plain", "plain"),
                                     family = "source"),
          axis.text.y = element_text(family = "source", colour = "black"),
          legend.position = "none")
)

# Bring altogether
layout <- c(
  area(t = 1, l = 1, b = 2, r = 12
  ),
  area(t = 3, l = 1, b = 8, r = 12
  ))

# plot(layout)

bar + maps_2 + 
  plot_layout(design = layout) +
  plot_annotation(title = 'Bird baths of Australia',
                  subtitle = "Researchers at Deakin University and Griffith University led a team of 2,500 citizen scientists to collect bird occurrence data  \nfrom bird baths during two four week periods (one during austral winter 2014, and the other during austral summer 2015).",
                  caption = "@jamie_bio | source: Cleary *et al*., (2016) PLOS ONE 11(3): e0150899",
                  theme = theme(panel.background = element_rect(fill = "#FAFAFA"),
                                plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"),
                                plot.title = element_text(size = 28,
                                                          colour = "black", family = "pacifico",
                                                          face = "bold"),
                                plot.subtitle = element_markdown(family = "source", size = 9.5, 
                                                                 colour = "black",
                                                                 lineheight = 1.2),
                                plot.caption = element_markdown(family = "source", size = 7,
                                                            colour = "black")))

ggsave(paste0("aus_birds_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 8,
       height = 8)

