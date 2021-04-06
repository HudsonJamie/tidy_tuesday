
# load libraries ----------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(gghighlight)
library(patchwork)
library(scales)
library(ggtext)
library(pdftools)


# load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 15)

forest <- tuesdata$forest
forest_area <- tuesdata$forest_area
brazil_loss <- tuesdata$brazil_loss
soybean_use <- tuesdata$soybean_use
vegetable_oil <- tuesdata$vegetable_oil

palm <- vegetable_oil %>% 
  filter(crop_oil %in% c("Palm", "Palm kernel")) %>% 
  na.omit()

# produce oil palm schematic ----------------------------------------------

df <- tribble(
  ~a, ~b,
  1, 45,
  2, 5,
  3, 20
)  

df$a <- as.character(df$a)

(full_palm <- ggplot(df, aes(x = 1, y = b, fill = a)) +
  geom_bar(position="stack", stat="identity") +
  coord_polar("x") +
  scale_fill_manual(values = c("#EF8421", "#553727", "#D1D5C9")) +
  geom_text(aes(x = 0.68, y = 100, label = "Oil palm \nfruit"),
      colour = "#800020",
      size = 4) +
  geom_text(aes(x = 1.2, y = 110, label = "Oil palm \nkernel"),
      colour = "#800020",
      size = 4) +
  geom_text(aes(x = 1, y = 120, label = "Oil can be obtained from both \nthe fruit and the seed kernel"),
      colour = "#800020",
      size = 4.8,
      hjust = 0.5) +
  geom_segment(aes(x = 0.66, y = 79, xend = 0.65, yend = 45),
                 size = 0.8,
                 arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = 1.2, y = 75, xend = 1.1, yend = 0.4),
                 size = 0.8,
                 arrow = arrow(length = unit(0.3, "cm"))) +
  theme_void() +
  theme(legend.position = "none",
  plot.subtitle = element_markdown(
    size = 16,
    colour = "#800020",
    family = "sans",
    hjust = 0.5
  )))

# palm oil - fruit --------------------------------------------------------

(fruit_line <- palm %>% 
  filter(crop_oil == "Palm",
         entity != "World") %>% 
  ggplot(aes(x = year, y = production, colour = entity)) +
  scale_colour_manual(values = c("#EF8421", "#F05E38")) +
  geom_line(size = 1) +
  ylab("Oil production (tonnes)") +
   xlab("Year") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none") +
  gghighlight(max(production) > 10000000) +
  annotate(geom = "richtext", label = "<span style='color: grey60;'>All other  \ncountries</span>",
            x = 2010, y = 5000000,
           color = "grey60") +
  theme_bw() +
  theme(text = element_text(
    colour = "#800020"
  ),
  axis.text = element_text(
    colour = "#800020"
  ),
  panel.border = element_rect(fill = NA,colour = "#800020")))


(fruit_palm <- ggplot(df, aes(x = 1, y = b, fill = a, alpha = a)) +
  geom_bar(position="stack", stat="identity") +
  coord_polar("x") +
  scale_fill_manual(values = c("#EF8421", "#553727", "#D1D5C9")) +
  scale_alpha_manual(values=c(1,0.2,0.2)) + 
  theme_void() +
  theme(legend.position = "none"))

fruit_plots <- fruit_line + 
  inset_element(fruit_palm, left = 0.0, right = 0.3, bottom = 0.65, top = 0.95)

fruit_plots$patches$layout$widths  <- 1
fruit_plots$patches$layout$heights <- 1

# palm oil - kernel -------------------------------------------------------

(kernel_line <- palm %>% 
  filter(crop_oil == "Palm kernel",
         entity != "World") %>% 
  ggplot(aes(x = year, y = production, colour = entity)) +
  scale_colour_manual(values = c("#E6C788", "#F1B345")) +
  geom_line(size = 1) +
  ylab("Oil production (tonnes)") +
   xlab("Year") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none") +
  gghighlight(max(production) > 1000000) +
  annotate(geom = "richtext", label = "<span style='color: grey60;'>All other  \ncountries</span>",
           x = 2010, y = 600000,
           color = "grey60") +
  theme_bw() +
   theme(text = element_text(
     colour = "#800020"
   ),
   axis.text = element_text(
     colour = "#800020"
   ),
   panel.border = element_rect(fill = NA,colour = "#800020")))

kernel_palm <- ggplot(df, aes(x = 1, y = b, fill = a, alpha = a)) +
  geom_bar(position="stack", stat="identity") +
  coord_polar("x") +
  scale_fill_manual(values = c("#EF8421", "#553727", "#D1D5C9")) +
  scale_alpha_manual(values=c(0.2,1,1)) + 
  theme_void() +
  theme(legend.position = "none")

kernel_plots <- kernel_line + 
  inset_element(kernel_palm, left = 0.0, right = 0.3, bottom = 0.65, top = 0.95)

kernel_plots$patches$layout$widths  <- 1
kernel_plots$patches$layout$heights <- 1

# ratio plot --------------------------------------------------------------

(ratio_line <- ratio %>% 
   ggplot(aes(x = year, y = ratio, colour = entity)) +
   geom_line(size = 1) +
   scale_colour_manual(values = c("#EF8421", "#F05E38")) +
   ylab("Ratio of fruit to \nkernel oil production ") +
   xlab("Year") +
   scale_y_continuous(labels = comma) +  
   gghighlight() +
   geom_text(aes(x = 1998.35, y = 41, label = ":"),
             size = 15,
             colour = "#800020") +  
   theme_bw() +  
   theme(legend.position = "none",
         text = element_text(
           colour = "#800020"
         ),
         axis.text = element_text(
           colour = "#800020"
         ),
         panel.border = element_rect(fill = NA,colour = "#800020")))

ratio_plots <- ratio_line + 
  inset_element(kernel_palm, left = 0.65, right = 0.95, bottom = 0.65, top = 0.95) +
  inset_element(fruit_palm, left = 0.43, right = 0.73, bottom = 0.65, top = 0.95)

ratio_plots$patches$layout$widths  <- 1
ratio_plots$patches$layout$heights <- 1

# bring plots together ----------------------------------------------------------

(wrap_plots(fruit_plots, kernel_plots, ratio_plots, full_palm) +
  plot_layout(widths = c(1,1)) +
  plot_annotation(
    title = 'Production of oil from the oil palm *Elaeis guineensis*',
    subtitle = 'Indonesia and Malaysia are head and shoulders above all other countries in their production of oil from both the *Elaeis guineensis* fruit 
    <br>and *Elaeis guineensis* seed kernels',
       caption = "@jamie_bio | source: Our World in Data",
 theme = theme(
  plot.title = element_markdown(
    size = 25,
    colour = "#800020",
    family = "sans",
    margin = margin(10, 0, 0, 0)
  ),
  plot.subtitle = element_markdown(
    colour = "#800020",
    family = "sans"),
  plot.caption = element_markdown(
    colour = "#800020",
    family = "sans"))))

ggsave(
  paste0("deforestation_1_", format(Sys.time(), "%d%m%Y"), ".pdf"),
  width = 25,
  height = 20,
  unit = "cm",
  dpi = 320
)

