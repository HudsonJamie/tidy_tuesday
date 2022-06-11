# pride_hypocrites.R
# Jamie Hudson
# Created: Date
# Edited: Date
# Data: Data

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(ggforce)
library(showtext)
font_add_google("Raleway")
font_add(family = "GilbertBoldPreview5", regular = "/Users/jamie/Library/Fonts/Gilbert-Bold Preview5.otf")
showtext_opts(dpi = 320)
showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 23)

# wrangle data ------------------------------------------------------------

static_list <- tuesdata$static_list

# plot ------------------------------------------------------------

options(scipen=10000)

plot_df <- static_list %>%
  janitor::clean_names() %>% 
  slice_head(n = 10) %>% 
  mutate(r = sqrt(amount_contributed_across_states/pi),
         r1 = r * 5/6,
         r2 = r * 4/6,
         r3 = r * 3/6,
         r4 = r * 2/6,
         r5 = r * 1/6,
         company = fct_reorder(factor(company), amount_contributed_across_states, min, .desc = T)) 
  
plot_df %>% 
  ggplot(aes(x0 = 1, y0 = 1)) +
  geom_arc_bar(aes(r0 = 0, r = r, start = - 1 * pi / 2, end = 1 * pi / 2, fill = ifelse(pride == T, "#E50000", "grey80"), colour = ifelse(pride == T, "#E50000", "grey80"))) +  
  geom_arc_bar(aes(r0 = 0, r = r1, start = - 1 * pi / 2, end = 1 * pi / 2, fill = ifelse(pride == T, "#FF8D00", "grey80"), colour = ifelse(pride == T, "#FF8D00", "grey80"))) +
  geom_arc_bar(aes(r0 = 0, r = r2, start = - 1 * pi / 2, end = 1 * pi / 2, fill = ifelse(pride == T, "#FFEE00", "grey80"), colour = ifelse(pride == T, "#FFEE00", "grey80"))) +
  geom_arc_bar(aes(r0 = 0, r = r3, start = - 1 * pi / 2, end = 1 * pi / 2, fill = ifelse(pride == T, "#028121", "grey80"), colour = ifelse(pride == T, "#028121", "grey80"))) +
  geom_arc_bar(aes(r0 = 0, r = r4, start = - 1 * pi / 2, end = 1 * pi / 2, fill = ifelse(pride == T, "#004CFF", "grey80"), colour = ifelse(pride == T, "#004CFF", "grey80"))) +
  geom_arc_bar(aes(r0 = 0, r = r5, start = - 1 * pi / 2, end = 1 * pi / 2, fill = ifelse(pride == T, "#770088", "grey80"), colour = ifelse(pride == T, "#770088", "grey80"))) +
  scale_fill_identity() +
  scale_colour_identity() +
  geom_segment(x = -550, xend = 550, y = 0, yend = 0, colour = "grey45") +
  geom_text(aes(x = 0, y = -100, label = company), family = "GilbertBoldPreview5") +
  geom_text(aes(x = 0, y = r + 100, label = paste0("$", round(amount_contributed_across_states,0))), family = "GilbertBoldPreview5") +
  coord_fixed() +
  lims(x = c(-580, 580), y = c(-200, 600)) +
  facet_wrap(~ company, ncol = 5) +
  labs(title = "Companies That Fund Anti-LGBTQ+ Politicians",
       subtitle = "Of the ten highest contributors towards anti-LGBTQ+ politicians, six companies are also <span style='color:#E50000;'>s</span><span style='color:#FF8D00;'>p</span><span style='color:#FFCC00;'>o</span><span style='color:#028121;'>n</span><span style='color:#004CFF;'>s</span><span style='color:#770088;'>o</span><span style='color:#E50000;'>r</span><span style='color:#FF8D00;'>s</span> <span style='color:#FFCC00;'>o</span><span style='color:#028121;'>f</span> <span style='color:#004CFF;'>P</span><span style='color:#770088;'>r</span><span style='color:#E50000;'>i</span><span style='color:#FF8D00;'>d</span><span style='color:#FFCC00;'>e</span>",
       caption = "@jamie_bio | source: Data For Progress") +
  theme(strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "GilbertBoldPreview5", size = 40),
        plot.subtitle = element_markdown(family = "Raleway", face = "bold", size = 14.3),
        plot.caption = element_markdown(family = "Raleway", size = 8))

ggsave(paste0("pride_politicians_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 10.5,
       height =  4)

