# file_name
# Jamie Hudson
# Created: Date
# Edited: Date
# Data: Data

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(patchwork)
library(png)
library(showtext)
library(ggtext)
font_add_google("Raleway", "raleway")
showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 32)
athletes <- tuesdata$athletes
athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')

# wrangle data ------------------------------------------------------------

athletes %>%
  skim()

athletes %>%
  glimpse()

athletes  %>%
  count(country)

irl_ath <- athletes %>% 
  filter(abb == "IRL",
         type == "Athletics") %>% 
  group_by(year) %>% 
  arrange(factor(medal, levels = c("Bronze", "Silver", "Gold"))) %>% 
  mutate(id = row_number(),
         num = case_when(medal == "Bronze" ~ 3,
                         medal == "Silver" ~ 2,
                         medal == "Gold" ~ 1),
         col = case_when(medal == "Bronze" ~ "#CD7F32",
                         medal == "Silver" ~ "#C0C0C0",
                         medal == "Gold" ~ "#FFD700"))

irl_ath_2 <- irl_ath %>% 
  filter(id == max(id)) %>% 
  mutate(n = id + 2)

# athletes with most medals
irl_ath %>% 
  ungroup() %>% 
  count(athlete) %>% 
  arrange(desc(n))

#GALLAGHER Rosaleen
gallagher <- irl_ath %>% 
  filter(athlete == "GALLAGHER Rosaleen") %>% 
  ungroup() %>% 
  mutate(id = row_number())

#TYNAN Ronan
tynan <- irl_ath %>% 
  filter(athlete == "TYNAN Ronan") %>% 
  ungroup() %>% 
  mutate(id = row_number())

top_athletes <- rbind(gallagher, tynan) %>% 
  mutate(num = case_when(medal == "Bronze" ~ 3,
                         medal == "Silver" ~ 2,
                         medal == "Gold" ~ 1),
         col = case_when(medal == "Bronze" ~ "#CD7F32",
                          medal == "Silver" ~ "#C0C0C0",
                          medal == "Gold" ~ "#FFD700"))

img <- readPNG("images/logo.png", T)
irl <- readPNG("images/irl.png", T)

# plot ------------------------------------------------------------

(plot_1 <- irl_ath %>% 
  ggplot(aes(x = year, y = id, fill = medal)) +
  geom_point(shape = 21, size = 4, stroke = 0, 
             alpha = 0.5) +
  scale_fill_manual(values = c("#CD7F32", "#FFD700", "#C0C0C0")) +
  scale_x_continuous(breaks = seq(from = 1980, to = 2016, by = 4)) +
  labs(title = "Athletic medals won for <span style = 'color:#169B62;'>Ireland</span> in  \nthe Paralympic Games since 1980") +
  annotate("text", label = "Medals \nwon =", x = 1980, y = 14,
           colour = "darkgreen", family = "raleway") +
  geom_text(data = irl_ath_2, mapping = aes(x = year, y = n, label = id),
            colour = "darkgreen", family = "raleway") +
  geom_text(mapping = aes(x = year, y = id, label = num, col = col), size = 2,
            family = "raleway") +
  scale_color_identity() +
  annotate("text", label = "Ireland won 13 Gold \nmedals in 1984", x = 1990, y = 47,
           family = "raleway", size = 3.5) +
  annotate("text", label = "Top 2 performing athletes", x = 2010, y = 46,
           family = "raleway") +
  geom_curve(aes(x = 1990, y = 45, xend = 1985, yend = 40),
             arrow = arrow(length = unit(0.07, "inch")),
             size = 0.2, color = "gray20", curvature = -0.2) +
  coord_cartesian(clip = 'off') +
  theme_bw() +
  theme(text = element_text(family = "raleway"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(20, 5.5, 5.5, 5.5),
        plot.title = element_markdown(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45)))

(plot_2 <- top_athletes %>% 
  ggplot(aes(x = id, y = gender, fill = medal)) +
  geom_point(shape = 21, size = 4, stroke = 0,
             alpha = 0.5) +
  scale_fill_manual(values = c("#CD7F32", "#FFD700", "#C0C0C0")) +
  geom_text(mapping = aes(x = id, y = gender, label = num, col = col), size = 2) +
  scale_color_identity() +
  theme_bw() +
  scale_y_discrete(labels = c("Ronan\nTynan", "Rosaleen\nGallagher")) +
  theme(text = element_text(family = "raleway"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()))

plot_1 + inset_element(plot_2, 0.55, 0.7, 0.99, 0.9) +
  inset_element(img, 0, 0.99, 0.2, 1.09, 
                  clip = F) +
  inset_element(irl, 0.8, 0.95, 1, 1.12, 
                clip = F)

ggsave(
  paste0("paralympics_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 6,
  height = 7.2
)

