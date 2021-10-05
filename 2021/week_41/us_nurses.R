# us_nurses.R
# Jamie Hudson
# Created: 05 Oct 2021
# Edited: 05 Oct 2021
# Data: Data.World

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(usdata)
library(RColorBrewer)
library(urbnmapr)
library(patchwork)
library(ggtext)
library(showtext)

font_add_google("Lora",
                "lora")
font_add_google("Lato", "lato")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 41)
nurses <- tuesdata$nurses

# wrangle data ------------------------------------------------------------

nurses <- janitor::clean_names(nurses)

nurses_2020 <- nurses %>% 
  filter(year == 2020,
         !state %in% c("Guam", "Puerto Rico", "Virgin Islands")) %>% 
  mutate(state = state2abbr(state),
         ave_sal = mean(annual_salary_avg),
         med_sal = median(annual_salary_median),
         pct_change_ave = ((annual_salary_avg - ave_sal)/annual_salary_avg),
         pct_change_med = ((annual_salary_median - med_sal)/annual_salary_median),
         diff_ave = (annual_salary_avg - ave_sal),
         diff_med = (annual_salary_median - med_sal),
         lab = case_when(pct_change_ave > 0 ~ pct_change_ave + 0.01,
                         TRUE ~ pct_change_ave - 0.01),
         lab_med = case_when(diff_med > 0 ~ diff_med + 1000,
                         TRUE ~ diff_med - 1000))

states_map <- urbnmapr::states %>% 
  rename(state = state_abbv)

nurses_map <- left_join(states_map, nurses_2020, by = "state")

# plot ------------------------------------------------------------

(map <- ggplot(nurses_map, aes(long, lat, group = group))+
    geom_polygon(aes(fill = diff_med), color = "white", size = 0.3) +
    scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                         limits = c(-50000, 50000)) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none"))

ggplot(nurses_2020, aes(x = reorder(state, diff_med), y = diff_med)) +
  geom_bar(stat = "identity", aes(fill = diff_med)) +
  geom_hline(yintercept = 0, colour = "grey80",
             size = 0.2) +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                       limits = c(-50000, 50000)) +
  geom_text(aes(y =lab_med, label = state),
            size = 1.2, colour = "grey50",
            family = "lato") +
  labs(title = "**Median** salaries of Registered Nurses in the US",
       subtitle = "The difference between **median** salaries of registered nurses of each state and the national **median** value ($71,280).  \nThere is a clear band within the US of States with <span style='color:#DFC27D;'>lower than average salaries</span> surrounded by those with <span style='color:#35978F;'>higher than average salaries</span>. ",
       caption = "@jamie_bio | source: Data.World") +
  scale_y_continuous(labels = function(x) ifelse(x > 0, paste0("+ $", x), ifelse(x < 0, paste0("- $", abs(x)), x)),
                     limits = c(-18000, 49000)) +
  # scale_y_continuous(labels = scales::dollar_format()) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "grey50",
                                   size = 5,
                                   family = "lato"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_line(colour = "grey80",
                                   size = 0.2),
        legend.position = "none",
        plot.title = element_markdown(size = 18,
                                      family = "lora"),
        plot.subtitle = element_markdown(colour = "grey50",
                                         family = "lato",
                                         size = 7,
                                         lineheight = 1.4),
        plot.caption = element_text(colour = "grey65",
                                    family = "lato",
                                    size = 5.5)) +
  inset_element(map, left = 0, right = 0.7, bottom = 0.3, top = 1.1)

ggsave(
  paste0("nurses_", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 7,
  height = 5
)
