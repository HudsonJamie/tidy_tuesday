# uk_gender_pay_gap.R
# Jamie Hudson
# Created: 28 June 2022
# Edited: 28 June 2022
# Data: gender-pay-gap.service.gov.uk

# load libraries ------------------------------------------------------------

library(tidytuesdayR)
library(lubridate)
library(ggpattern)
library(scales)
library(showtext)
font_add_google("Montserrat")
font_add_google("Bebas Neue")
showtext_opts(dpi = 320)
showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 26)

# wrangle data ------------------------------------------------------------

paygap <- tuesdata$paygap

paygrap_df_10 <- paygap %>% 
  mutate(year = as.factor(year(due_date))) %>% 
  mutate(employer_name = tolower(employer_name)) %>% 
  filter(grepl("virgin atlantic|easyjet|british airways|eastern airways|flybe|jet2|loganair|ryanair|wizz air", employer_name)) %>% 
  group_by(year) %>% 
  summarise(mean = mean(diff_median_hourly_percent)) %>% 
  mutate(value = (mean/100)*10 + 10,
         value_b = value - 10,
         value_a = 10) %>% 
  pivot_longer(!c(year, mean, value), names_to = "cash", values_to = "money")

paygrap_df_20 <- paygap %>% 
  mutate(year = as.factor(year(due_date))) %>% 
  mutate(employer_name = tolower(employer_name)) %>% 
  filter(grepl("virgin atlantic|easyjet|british airways|eastern airways|flybe|jet2|loganair|ryanair|wizz air", employer_name)) %>% 
  group_by(year) %>% 
  summarise(mean = mean(diff_median_hourly_percent)) %>% 
  mutate(value = (mean/100)*20 + 20,
         value_b = value - 20,
         value_a = 20) %>% 
  pivot_longer(!c(year, mean, value), names_to = "cash", values_to = "money")

# plot ------------------------------------------------------------

ggplot(paygrap_df_10) +
  geom_col_pattern(position="stack",
                   aes(x = year, y = money, pattern_fill = factor(cash, levels = c("value_b", "value_a"))), 
                   pattern = "image",
                   pattern_filename = 'img/ten_pound.jpeg',
                   pattern_type = "tile",
                   pattern_res = 320,
                   pattern_scale = -1,
                   width = 0.8,
                   colour  = "white") +
  scale_y_continuous(expand = c(0,0.5)) +
  geom_text(aes(x = year, y = value + 0.5, label = label_dollar(prefix = "£")(value)),
            family = "Bebas Neue", colour = "#A7582F", size = 4.5, alpha = 0.5) +
  labs(title = "For every £10 a woman earns...",
       subtitle = "In the UK airline industry, for every £10 a woman earns, this is how much is earned by a man. Based on median hourly wages.",
       caption = "@jamie_bio | source: gender-pay-gap.service.gov.uk") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "Montserrat", colour = "#A7582F", vjust = 3),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(family = "Bebas Neue", size = 27, colour = "#A7582F", hjust = 0.5),
        plot.subtitle = element_text(family = "Montserrat", size = 7, colour = "#A7582F", hjust = 0.5),
        plot.caption = element_text(family = "Montserrat", size = 5, colour = "#A7582F"))

ggsave(paste0("gender_pay_gap_10", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 7,
       height =  4.1)

ggplot(paygrap_df_20) +
  geom_col_pattern(position="stack",
                   aes(x = year, y = money, pattern_fill = factor(cash, levels = c("value_b", "value_a"))), 
                   pattern = "image",
                   pattern_filename = 'img/twenty_pound.jpeg',
                   pattern_type = "tile",
                   pattern_res = 320,
                   pattern_scale = -1,
                   width = 0.8,
                   colour  = "white") +
  scale_y_continuous(expand = c(0,1)) +
  geom_text(aes(x = year, y = value + 0.8, label = label_dollar(prefix = "£")(value)),
            family = "Bebas Neue", colour = "#443351", size = 4.5, alpha = 0.5) +
  labs(title = "For every £20 a woman earns...",
       subtitle = "In the UK airline industry, for every £20 a woman earns, this is how much is earned by a man. Based on median hourly wages.",
       caption = "@jamie_bio | source: gender-pay-gap.service.gov.uk") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "Montserrat", colour = "#443351", vjust = 3),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(family = "Bebas Neue", size = 27, colour = "#443351", hjust = 0.5),
        plot.subtitle = element_text(family = "Montserrat", size = 7, colour = "#443351", hjust = 0.5),
        plot.caption = element_text(family = "Montserrat", size = 5, colour = "#443351"))

ggsave(paste0("gender_pay_gap_20_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 7,
       height =  4.1)

