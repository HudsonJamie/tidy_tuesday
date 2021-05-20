# annual_salary.R
# Jamie Hudson
# Created: 20 May 2021
# Edited: 20 May 2021
# Data: Ask a Manager (https://docs.google.com/spreadsheets/d/1IPS5dBSGtwYVbjsfbaMCYIWnOuRmJcbequohNxCyGVw/edit?resourcekey#gid=1625408792)

# load libraries ----------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggtext) 

# load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 21)

survey <- tuesdata$survey

# wrangle data ------------------------------------------------------------

us_data <- survey %>% 
  filter(currency == "USD" & annual_salary < 20000000)

# education completed -----------------------------------------------------

education_completed <- us_data %>% 
  filter(gender %in% c("Man", "Woman")) %>% 
  group_by(highest_level_of_education_completed, gender) %>% 
  dplyr::summarise(n = n())

# higher education ------------------------------------------

phd <- us_data %>% 
  filter(highest_level_of_education_completed == "PhD") %>% 
  group_by(gender) %>% 
  drop_na(gender) %>% 
  dplyr::summarise(n = n())

sum_n_phd <-  sum(phd$n)

(PhD_all <- phd %>%
  ggplot(aes(fill = gender, values = round(n*100/(sum_n_phd+1)))) +
  waffle::geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
  coord_equal() +
  scale_fill_manual(
    name = NULL,
    values = c("#67B8D6FF", "#D8AF39", "#5E2D30FF", "#C5A387FF"),
    labels = c("Male", "Non-binary", "Other", "Female")) +
    theme_void() +
    labs(title = "PhD degree") +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 8))
  )

# Top 10%

phd_top_10perc <- us_data %>% 
  filter(highest_level_of_education_completed == "PhD") %>% 
  drop_na(gender) %>%
  arrange(desc(annual_salary)) %>% 
  slice_max(annual_salary, prop = 0.1) %>% 
  group_by(gender) %>% 
  dplyr::summarise(n = n())

sum_n_phd_10 <-  sum(phd_top_10perc$n)

(PhD_10 <- phd_top_10perc %>%
  ggplot(aes(fill = gender, values = round(n*100/sum_n_phd_10-0.2))) + # 355.5 as issue with 101% ruining aestheti
  waffle::geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
  coord_equal() +
    scale_fill_manual(
      name = NULL,
      values = c("#67B8D6FF", "#D8AF39", "#5E2D30FF", "#C5A387FF"),
      labels = c("Male", "Non-binary", "Other", "Female")) +
    theme_void() +
    labs(title = "PhD degree \n(top 10% of earners)") +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 8))
)

# master's degree   ------------------------------------------

masters <- us_data %>% 
  filter(highest_level_of_education_completed == "Master's degree") %>% 
  group_by(gender) %>% 
  drop_na(gender) %>% 
  dplyr::summarise(n = n())

sum_n_masters <-  sum(masters$n)

masters_all <- masters %>%
  ggplot(aes(fill = gender, values = round(n*100/sum_n_masters))) +
  waffle::geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
  coord_equal() +
  scale_fill_manual(
    name = NULL,
    values = c("#67B8D6FF", "#D8AF39", "#5E2D30FF", "#C5A387FF"),
    labels = c("Male", "Non-binary", "Other", "Female")) +
  theme_void() +
  labs(title = "Master's degree") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 8))

# Top 10%

masters_top_10perc <- us_data %>% 
  filter(highest_level_of_education_completed == "Master's degree") %>% 
  drop_na(gender) %>%
  arrange(desc(annual_salary)) %>% 
  slice_max(annual_salary, prop = 0.1) %>% 
  group_by(gender) %>% 
  dplyr::summarise(n = n())

sum_n_masters_10 <-  sum(masters_top_10perc$n)

masters_10 <- masters_top_10perc %>%
  ggplot(aes(fill = gender, values = round(n*100/sum_n_masters_10-0.1))) + # 355.5 as issue with 101% ruining aestheti
  waffle::geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
  coord_equal() +
  scale_fill_manual(
    name = NULL,
    values = c("#67B8D6FF", "#D8AF39", "#5E2D30FF", "#C5A387FF"),
    labels = c("Male", "Non-binary", "Other", "Female")) +
  theme_void() +
  labs(title = "Masters degree \n(top 10% of earners)") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 8))


# professional degree   ------------------------------------------

prof <- us_data %>% 
  filter(highest_level_of_education_completed == "Professional degree (MD, JD, etc.)") %>% 
  group_by(gender) %>% 
  drop_na(gender) %>% 
  dplyr::summarise(n = n())

sum_n_prof <-  sum(prof$n)


prof_all <- prof %>%
  ggplot(aes(fill = gender, values = round(n*100/sum_n_prof+0.1))) +
  waffle::geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
  coord_equal() +
  scale_fill_manual(
    name = NULL,
    values = c("#67B8D6FF", "#D8AF39", "#5E2D30FF", "#C5A387FF"),
    labels = c("Male", "Non-binary", "Other", "Female")) +
  theme_void() +
  labs(title = "Professional degree") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 8))

# Top 10%

prof_top_10perc <- us_data %>% 
  filter(highest_level_of_education_completed == "Professional degree (MD, JD, etc.)") %>% 
  drop_na(gender) %>%
  arrange(desc(annual_salary)) %>% 
  slice_max(annual_salary, prop = 0.1) %>% 
  group_by(gender) %>% 
  dplyr::summarise(n = n()) %>% 
  add_row(gender = c("Non-binary", "Other or prefer not to answer"), n = c(0,0)) %>% 
  arrange(gender)



sum_n_prof_10 <-  sum(prof_top_10perc$n)

prof_10 <- prof_top_10perc %>%
  ggplot(aes(fill = gender, values = round(n*100/sum_n_prof_10-0.2))) + # 355.5 as issue with 101% ruining aestheti
  waffle::geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
  coord_equal() +
  scale_fill_manual(
    name = NULL,
    values = c("#67B8D6FF", "#C5A387FF", "#5E2D30FF", "#C5A387FF"),
    labels = c("Male", "Female", "Other", "Non-binary")) +
  theme_void() +
  labs(title = "Profesional degree \n(top 10% of earners)") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 8))

# college degree   ------------------------------------------

college <- us_data %>% 
  filter(highest_level_of_education_completed == "College degree") %>% 
  group_by(gender) %>% 
  drop_na(gender) %>% 
  dplyr::summarise(n = n())

sum_n_college <-  sum(college$n)

college_all <- college %>%
  ggplot(aes(fill = gender, values = round(n*100/sum_n_college))) +
  waffle::geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
  coord_equal() +
  scale_fill_manual(
    name = NULL,
    values = c("#67B8D6FF", "#D8AF39", "#5E2D30FF", "#C5A387FF"),
    labels = c("Male", "Non-binary", "Other", "Female")) +
  theme_void() +
  labs(title = "College") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 8))

# Top 10%

college_top_10perc <- us_data %>% 
  filter(highest_level_of_education_completed == "College degree") %>% 
  drop_na(gender) %>%
  arrange(desc(annual_salary)) %>% 
  slice_max(annual_salary, prop = 0.1) %>% 
  group_by(gender) %>% 
  dplyr::summarise(n = n())

sum_n_college_10 <-  sum(college_top_10perc$n)

college_10 <- college_top_10perc %>%
  ggplot(aes(fill = gender, values = round(n*100/sum_n_college_10-0.05))) + # 355.5 as issue with 101% ruining aestheti
  waffle::geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
  coord_equal() +
  scale_fill_manual(
    name = NULL,
    values = c("#67B8D6FF", "#D8AF39", "#5E2D30FF", "#C5A387FF"),
    labels = c("Male", "Non-binary", "Other", "Female")) +
  theme_void() +
  labs(title = "College \n(top 10% of earners)") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 8))

# high school   ------------------------------------------

hs <- us_data %>% 
  filter(highest_level_of_education_completed == "High School") %>% 
  group_by(gender) %>% 
  drop_na(gender) %>% 
  dplyr::summarise(n = n())

sum_n_hs <-  sum(hs$n)

hs_all <- hs %>%
  ggplot(aes(fill = gender, values = round(n*100/sum_n_hs))) +
  waffle::geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
  coord_equal() +
  scale_fill_manual(
    name = NULL,
    values = c("#67B8D6FF", "#D8AF39", "#5E2D30FF", "#C5A387FF"),
    labels = c("Male", "Non-binary", "Other", "Female")) +
  theme_void() +
  labs(title = "High School") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 8))

# Top 10%

hs_top_10perc <- us_data %>% 
  filter(highest_level_of_education_completed == "High School") %>% 
  drop_na(gender) %>%
  arrange(desc(annual_salary)) %>% 
  slice_max(annual_salary, prop = 0.1) %>% 
  group_by(gender) %>% 
  dplyr::summarise(n = n())

sum_n_hs_10 <-  sum(hs_top_10perc$n)

hs_10 <- hs_top_10perc %>%
  ggplot(aes(fill = gender, values = round(n*100/sum_n_hs_10-0.2))) + # 355.5 as issue with 101% ruining aestheti
  waffle::geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
  coord_equal() +
  scale_fill_manual(
    name = NULL,
    values = c("#67B8D6FF", "#5E2D30FF", "#C5A387FF"),
    labels = c("Male", "Other", "Female")) +
  theme_void() +
  labs(title = "High School \n(top 10% of earners)") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 8))

# some college   ------------------------------------------

some_col <- us_data %>% 
  filter(highest_level_of_education_completed == "Some college") %>% 
  group_by(gender) %>% 
  drop_na(gender) %>% 
  dplyr::summarise(n = n())

sum_n_some_col <-  sum(some_col$n)

some_col_all <- some_col %>%
  ggplot(aes(fill = gender, values = round(n*100/sum_n_some_col+0.15))) +
  waffle::geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
  coord_equal() +
  scale_fill_manual(
    name = NULL,
    values = c("#67B8D6FF", "#D8AF39", "#5E2D30FF", "#C5A387FF"),
    labels = c("Male", "Non-binary", "Other", "Female")) +
  theme_void() +
  labs(title = "Some college") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 8))

# Top 10%

some_col_top_10perc <- us_data %>% 
  filter(highest_level_of_education_completed == "Some college") %>% 
  drop_na(gender) %>%
  arrange(desc(annual_salary)) %>% 
  slice_max(annual_salary, prop = 0.1) %>% 
  group_by(gender) %>% 
  dplyr::summarise(n = n())

sum_n_some_col_10 <-  sum(some_col_top_10perc$n)

some_col_10 <- some_col_top_10perc %>%
  ggplot(aes(fill = gender, values = round(n*100/sum_n_some_col_10-0.1))) + # 355.5 as issue with 101% ruining aestheti
  waffle::geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
  coord_equal() +
  scale_fill_manual(
    name = NULL,
    values = c("#67B8D6FF", "#D8AF39", "#5E2D30FF", "#C5A387FF"),
    labels = c("Male", "Non-binary", "Other", "Female")) +
  theme_void() +
  labs(title = "Some college \n(top 10% of earners)") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 8))

# filter figure -----------------------------------------------------------
# very messy code but my brain can't cope with tidying it right now :( 

(filt <- ggplot() + 
    geom_regon(aes(x0 = c(-0.5,-0.3,-0.1,0.1,0.4), y0 = c(1, 0.9, 0.75, 0.7, 0.8), sides = rep(4, 5),
                   angle = 0, r = rep(0.055, 5)), fill = "#C5A387FF", alpha = 0.6) +
    geom_regon(aes(x0 = c(-0.23, 0.23), y0 = c(0.7, 0.87), sides = rep(4, 2),
                   angle = 0, r = rep(0.055, 2)), fill = "#67B8D6FF", alpha = 0.6) +
    geom_regon(aes(x0 = c(-0.1, -0.55), y0 = c(0.95, 0.9), sides = rep(4, 1),
                   angle = 0, r = rep(0.055, 1)), fill = "#D8AF39", alpha = 0.6) +
    geom_regon(aes(x0 = c(0.5, 0.05), y0 = c(0.95, 0.85), sides = rep(4, 2),
                   angle = 0, r = rep(0.055, 2)), fill = "#5E2D30FF", alpha = 0.6) +
    geom_regon(aes(x0 = c(-0.02, 0.02), y0 = c(0.17, 0.4), sides = rep(4, 2),
                   angle = 0, r = rep(0.055, 2)), fill = "#C5A387FF", alpha = 0.6) +
    geom_segment(aes(x = -0.85, y = 0.9, xend = -0.1, yend = 0.5), size = 1, alpha = 0.6) +
    geom_segment(aes(x = 0.85, y = 0.9, xend = 0.1, yend = 0.5), size = 1, alpha = 0.6) +
    geom_segment(aes(x = -0.1, y = 0.5, xend = -0.1, yend = 0), size = 1, alpha = 0.6) +
    geom_segment(aes(x = 0.1, y = 0.5, xend = 0.1, yend = 0), size = 1, alpha = 0.6) +
    geom_richtext(aes(x = 0.6, y = 0.5, label = "If we filter the data to look at only the top 10%  \nof earners in each group, the gender composition  \nchanges, with >50%  of the top earners with some  \ncollege and high school experience identifying as male",
                      hjust = 0), size = 3.5, colour = "black",
                  fill = NA, label.color = NA) +
    geom_curve(aes(x = 0.6, y = 0.55, xend = 0.15, yend = 0.4),
               arrow = arrow(length = unit(0.1, "inch")),
               size = 0.5,
               curvature = -0.2,
               color = "black"
    ) +
    geom_curve(aes(x = 1.63, y = 0.26, xend = 1.83, yend = 0),
               arrow = arrow(length = unit(0.1, "inch")),
               size = 0.5,
               curvature = -0.2,
               color = "black"
    ) +
    lims(x = c(-2.5,2.5), y = c(0, 1)) +
    theme_void())

# patchwork ---------------------------------------------------------------
# Produce patchwork layout

layout <- '
ABCDEF
OOOOOO
GHIJKL
'

# Combine plots

wrap_plots(A = prof_all, B = PhD_all, C = masters_all, D = college_all, E = some_col_all, F = hs_all,
           O = filt,
           G = prof_10, H = PhD_10, I = masters_10, J = college_10, K = some_col_10, L = hs_10, design = layout)  +
  plot_annotation(title = 'The gender gap by education',
                  subtitle = "The top row of waffle charts represent all respondees that are paid in USD grouped by education level.  \nEach invidual box represents 1% of respondees, per education level, colour coded as identifying as either  \n<b><span style = 'color:#C5A387FF;'>female</span></b>, <b><span style = 'color:#D8AF39;'>non-binary</span></b>, <b><span style = 'color:#5E2D30FF;'>other</span></b>, or <b><span style = 'color:#67B8D6FF;'>male</span></b>.",
                  caption = "@jamie_bio | source = Ask A Manager",
                  theme = theme(text = element_text(family ="robo"),
                                plot.title = element_markdown(size = 30, family = "merri"),
                                plot.subtitle = element_markdown(lineheight = 1.1),
                                plot.title.position = 'plot',
                                plot.caption.position = 'plot'))

# ggsave("gendergap1.png",
#        width = 9, height = 6.5)

ggsave(paste0("salary_data", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 9,
       height = 6.5)


