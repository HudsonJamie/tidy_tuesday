# mario_kart_records.R
# Jamie Hudson
# Created: 27 May 2021
# Edited: 27 May 2021
# Data: From Benedikt Claus : Mario Kart World Records (https://mkwrs.com/)

library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(ggforce)
library(ggtext)
library(showtext)
library(patchwork)
font_add_google("Play", "play")
showtext_auto()
font_add(family = "pretendo", regular = "/Library/Fonts/Pretendo.ttf")

# load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 22)

records <- tuesdata$records

# add cup information and course lengths 
# (from https://www.neoseeker.com/forums/2473/t1363478-course-lengths/)
# and calculate speed (for both single and three laps)

records_2 <- records %>% 
  mutate(cup = case_when(track %in% c("Luigi Raceway", "Moo Moo Farm", "Koopa Troopa Beach", "Kalimari Desert") ~ "mushroom_cup",
                         track %in% c("Toad's Turnpike", "Frappe Snowland", "Choco Mountain", "Mario Raceway") ~ "flower_cup",
                         track %in% c("Wario Stadium", "Sherbet Land", "Royal Raceway", "Bowser's Castle") ~ "star_cup",
                         track %in% c("D.K.'s Jungle Parkway", "Yoshi Valley", "Banshee Boardwalk", "Rainbow Road") ~ "special_cup"),
         length = case_when(track == "Rainbow Road"~2000,
                            track == "Wario Stadium"~1591,
                            track == "Toad's Turnpike"~1036,
                            track == "Royal Raceway"~1025,
                            track == "D.K.'s Jungle Parkway"~893,
                            track == "Bowser's Castle"~777,
                            track == "Yoshi Valley"~772,
                            track == "Sherbet Land"~756,
                            track == "Kalimari Desert"~753,
                            track == "Banshee Boardwalk"~747,
                            track == "Frappe Snowland"~734,
                            track == "Luigi Raceway"~717,
                            track == "Koopa Troopa Beach"~691,
                            track == "Choco Mountain"~687,
                            track == "Mario Raceway"~567,
                            track == "Moo Moo Farm"~527),
         speed = case_when(type == "Three Lap" ~ length*3/ time,
                           type == "Single Lap" ~ length/ time))

# Find top 5 players (i.e. have held the most number of records)

top_players <- records %>% 
  group_by(player) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  slice_head(n = 5)

# Obtain information for each record of top player
# and add nation to this

best_drivers <- records_2 %>% 
  filter(player %in% top_players$player) %>% 
  group_by(player) %>% 
  add_tally() %>% 
  mutate(nation = case_when(player == "Booth" | player == "Dan" ~ "USA",
                            player == "Penev" ~ "Australia",
                            player == "MJ"  ~ "Netherlands",
                            player == "MR" ~ "Germany"))

# What cup does each player have most records in?
fav_cup <- best_drivers %>% 
  group_by(player, cup) %>% 
  count() %>% 
  group_by(player) %>% 
  filter(n == max(n)) %>% 
  rename(fav_cup = cup) %>% 
  select(player, fav_cup) %>% 
  ungroup()

# What system does each player have most records in? (PAL or NTSC)
system_type <- best_drivers %>% 
  group_by(player, system_played) %>% 
  count() %>% 
  group_by(player) %>% 
  filter(n == max(n)) %>% 
  rename(fav_system_played = system_played) %>% 
  select(player, fav_system_played)

# Join together
best_drivers_1 <- inner_join(fav_cup, best_drivers, by = "player")
best_drivers_2 <- inner_join(system_type, best_drivers_1, by = "player")

# For joystick movement, create x and y movement based on angle.
best_drivers_3 <- best_drivers_2  %>%
  mutate(angle_rad = 2*pi/400*n,  ## using current digit determine direction to move
         angle_deg = circular::deg(angle_rad), ## I just like to see number in degree...
         move_x = sin(angle_rad)*0.7, ## how much to move in x direction
         move_y = cos(angle_rad)*0.7 ## how much to move in y direction
  )

# Shapes for N64 controller -------------------------------------------------------------

beziers <- data.frame(
  x = c(-1.6, -1.3, 1.3, 1.6, -5.9, -6.8, -4.5, -3.4, 3.4, 4.5, 6.8, 5.9),
  y = c(-2, -10, -10, -2, -1, -7.5, -7.6, -2, -2, -7.6, -7.5, -1),
  point = rep(c('end', 'control', 'control', 'end'), c(3,3,3,3))
)

beziersData <- data.frame(
  x = c(-1.6, -1.3, 1.3, 1.6, -1.6, -1.3, 1.3, 1.6, -1.6, -1.3, 1.3, 1.6),
  y = c(-2, -10, -10, -2, -2, -9, -9, -2, -2, -2.5, -2.5, -2),
  group = c(1,1,1,1, 2, 2, 2, 2, 3, 3, 3, 3))

central_brez <- data.frame(
  x = c(-1.6, -1.3, 1.3, 1.6),
  y = c(-2, -10, -10, -2),
  group = c(1,1,1,1))

central_brez <- do.call(rbind, lapply(seq_len(500)-1, function(i) {
  central_brez$y <- central_brez$y + c(0,i/50, i/50,0);
  central_brez$group <- i+1;
  central_brez
}))

left_brez <- data.frame(
  x = c(-5.9, -6.8, -4.5, -3.4, -5.9, -5, -4.5, -3.4),
  y = c(-1, -7.5, -7.6, -2, -1, -1.5, -1.6, -2),
  group = c(1,1,1,1,2,2,2,2))

left_brez <- data.frame(
  x = c(-5.9, -6.8, -4.5, -3.4),
  y = c(-0.5, -7.5, -7.6, -2),
  group = c(1,1,1,1))

left_brez <- do.call(rbind, lapply(seq_len(500)-1, function(i) {
  left_brez$y <- left_brez$y + c(0,i/83, i/83,0);
  left_brez$x <- left_brez$x + c(0,(i/250), 0,0);
  left_brez$group <- i+1;
  left_brez
}))

right_brez <- data.frame(
  x = c(3.4, 4.5, 6.8, 5.9),
  y = c(-2, -7.6, -7.5, -0.5),
  group = c(1,1,1,1))

right_brez <- do.call(rbind, lapply(seq_len(500)-1, function(i) {
  right_brez$y <- right_brez$y + c(0,i/83, i/83,0);
  right_brez$x <- right_brez$x - c(0,0, (i/250),0);
  right_brez$group <- i+1;
  right_brez
}))

rocketData <- data.frame(
  y = c(-2.6, -2.6, -8, -8),
  x = c(-2.9,-1,-1.5,0),
  group = c(1,1,1,1)
)

rocketData <- do.call(rbind, lapply(seq_len(500)-1, function(i) {
  rocketData$y <- rocketData$y + c(0,0,i/90.1,i/90.1);
  rocketData$group <- i+1;
  rocketData
}))

rocketData2 <- data.frame(
  y = c(-8, -8, -2.6, -2.6),
  x = c(0, 1.5, 0.5, 2.9),
  group = c(1,1,1,1)
)

rocketData2 <- do.call(rbind, lapply(seq_len(500)-1, function(i) {
  rocketData2$y <- rocketData2$y + c(i/90.1,i/90.1,0,0);
  rocketData2$group <- i+1;
  rocketData2
}))

left_brez1 <- data.frame(
  x = c(-5.9, -6.8, -6.5, -5.2),
  y = c(-0.3, -2, -6, -6),
  group = c(1,1,1,1))

left_brez1 <- do.call(rbind, lapply(seq_len(500)-1, function(i) {
  left_brez1$x <- left_brez1$x + c(0, i/500, i/500,0);
  left_brez1$y <- left_brez1$y + c(0, i/334, i/100, i/100);
  left_brez1$group <- i+1;
  left_brez1
}))

left_brez2 <- data.frame(
  x = c(-5.2, -4.5, -4.3, -3.5),
  y = c(-6, -6, -2.1, -2.5),
  group = c(1,1,1,1))

left_brez2 <- do.call(rbind, lapply(seq_len(500)-1, function(i) {
  left_brez2$y <- left_brez2$y + c(i/90.1,i/90.1,0,0);
  left_brez2$group <- i+1;
  left_brez2
}))

right_brez1 <- data.frame(
  x = c(3.5, 4.3, 4.5, 5.2),
  y = c(-2.5, -2.1, -6, -6),
  group = c(1,1,1,1))


right_brez1 <- do.call(rbind, lapply(seq_len(500)-1, function(i) {
  right_brez1$y <- right_brez1$y + c(0,0,i/90.1,i/90.1);
  right_brez1$group <- i+1;
  right_brez1
}))

right_brez2 <- data.frame(
  x = c(5.2, 6.5, 6.8, 5.9),
  y = c(-6, -6, -2, -0.3),
  group = c(1,1,1,1))



right_brez2 <- do.call(rbind, lapply(seq_len(500)-1, function(i) {
  right_brez2$x <- right_brez2$x - c(0, i/500, i/500,0);
  right_brez2$y <- right_brez2$y + c(i/100, i/100, i/334, 0);
  right_brez2$group <- i+1;
  right_brez2
}))


top_brez_left <- data.frame(
  x = c(-2.7, -2.8, -0.6, 0),
  y = c(2.5, 3.45, 3.45, 3.45),
  group = c(1,1,1,1))

top_brez_left <- do.call(rbind, lapply(seq_len(500)-1, function(i) {
  top_brez_left$y <- top_brez_left$y - c(0,i/500,i/500,i/500);
  top_brez_left$group <- i+1;
  top_brez_left
}))

top_brez_right <- data.frame(
  x = c(0, 0.6, 2.8, 2.7),
  y = c(3.45, 3.45, 3.45, 2.5),
  group = c(1,1,1,1))

top_brez_right <- do.call(rbind, lapply(seq_len(500)-1, function(i) {
  top_brez_right$y <- top_brez_right$y - c(i/500,i/500,i/500, 0);
  top_brez_right$group <- i+1;
  top_brez_right
}))

left_trigger <- data.frame(
  x = c(-5.1, -4.3, -3.5, -2.6),
  y = c(2.3, 2.7, 2.6, 2.85),
  group = c(1,1,1,1))

left_trigger <- do.call(rbind, lapply(seq_len(500)-1, function(i) {
  left_trigger$y <- left_trigger$y - c(i/1000,i/1000,i/1000, i/1000);
  left_trigger$group <- i+1;
  left_trigger
}))

right_trigger <- data.frame(
  x = c(2.6, 3.5, 4.3, 5.1),
  y = c(2.85, 2.6, 2.7, 2.3),
  group = c(1,1,1,1))

right_trigger <- do.call(rbind, lapply(seq_len(500)-1, function(i) {
  right_trigger$y <- right_trigger$y - c(i/1000,i/1000,i/1000, i/1000);
  right_trigger$group <- i+1;
  right_trigger
}))



# N64 function ------------------------------------------------------------
# Awfully messy code- I need to learn how to actually write functions :'(
# but it does the job for now.
n64_plot <- function(data) {
  if(data$nation == "USA") {
    col = "grey80"
    col2 = "grey50"
    col3 = "grey85"}
  else if(data$nation == "Netherlands") {
    col = "#F9B704"
    col2 = "#E49517"
    col3 = "#F9C107"}
  else if(data$nation == "Australia") {
    col = "#32CA9B"
    col2 = "#259A81"
    col3 = "#49DC9B"}
  else {
    col = "#547DE1"
    col2 = "#2E4580"
    col3 = "#749DF1"}
  
  if(data$fav_cup == "mushroom_cup") {
    colyel1 = "darkorange3"
    colyel2 = "#F9BB03"
    colyel3 = "#F9BB03"
    colyel4 = "#F9BB03"}
  else if(data$fav_cup == "flower_cup") {
    colyel1 = "#F9BB03"
    colyel2 = "darkorange3"
    colyel3 = "#F9BB03"
    colyel4 = "#F9BB03"}
  else if(data$fav_cup == "star_cup") {
    colyel1 = "#F9BB03"
    colyel2 = "#F9BB03"
    colyel3 = "darkorange3"
    colyel4 = "#F9BB03"}
  else {
    colyel1 = "#F9BB03"
    colyel2 = "#F9BB03"
    colyel3 = "#F9BB03"
    colyel4 = "darkorange3"}
  
  
  joy_x_shift = data %>% select(move_x) %>% distinct() %>% as.numeric()
  joy_y_shift = data %>% select(move_y) %>% distinct() %>% as.numeric()
  player_name = data %>% select(player) %>% distinct() %>% as.character()
  top_speed = data %>% filter(shortcut == "No") %>% 
    select(speed) %>% filter(speed == max(speed)) %>% as.numeric()
  top_speed_track = data %>% filter(shortcut == "No") %>% 
    select(speed, track) %>% filter(speed == max(speed)) %>% 
    select(track) %>% as.character()
  
  if(data$fav_system_played == "PAL") {
    green_button = 0.1
    blue_button = 1
  } else {
    green_button = 1
    blue_button = 0.1
  }
  
  ggplot()  + 
    geom_bezier(data=left_trigger, aes(x=x, y=y, group=group), colour="grey50") +
    geom_bezier(data=right_trigger, aes(x=x, y=y, group=group), colour="grey50") +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = 6, b = 2.7, angle = 0, m1 = 3),
                 colour = col, fill = col) +
    geom_bezier(data=rocketData, aes(x=x, y=y, group=group), colour=col) +
    geom_bezier(data=rocketData2, aes(x=x, y=y, group=group), colour=col) +
    geom_bezier(data=left_brez1, aes(x=x, y=y, group=group), colour=col) +
    geom_bezier(data=left_brez2, aes(x=x, y=y, group=group), colour=col) +
    geom_bezier(data=right_brez1, aes(x=x, y=y, group=group), colour=col) +
    geom_bezier(data=right_brez2, aes(x=x, y=y, group=group), colour=col) +
    geom_circle(aes(x0 = 0, y0 = -3.2, r = 1.2), fill = "grey50") +
    geom_regon(aes(x0 = 0, y0 = -3.2, r = 0.7, sides = 8, angle = 90), fill = "grey25") +
    geom_circle(aes(x0 = 0 + (joy_x_shift), y0 = -3.2 + (joy_y_shift), r = 0.45), fill = "grey90", colour = "grey90") +
    geom_segment(aes(x = 2.94, y = -1.16, xend = 2.75, yend = -0.85), colour = col2, size = 1) +
    geom_segment(aes(x = 3.05, y = -1.1, xend = 2.865, yend = -0.79), colour = col2, size = 1) +
    geom_circle(aes(x0 = 0, y0 = -0.5, r = 0.43), fill = "#F71A0F") +
    geom_circle(aes(x0 = 3.2, y0 = -1.5, r = 0.43), fill = "#2217B7", alpha = blue_button) +
    geom_circle(aes(x0 = 2.6, y0 = -0.45, r = 0.43), fill = "#1EA21B", alpha = green_button) +
    geom_circle(aes(x0 = 4.4, y0 = 0.2, r = 0.8), fill = col, colour = col2, size = 1) +
    geom_circle(aes(x0 = 4.4, y0 = -0.41, r = 0.33), fill = colyel2) +
    geom_circle(aes(x0 = 4.4, y0 = 0.8, r = 0.33), fill = colyel4) +
    geom_circle(aes(x0 = 3.7, y0 = 0.2, r = 0.33), fill = colyel3) +
    geom_circle(aes(x0 = 5.1, y0 = 0.2, r = 0.33), fill = colyel1) +
    geom_circle(aes(x0 = -4, y0 = -0.2, r = 1.2), fill = col3, colour = col3) +
    geom_rect(aes(xmin = -4.3, xmax = -3.7, ymin = -1, ymax = 0.6), fill = "grey50") +
    geom_rect(aes(xmin = -4.8, xmax = -3.2, ymin = -0.5, ymax = 0.1), fill = "grey50") +
    geom_bezier(data=top_brez_left, aes(x=x, y=y, group=group), colour=col) +
    geom_bezier(data=top_brez_right, aes(x=x, y=y, group=group), colour=col) +
    geom_textbox(aes(x = 0, y = 2, box.size = 0.6), fill = col, colour = col2, label = player_name, family = "pretendo",
                 box.r = unit(0.6, "lines"), width = 0.28, height = 0.055 , size = 3,
                 halign = 0.5, valign = 0.5) +
    geom_richtext(aes(x = 0, y = -9.5), label = glue::glue("Top Speed = {round(top_speed, 1)}ms<sup>-1</sup>  \n attained at {top_speed_track}"), size = 4.5, hjust = 0.5,
                  fill = NA, label.color = NA, family = "play") +  
    coord_fixed(ylim = c(-12,5)) +
    theme_void()
}

# legend ------------------------------------------------------------------

(legend <- ggplot()  + 
  geom_bezier(data=left_trigger, aes(x=x, y=y, group=group), colour="grey50") +
  geom_bezier(data=right_trigger, aes(x=x, y=y, group=group), colour="grey50") +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 6, b = 2.7, angle = 0, m1 = 3),
               colour = "grey80", fill = "grey80") +
  geom_bezier(data=rocketData, aes(x=x, y=y, group=group), colour="grey80") +
  geom_bezier(data=rocketData2, aes(x=x, y=y, group=group), colour="grey80") +
  geom_bezier(data=left_brez1, aes(x=x, y=y, group=group), colour="grey80") +
  geom_bezier(data=left_brez2, aes(x=x, y=y, group=group), colour="grey80") +
  geom_bezier(data=right_brez1, aes(x=x, y=y, group=group), colour="grey80") +
  geom_bezier(data=right_brez2, aes(x=x, y=y, group=group), colour="grey80") +
  geom_circle(aes(x0 = 0, y0 = -3.2, r = 1.2), fill = "grey50") +
  geom_regon(aes(x0 = 0, y0 = -3.2, r = 0.7, sides = 8, angle = 90), fill = "grey25") +
  geom_segment(aes(x = 0, y = -1.6, xend = 0, yend = -4.8), size = 1,
               arrow = arrow(length = unit(0.1, "inch"), ends = "both")) +
  geom_segment(aes(x = -1.6, y = -3.2, xend = 1.6, yend = -3.2), size = 1,
               arrow = arrow(length = unit(0.1, "inch"), ends = "both")) +
  geom_circle(aes(x0 = 0, y0 = -3.2, r = 0.45), fill = "grey90", colour = "grey90") +
  geom_segment(aes(x = 2.94, y = -1.16, xend = 2.75, yend = -0.85), colour = "grey50", size = 1) +
  geom_segment(aes(x = 3.05, y = -1.1, xend = 2.865, yend = -0.79), colour = "grey50", size = 1) +
  geom_circle(aes(x0 = 0, y0 = -0.5, r = 0.43), fill = "#F71A0F") +
  geom_circle(aes(x0 = 3.2, y0 = -1.5, r = 0.43), fill = "#2217B7") +
  geom_circle(aes(x0 = 2.6, y0 = -0.45, r = 0.43), fill = "#1EA21B") +
  geom_circle(aes(x0 = 4.4, y0 = 0.2, r = 0.8), fill = "grey80", colour = "grey50", size = 1) +
  geom_circle(aes(x0 = 4.4, y0 = -0.41, r = 0.33), fill = "#F9BB03") +
  geom_circle(aes(x0 = 4.4, y0 = 0.8, r = 0.33), fill = "#F9BB03") +
  geom_circle(aes(x0 = 3.7, y0 = 0.2, r = 0.33), fill = "#F9BB03") +
  geom_circle(aes(x0 = 5.1, y0 = 0.2, r = 0.33), fill = "#F9BB03") +
  geom_circle(aes(x0 = -4, y0 = -0.2, r = 1.2), fill = "grey85", colour = "grey85") +
  geom_rect(aes(xmin = -4.3, xmax = -3.7, ymin = -1, ymax = 0.6), fill = "grey50") +
  geom_rect(aes(xmin = -4.8, xmax = -3.2, ymin = -0.5, ymax = 0.1), fill = "grey50") +
  geom_bezier(data=top_brez_left, aes(x=x, y=y, group=group), colour="grey80") +
  geom_bezier(data=top_brez_right, aes(x=x, y=y, group=group), colour="grey80") +
  geom_textbox(aes(x = 0, y = 2, box.size = 0.9), fill = "grey80", colour = "grey50", label = "Name of player", family = "pretendo",
               box.r = unit(0.7, "lines"), width = 0.28, size = 3,
               halign = 0.5, valign = 0.5) +
  annotate("text", x = 4.4, y = 1.6, label = "Mushroom \nCup", size = 3, hjust = 0.5, family = "play") +
  annotate("text", x = 5.9, y = 0.5, label = "Flower \nCup", size = 3, hjust = 0.5, family = "play") +
  annotate("text", x = 4.4, y = -1.2, label = "Star \nCup", size = 3, hjust = 0.5, family = "play") +
  annotate("text", x = 3, y = 0.5, label = "Special \nCup", size = 3, hjust = 0.5, family = "play") +
  annotate("text", x = 0, y = -1.4, label = "400 records", size = 3, hjust = 0.5, family = "play") +
  annotate("text", x = 2, y = -3.2, label = "100 \nrecords", size = 3, hjust = 0.5, family = "play") +
  annotate("text", x = 0, y = -5, label = "200 records", size = 3, hjust = 0.5, family = "play") +
  annotate("text", x = -2, y = -3.2, label = "300 \nrecords", size = 3, hjust = 0.5, family = "play") +
  coord_fixed() +
  labs(title = "Let's-a go! The top 5 Mario Kart 64 players",
       subtitle = "Here we see the five Mario Kart 64 players that have held the most number of records.  \nAfter including data relating to track length (available online),  \none can calculate the top speed attained by each driver throughout their World Record races.  \n The <b><span style = 'color:#F9BB03;'>yellow C-Pad</span></b> represents the cup that each player obtained the most World Record times.  \nThe <b><span style = 'color:#1EA21B;'>green B button</span></b> and <b><span style = 'color:#2217B7;'>blue A button</span></b> represent whether the player tended to use <b><span style = 'color:#1EA21B;'>PAL</span></b> or <b><span style = 'color:#2217B7;'>NTSC</span></b> systems.  \n The total number of World Record  achieved by each player is mapped to the direction of the joystick.  \n Finally, the controller colour represents the nation of each player, either: <b><span style = 'color:#32CA9B;'>Australia</span></b>, <b><span style = 'color:#547DE1;'>Germany</span></b>, <b><span style = 'color:#F9B704;'>Netherlands</span></b>, or <b><span style = 'color:grey80;'>USA</span></b>.") +
  theme_void() +
  theme(plot.title = element_text(family = "play", size = 35,
                                  hjust = 0.5),
        plot.subtitle = element_markdown(size = 13,
                                         family = "play", 
                                         lineheight = 1.1,
                                         hjust = 0.5)))

# produce plots -----------------------------------------------------------

# Penev
penev_df <- best_drivers_3 %>% ungroup() %>% filter(player == "Penev")
A <- n64_plot(data = penev_df)

# Booth
booth_df <- best_drivers_3 %>% ungroup() %>% filter(player == "Booth")
B <- n64_plot(data = booth_df)

# MJ
mj_df <- best_drivers_3 %>% ungroup() %>% filter(player == "MJ")
C <- n64_plot(data = mj_df)

# MR
mr_df <- best_drivers_3 %>% ungroup() %>% filter(player == "MR")
D <- n64_plot(data = mr_df)

# Dan
dan_df <- best_drivers_3 %>% ungroup() %>% filter(player == "Dan")
E <- n64_plot(data = dan_df)


layout <- c(
  area( t = 1, l = 1, b = 4, r = 4),
  area( t = 1, l = 5, b = 4, r = 8),
  area( t = 1, l = 9, b = 4, r = 12),
  area( t = 5, l = 1, b = 8, r = 2),
  area( t = 5, l = 3, b = 8, r = 6),
  area( t = 5, l = 7, b = 8, r = 10),
  area( t = 5, l = 11, b = 8, r = 12))

controllers <- D + E + C + plot_spacer() + A + B + plot_spacer() +
  plot_layout(design = layout) & theme(plot.margin = margin(0,0,0,0, "cm"))

layout_2 <- c(
  area( t = 1, l = 1, b = 4, r = 6),
  area( t = 5, l = 1, b = 11, r = 6))

legend + controllers +
  plot_layout(design = layout_2) +
  plot_annotation(caption = "@jamie_bio | source = Benedikt Claus",
                  theme = theme(plot.caption.position = 'plot',
                                plot.caption = element_text(family = "play")))

ggsave(paste0("mario_kart_64_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 10,
       height = 14)
