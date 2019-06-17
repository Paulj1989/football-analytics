rm(list=ls())
library(foreign)
library(tidyverse)
library(ggpubr)
library(hrbrthemes)
library(ineq)
library(gganimate)
library(gifski)
library(transformr)

time3 <- read_csv("~/Documents/GitHub Directory/ftw/time3.csv")
big3$year <- as.factor(big3$year)


#Goals/Performance animation

my.animation <- ggplot(time3, aes(value, pts, color = country)) +
  geom_point(alpha = 0.7, size = 12) +
  theme_ipsum(base_size = 25, axis_title_size = 25)+
  theme(legend.position="bottom",
        plot.margin = margin(5.5, 40, 5.5, 5.5))+
  scale_color_brewer(name=NULL,
                        labels=c("Bundesliga", "Premier League", "La Liga"), palette="Set1")+
  labs(title = 'Year: {closest_state}', x = 'Market Value (€m)', y = 'Points') +
  transition_states(year, state_length = 0) +
  coord_cartesian(clip = 'off') + 
  shadow_mark(alpha = 0.3, size = 5)+
ease_aes('linear')


animate(my.animation, height = 800, width =800, nframes = 100, duration = 20)
anim_save("value.gif")

my.animation <- ggplot(time3, aes(value, color = country)) +
  geom_density(alpha = 0.7, size = 12) +
  theme_ipsum(base_size = 25, axis_title_size = 25)+
  theme(legend.position="bottom",
        plot.margin = margin(5.5, 40, 5.5, 5.5))+
  scale_color_brewer(name=NULL,
                     labels=c("Bundesliga", "Premier League", "La Liga"), palette="Set1")+
  labs(title = 'Year: {closest_state}', x = 'Market Value (€m)', y = 'Points') +
  transition_states(year, state_length = 0) +
  coord_cartesian(clip = 'off') + 
  shadow_mark(alpha = 0.3, size = 5)+
  ease_aes('linear')


animate(my.animation, height = 800, width =800, nframes = 100, duration = 20)
anim_save("value.gif")

#Transfer spending animation

ggplot(time3, aes(value, group = year)) + 
  geom_line() + 
  geom_segment(aes(xend = 31, yend = Temp), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text(aes(x = 31.1, label = year), hjust = 0) + 
  transition_reveal(year) + 
  coord_cartesian(clip = 'off') + 
  labs(title = 'Temperature in New York', y = 'Temperature (°F)') + 
  theme_minimal() + 
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))
