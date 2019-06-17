rm(list=ls())
library(foreign)
library(tidyverse)
library(ggpubr)
library(hrbrthemes)
library(ineq)
library(gganimate)
library(gifski)
library(ggthemes)

time3 <- read_csv("~/Documents/GitHub Directory/ftw/time3.csv")
big3$year <- as.factor(big3$year)


#Goals/Performance animation

my.animation <- ggplot(time3, aes(value, pts, color = country)) +
  geom_point(alpha = 0.7, size = 12) +
  theme_ipsum(base_size = 25, axis_title_size = 25)+
  theme(legend.position="bottom",
        plot.margin = margin(5.5, 40, 5.5, 5.5))+
  scale_color_calc(name=NULL,
                        labels=c("Bundesliga", "Premier League", "La Liga"))+
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

spend <- read_csv("~/Documents/GitHub Directory/ftw/spend.csv")

spend_plot <- spend %>%
  ggplot(aes(Season, Spend, group=League, color=League)) +
  geom_line(size = 1, alpha = 0.6)+
  geom_point(size=4, alpha = 0.6)+xlab("Year")+ylab("Transfer Spending (€m)")+
  theme_ipsum(base_size = 18, axis_title_size = 18)+
  labs(color = "League")+
  scale_color_brewer(name = NULL, palette = "Set1")+
  theme(axis.text.x = element_text(angle=45))+
  transition_states(Season, state_length = 0)


animate(spend_plot, height = 800, width =800, nframes = 100, duration = 20)
anim_save("value.gif")

spend_plot

spend_plot <- ggplot(spend, aes(Season, Spend, group=League, color = League)) + 
  geom_line() + 
  geom_segment(aes(xend = Season, yend = Spend), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  transition_reveal(Year) + 
  coord_cartesian(clip = 'off') + 
  labs(title = NULL, y = 'Transfer Spending (€m)') + 
  scale_color_calc(name = NULL)+
  theme_ipsum(base_size = 25, axis_title_size = 25)+
  theme(legend.position="bottom", axis.text.x = element_text(angle=45))

animate(spend_plot, height = 800, width =800, nframes = 100, duration = 20)
