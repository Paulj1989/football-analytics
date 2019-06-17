rm(list=ls())
library(foreign)
library(tidyverse)
library(ggpubr)
library(hrbrthemes)
library(ineq)
library(gganimate)
library(gifski)
library(ggthemes)
library(ggrepel)
library(directlabels)

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


animate(my.animation, height = 1200, width = 1000, nframes = 100, duration = 20)
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
spend$Year <- as.numeric(spend$Year)

spend_plot <- spend %>%
  ggplot(aes(Season, Spend, group = League, color = League)) +
  geom_line(size = 1.2) +
  geom_text(aes(x = max(Season), label = League), size = 10, hjust = -0.1) +
  geom_segment(aes(xend = max(Season), yend = Spend, group = League), linetype = 2, alpha = 0.5)+
  transition_reveal(Year, keep_last = FALSE) + 
  coord_cartesian(clip = 'off') + 
  labs(title = NULL, y = 'Transfer Spending (€m)') + 
  scale_color_calc(name = NULL)+
  theme_ipsum(base_size = 25, axis_title_size = 25)+
  theme(legend.position="none", axis.text.x = element_text(angle=45),
        plot.margin = margin(50, 200, 50, 50))


animate(spend_plot, height = 1000, width = 1000, end_pause = 30)
anim_save("spend.gif")

pts_all <- ggplot(big3)+
  geom_density(aes(pts, fill = country), alpha=0.6)+
  theme_ipsum(base_size = 18, axis_title_size = 18)+
  labs(y = "", x = "Points")+
  xlim(0,120)+
  scale_fill_calc(name=NULL,
                  labels=c("Bundesliga", "Premier League", "La Liga")) +
  transition_reveal(year) + 
  coord_cartesian(clip = 'off')
pts_all
