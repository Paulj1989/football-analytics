rm(list=ls())
library(foreign)
library(tidyverse)
library(ggpubr)
library(gganimate)
library(gifski)
library(ggthemes)
library(ggrepel)
library(directlabels)

time3 <- read_csv("~/Documents/GitHub Directory/ftw/time3.csv")

ggthemr('greyscale', layout = "scientific", text_size = 25)

#Value/Points animation

value_plot <- ggplot(time3, aes(value, pts, color = country)) +
  geom_point(alpha = 0.7, size = 12) +
  theme(plot.title = element_text(face = "plain"), 
        legend.position = "bottom", 
        legend.box.spacing = unit(0.005, "cm"),
        legend.key.size = unit(0.7, "cm"),
        plot.margin = margin(5.5, 40, 5.5, 5.5))+
  scale_color_lancet(labels=c("Bundesliga", "Premier League", "La Liga"))+
  labs(title = "The Growth of Market Value by Team Points Totals",
       subtitle = 'Season: {closest_state}',
       x = 'Market Value (€m)', y = 'Points',
       color = NULL) +
  transition_states(year, state_length = 0) +
  coord_cartesian(clip = 'off') + 
  shadow_mark(alpha = 0.3, size = 5)+
ease_aes('linear')


animate(value_plot, height = 1000, width = 1000, nframes = 100, duration = 20)
anim_save("value.gif")

#Transfer spending animation

spend <- read_csv("~/Documents/GitHub Directory/ftw/spend.csv")
spend$Year <- as.numeric(spend$Year)

spend_plot <- spend %>%
  ggplot(aes(Season, Spend, group = League, color = League)) +
  geom_line(size = 1) +
  geom_text(aes(x = max(Season), label = League), size = 10, hjust = -0.1) +
  geom_segment(aes(xend = max(Season), yend = Spend, group = League), linetype = 2, alpha = 0.5)+
  transition_reveal(Year, keep_last = FALSE) + 
  coord_cartesian(clip = 'off') + 
  labs(title = "The Growth of Transfer Spending", x = NULL, y = 'Transfer Spending (€m)') + 
  scale_color_lancet()+
  theme(plot.title = element_text(face = "plain"), 
        legend.position="none",
        axis.text.x = element_text(angle=45),
        plot.margin = margin(50, 200, 50, 50))



animate(spend_plot, height = 1000, width = 1000, end_pause = 5)
anim_save("spend.gif")

