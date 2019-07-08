rm(list=ls())
library(foreign)
library(tidyverse)
library(ggthemes)
library(gganimate)

ucl<- read_csv("~/Documents/GitHub Directory/ftw/ucl.csv")


ggthemr('greyscale', layout = "scientific", text_size = 25)

ucl %>%
  ggplot(aes(x = reorder(Season, Year), Pts, group=League, color=League)) +
  geom_line(size = 0.8, alpha = 1)+
  geom_point(size=4, alpha = 1) +
  labs(title = "Champions League Performance", x = NULL, y = NULL, color = NULL)+
  scale_color_lancet()+
  theme(plot.title = element_text(face = "plain"), 
        legend.position="bottom", 
        axis.text.x = element_text(angle=45, size = 18))

#Top4

top4 <- read_csv("~/Documents/GitHub Directory/ftw/top4.csv")
top4$Year <- as.numeric(top4$Year)

top4_plot <- top4 %>%
  ggplot(aes(Year, Average, group = League, color = League)) +
  geom_line(size = 0.8) +
  labs(title = "Top 4 Mean Average Revenue", subtitle = "Rankings from Deloitte's Football Money League",
       x = NULL, y = 'Revenue (€m)', color = NULL) + 
  scale_color_lancet()+
  theme(plot.title = element_text(face="plain"),
        plot.subtitle = element_text(size = 18),
        legend.position="bottom",
        axis.text.x = element_text(angle=45))+
  scale_x_continuous(breaks=seq(2012, 2019, 1))

top4_plot

#Rich List

rich_list <- read_csv("~/Documents/GitHub Directory/ftw/rich_list.csv")
rich_list$Year <- as.numeric(rich_list$Year)

rich_plot <- rich_list %>%
  ggplot(aes(Year, Revenue, group = League, color = League)) +
  geom_line(size = 0.8) +
  labs(title = "Total Revenue of Football's Richest Teams",
       subtitle = " Including all teams ranked in the Money League",
       color = NULL, x = NULL, y = 'Revenue (€m)') + 
  scale_color_lancet()+
  theme(plot.title = element_text(face="plain"),
        plot.subtitle = element_text(size = 18),
        legend.position="bottom",
        axis.text.x = element_text(angle=45))+
  scale_x_continuous(breaks=seq(2012, 2018, 1))

rich_plot
