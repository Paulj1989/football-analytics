rm(list=ls())
library(foreign)
library(tidyverse)
library(ggthemes)
library(hrbrthemes)
library(gganimate)

ucl<- read_csv("~/Documents/GitHub Directory/ftw/ucl.csv")

ucl %>%
  ggplot(aes(x = reorder(Season, Year), Pts, group=League, color=League)) +
  geom_line(size = 0.8, alpha = 0.6)+
  geom_point(size=2, alpha = 0.7)+ xlab("Season") + ylab("Champions League Performance")+
  labs(color = "League")+
  scale_color_calc(name = NULL)+
  theme_ipsum(base_size = 25, axis_title_size = 25, grid = FALSE)+
  theme(legend.position="bottom", axis.text.x = element_text(angle=45),
        plot.margin = margin(50, 50, 0, 50))

