rm(list=ls())
library(foreign)
library(tidyverse)
library(ggthemes)
library(hrbrthemes)
library(gganimate)

ucl<- read_csv("~/Documents/GitHub Directory/ftw/ucl.csv")


ggthemr('greyscale', layout = "scientific", text_size = 30)

ucl %>%
  ggplot(aes(x = reorder(Season, Year), Pts, group=League, color=League)) +
  geom_line(size = 0.8, alpha = 1)+
  geom_point(size=4, alpha = 1)+ xlab("Season") + ylab("Champions League Performance")+
  labs(color = "League")+
  scale_color_lancet()+
  theme(text = element_text(size=25), legend.position="bottom", axis.text.x = element_text(angle=45),
        plot.margin = margin(50, 50, 0, 50))

