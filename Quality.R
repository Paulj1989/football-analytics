rm(list=ls())
library(foreign)
library(tidyverse)
library(reshape)
library(hrbrthemes)

ucl<- read_csv("~/Documents/FTW/Changes/ucl.csv")

n <- ucl %>%
  gather(2:27, key = "Season", value = "Pts")

n %>%
  ggplot(aes(Season, Pts, group=League, color=League)) +
  geom_line(size = 1, alpha = 0.6)+
  geom_point(size=4, alpha = 0.6)+xlab("Year")+ylab("Champions League Performance")+
  theme_ipsum(base_size = 18, axis_title_size = 18)+
  labs(color = "League")+
  scale_color_brewer(name = NULL, palette = "Set1")+
  theme(axis.text.x = element_text(angle=45))

