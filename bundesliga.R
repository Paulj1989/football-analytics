rm(list=ls())
library(foreign)
library(tidyverse)
library(broom)
library(hrbrthemes)
library(ggrepel)
library(ggpubr)
library(MASS)
library(gtools)
library(jtools)
library(ggstance)
library(Metrics)
library(gtable)
library(viridis)
library(stargazer)

#Loading relevant data

germany <- read.csv("~/Documents/FTW/germany.csv")

#Transforming year variable to a factor

germany$year <- as.factor(germany$year)

#Testing distributions

ggdensity(germany$pts)
ggdensity(germany$net.spend) 
ggdensity(germany$spend)
ggdensity(germany$value) 
ggdensity(germany$absences)
ggdensity(germany$xg)
ggdensity(germany$gf)

#Distributions split by year

p1 <- ggplot(germany, aes(pts, fill = year))+
  geom_density(alpha=0.5)+
  theme_ipsum(plot_title_face = "plain", plot_title_size = 30, base_size = 18, axis_title_size = 20)+
  theme(legend.title = element_blank(),
        legend.key.size = unit(1, "cm"))+
  xlim(0,120)+
  labs(title = "", y = "", x = "Points")+
  scale_fill_viridis_d(labels = c("14/15", "15/16", "16/17", "17/18", "18/19"))
p1

p2 <- ggplot(germany, aes(pts))+
  geom_density(alpha=0.5)+
  theme_ipsum(base_size = 18, axis_title_size = 18)+
  xlim(0,120)+
  labs(y = "", x = "Points")+
  scale_fill_viridis_d()+
  theme(legend.key.size = unit(0.7, "cm"))
p2

ggarrange(p1,p2,legend="right", align = "hv")

p3 <- ggplot(germany, aes(spend, fill = year))+
  geom_density(alpha=0.5)+
  theme_ipsum(plot_title_face = "plain", plot_title_size = 30, base_size = 18, axis_title_size = 20, grid = FALSE)+
  theme(legend.title = element_blank(),
        legend.key.size = unit(1, "cm"))+
  labs(title = NULL, y = "", x = "Transfer Expenditure (Millions €)")+
  scale_fill_viridis_d(labels = c("14/15", "15/16", "16/17", "17/18", "18/19"))
p3

p4 <- ggplot(germany, aes(net.spend, fill = year))+
  geom_density(alpha=0.5)+
  theme_ipsum(plot_title_face = "plain", plot_title_size = 30, base_size = 18, axis_title_size = 20, grid = FALSE)+
  theme(legend.title = element_blank(),
        legend.key.size = unit(1, "cm"))+
  labs(title = NULL, y = "", x = "Net Spending (Millions €)")+
  scale_fill_viridis_d(labels = c("14/15", "15/16", "16/17", "17/18", "18/19"))
p4

ggarrange(p3,p4, common.legend = TRUE, legend="bottom", align = "hv", labels = c("Distribution of BuLi spending"), hjust = -1.8, vjust = 1.25, font.label = list(size = 25, color = "black", face = "plain", family = "roboto", alpha = 0.8))

#Linear regressions

lm1 <- lm(pts ~ xg, data = germany)
summary(lm1)

lm2 <- lm(pts ~ spend + absences, data = germany)
summary(lm2)

lm3 <- lm(pts ~ value + absences, data = germany)
summary(lm3)

#Poisson or Negative Binomial regresions

glm1 <- glm(xg ~ spend + absences, family = neg.bin(theta = 1), data = germany)
summary(glm1)

glm2 <- glm(xg ~ value + absences, family = neg.bin(theta = 1) ,data = germany)
summary(glm2)

stargazer(lm1,lm2,lm3, style = "ajps")
stargazer(glm1, glm2, style = "ajps")

#Plots to help visualize interesting stories
int.plot1 <- ggplot(germany, aes(spend, pts))+
  geom_point(aes(fill = value), alpha = 1, size = 4, shape = 21)+
  labs(y = "Points", x = "Transfer Spending (€m)", fill = "Market Value (€m)")+
  geom_smooth(method = "lm", color = "black")+
  theme_ipsum(base_size = 18, axis_title_size = 18)+
  theme(legend.key.size = unit(0.7, "cm"))+
  scale_fill_viridis()

int.plot1
