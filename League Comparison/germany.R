rm(list=ls())
library(engsoccerdata)
library(regista)
library(dplyr)
library(tidyverse)
library(broom)
library(hrbrthemes)
library(ggrepel)
library(poisbinom)

#Loading relevant data

buli <- read.csv("~/Documents/Stochastic Soccer/germany.csv")

buli$winprob <- buli$hxg/(buli$hxg+buli$vxg)
buli$lossprob <- buli$vxg/(buli$hxg+buli$vxg)

germany %>%
  filter (Season == 2012,
          tier==1) %>%
  mutate(home = as.character(home),
         visitor = as.character(visitor))-> germany_2012

germany %>%
  filter (Season == 2013,
          tier==1) %>%
  mutate(home = as.character(home),
         visitor = as.character(visitor))-> germany_2013

germany %>%
  filter (Season == 2014,
          tier==1) %>%
  mutate(home = as.character(home),
         visitor = as.character(visitor))-> germany_2014

germany %>%
  filter (Season == 2015,
          tier==1) %>%
  mutate(home = as.character(home),
         visitor = as.character(visitor))-> germany_2015

germany %>%
  filter (Season == 2016,
          tier==1) %>%
  mutate(home = as.character(home),
         visitor = as.character(visitor))-> germany_2016

#Merging data

germany <-rbind(germany_2016,germany_2015,germany_2014,germany_2013,germany_2012)

rm(germany_2016,germany_2015,germany_2014,germany_2013,germany_2012)

germany$home <- as.factor(germany$home)
germany$visitor <- as.factor(germany$visitor)

#Model

fit <- dixoncoles(hgoal, vgoal, home, visitor, data = germany)
print(fit)


fitxg <- dixoncoles_ext(hgoal ~ off(home) + def(away) + hfa + hxg + 0,
                      vgoal ~ off(away) + def(home) + vxg,
                      weights = 1,
                      data = buli)


parameters <- tibble::tibble(
  parameter = names(fit$par),
  value     = fit$par
)

parameters

#Predictions

to_predict <- germany
to_predict$predictions <- predict(fit, newdata = germany)

to_predict



#Predicted probabilities

preds<- fit %>%
  augment(newdata = germany, type = "outcomes") %>%
  unnest() %>%
  mutate(prob = scales::percent(prob, 2)) %>%
  spread(outcome, prob) %>%
  select(home, visitor, home_win, draw, away_win, FT)

preds

#Parse team parameter estimates

estimates <-
  broom::tidy(fit) %>%
  filter(parameter != "rho",
         parameter != "hfa") %>%
  mutate(value = exp(value))

estimates

#Mapping team ratings


plot<- estimates %>%
  spread(parameter, value) %>%
  ggplot(aes(x = def, y = off)) +
  geom_text_repel(aes(label = team)) +
  scale_x_continuous(limits = c(0, 2))+
  ylim(0,2)+
  theme_ipsum(plot_title_size = 18, base_size = 10, axis_title_size = 18, grid=F)+
  labs(title = "German team ratings",
       subtitle = str_glue("From 2012 to 2016"),
       x = "Defence",
       y = "Attack")

plot


#Using xG

#Link to resources: http://www.statsandsnakeoil.com/2019/01/06/predicting-the-premier-league-with-dixon-coles-and-xg/

#http://www.statsandsnakeoil.com/2019/01/01/predicting-the-premier-league-with-dixon-coles/

#http://www.robert-hickman.eu/post/wsl-prediction-1/

#Should I be using the weights for home and away and the attack/defense parameters to calculate the probability of outcomes?