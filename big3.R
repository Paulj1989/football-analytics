rm(list=ls())
library(foreign)
library(tidyverse)
library(ggpubr)
library(hrbrthemes)
library(ineq)
install.packages("gganimate")
library(gganimate)
library(gapminder)

big3 <- read_csv("~/Documents/FTW/big3.csv")
big3$year <- as.factor(big3$year)

#Performance stats

xg_all <- ggplot(big3)+
  geom_density(aes(xg, fill = country), alpha=0.6)+
  theme_ipsum(base_size = 18, axis_title_size = 18)+
  xlim(0,120)+
  labs(y = "", x = "xG")+
  scale_fill_viridis_d(name=NULL,
                      labels=c("Bundesliga", "Premier League", "La Liga"))
xg_all

pts_all <- ggplot(big3)+
  geom_density(aes(pts, fill = country), alpha=0.6)+
  theme_ipsum(base_size = 18, axis_title_size = 18)+
  labs(y = "", x = "Points")+
  xlim(0,120)+
  scale_fill_viridis_d(name=NULL,
                      labels=c("Bundesliga", "Premier League", "La Liga"))
pts_all

gf_all <- ggplot(data = big3)+
  geom_density(aes(x = gf, fill = country), alpha=0.5)+
  theme_ipsum(base_size = 18, axis_title_size = 18)+
  labs(y = "", x = "Goals Scored")+
  xlim(0,150)+
  scale_fill_viridis_d(name=NULL,
                    labels=c("Bundesliga", "Premier League", "La Liga"))
gf_all


ga_all <- ggplot(data = big3)+
  geom_density(aes(x = ga, fill = country), alpha=0.5)+
  theme_ipsum(base_size = 18, axis_title_size = 18)+
  labs(y = "", x = "Goals Conceded")+
  xlim(0,100)+
  scale_fill_viridis_d(name=NULL,
                      labels=c("Bundesliga", "Premier League", "La Liga"))
ga_all

ggarrange(xg_all, pts_all, gf_all, ga_all, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

#Spending stats

spend_all <- ggplot(data = big3)+
  geom_density(aes(x = spend, fill = country), alpha=0.3)+
  theme_ipsum(base_size = 18, axis_title_size = 18)+
  xlim(0,400)+
  labs(y = "", x = "Transfer Spending (€m)")+
  scale_fill_discrete(name=NULL,
                      labels=c("Bundesliga", "Premier League", "La Liga"))
spend_all

value_all <- ggplot(data = big3)+
  geom_density(aes(x = value, fill = country), alpha=0.3)+
  theme_ipsum(base_size = 18, axis_title_size = 18)+
  xlim(0,1500)+
  labs(y = "", x = "Market Value (€m)")+
  scale_fill_discrete(name=NULL,
                      labels=c("Bundesliga", "Premier League", "La Liga"))
value_all

ggarrange(spend_all, value_all, common.legend = TRUE, legend="bottom")

#Distributions split by year and league

big3$year <- as.factor(big3$year)

germany <- filter(big3, country == "Bundesliga")
england <- filter(big3, country == "Premier League")
spain <- filter(big3, country == "La Liga")


eng_goals <- ggplot(england, aes(gf, fill = year))+
  geom_dotplot(method = "histodot", alpha=0.5, binwidth = 1, dots = 1)+
  theme_ipsum(plot_title_face = "plain", plot_title_size = 20, base_size = 18, axis_title_size = 20)+
  theme(legend.title = element_blank(),
        legend.key.size = unit(1, "cm"))+
  labs(title = "Premier League", y = "", x = "Goals Scored")+
  scale_fill_viridis_d(labels = c("14/15", "15/16", "16/17", "17/18", "18/19"))
eng_goals


  
  


eng_goals <- ggplot(england, aes(gf, xg, fill = year, frame = year)) +
  geom_point() +
  geom_smooth(aes(), 
              method = "lm", 
              show.legend = FALSE) +
  facet_wrap(~country, scales = "free")

gganimate(eng_goals, interval=0.2)

#Spending

eng_spend <- ggplot(england, aes(spend, fill = year))+
  geom_density(alpha=0.5)+
  theme_ipsum(plot_title_face = "plain", plot_title_size = 20, base_size = 18, axis_title_size = 20)+
  theme(legend.title = element_blank(),
        legend.key.size = unit(1, "cm"))+
  xlim(0,400)+
  ylim(0,0.04)+
  labs(title = "Premier League", y = "", x = "Transfer Expenditure (€m)")+
  scale_fill_viridis_d(labels = c("14/15", "15/16", "16/17", "17/18", "18/19"))
eng_spend

de_spend <- ggplot(germany, aes(spend, fill = year))+
  geom_density(alpha=0.5)+
  theme_ipsum(plot_title_face = "plain", plot_title_size = 20, base_size = 18, axis_title_size = 20)+
  theme(legend.title = element_blank(),
        legend.key.size = unit(1, "cm"))+
  xlim(0,400)+
  ylim(0,0.04)+
  labs(title = "Bundesliga", y = "", x = "Transfer Expenditure (€m)")+
  scale_fill_viridis_d(labels = c("14/15", "15/16", "16/17", "17/18", "18/19"))
de_spend

esp_spend <- ggplot(spain, aes(spend, fill = year))+
  geom_density(alpha=0.5)+
  theme_ipsum(plot_title_face = "plain", plot_title_size = 20, base_size = 18, axis_title_size = 20)+
  theme(legend.title = element_blank(),
        legend.key.size = unit(1, "cm"))+
  xlim(0,400)+
  ylim(0,0.04)+
  labs(title = "La Liga", y = "", x = "Transfer Expenditure (€m)")+
  scale_fill_viridis_d(labels = c("14/15", "15/16", "16/17", "17/18", "18/19"))
esp_spend

ggarrange(eng_spend, de_spend, esp_spend, nrow = 1, ncol = 3, common.legend = TRUE, legend="bottom", align = "hv")

#Value

eng_value <- ggplot(england, aes(value, fill = year))+
  geom_density(alpha=0.5)+
  theme_ipsum(plot_title_face = "plain", plot_title_size = 20, base_size = 18, axis_title_size = 20)+
  theme(legend.title = element_blank(),
        legend.key.size = unit(1, "cm"))+
  xlim(0,1500)+
  ylim(0,0.01)+
  labs(title = "Premier League", y = "", x = "Market Value (€m)")+
  scale_fill_viridis_d(labels = c("14/15", "15/16", "16/17", "17/18", "18/19"))
eng_value

de_value <- ggplot(germany, aes(value, fill = year))+
  geom_density(alpha=0.5)+
  theme_ipsum(plot_title_face = "plain", plot_title_size = 20, base_size = 18, axis_title_size = 20)+
  theme(legend.title = element_blank(),
        legend.key.size = unit(1, "cm"))+
  xlim(0,1500)+
  ylim(0,0.01)+
  labs(title = "Bundesliga", y = "", x = "Market Value (€m)")+
  scale_fill_viridis_d(labels = c("14/15", "15/16", "16/17", "17/18", "18/19"))
de_value

esp_value <- ggplot(spain, aes(value, fill = year))+
  geom_density(alpha=0.5)+
  theme_ipsum(plot_title_face = "plain", plot_title_size = 20, base_size = 18, axis_title_size = 20)+
  theme(legend.title = element_blank(),
        legend.key.size = unit(1, "cm"))+
  xlim(0,1500)+
  ylim(0,0.01)+
  labs(title = "La Liga", y = "", x = "Market Value (€m)")+
  scale_fill_viridis_d(labels = c("14/15", "15/16", "16/17", "17/18", "18/19"))
esp_value

ggarrange(eng_value, de_value, esp_value, nrow = 1, ncol = 3, common.legend = TRUE, legend="bottom", align = "hv")


# Facets

facet_pts <- ggplot(big3, aes(pts, fill = year))+
  geom_density(alpha=0.5)+
  theme_ipsum(plot_title_face = "plain", 
              plot_title_size = 20, 
              base_size = 18, 
              axis_title_size = 20,
              strip_text_size = 20)+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.box.spacing = unit(0.005, "cm"),
        legend.key.size = unit(0.7, "cm"))+
  xlim(16,100)+
  coord_cartesian(xlim = c(19,99.5))+
  labs(title = NULL, y = NULL, x = "Points")+
  scale_fill_viridis_d(labels = c("14/15", "15/16", "16/17", "17/18", "18/19")) + 
  facet_wrap(~country, nrow = 3)
facet_pts
