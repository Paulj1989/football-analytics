rm(list=ls())
library(foreign)
library(tidyverse)
library(ggpubr)
library(hrbrthemes)
library(ineq)
library(gganimate)


big3 <- read_csv("~/Documents/FTW/big3.csv")
big3$year <- as.factor(big3$year)

goals_league <- big3 %>%
  group_by(country, year, games) %>%
  summarise(goals = sum(gf)) %>%
  mutate(goals_pg = goals/games)

goals_league

#Performance stats

gf_all <- ggplot(goals_league, aes(year, goals_pg, fill = country))+
  geom_col(position = "dodge", width = .8, alpha=0.8)+
  theme(plot.title = element_text(face = "plain"), 
        legend.position = "bottom", 
        legend.box.spacing = unit(0.005, "cm"),
        legend.key.size = unit(0.7, "cm"))+
  labs(title = "Average Goals Per Game", fill = NULL, y = NULL, x = NULL)+
  scale_x_discrete(labels=c("14/15", "15/16", "16/17", "17/18", "18/19"))+
  scale_fill_lancet()
gf_all



 
xg_all <- ggplot(big3)+
   geom_density(aes(xg, color = country), alpha=0.6)+
   theme(legend.position = "bottom", 
         legend.box.spacing = unit(0.005, "cm"),
         legend.key.size = unit(0.7, "cm"))+
   xlim(0,120)+
   labs(y = "", x = "xG")+
   scale_color_lancet(name=NULL,
                      labels=c("Bundesliga", "Premier League", "La Liga"))
xg_all
 
pts_all <- ggplot(data = big3)+
  geom_density(aes(x = pts, color = country), alpha=0.5)+
  theme(legend.position = "bottom", 
        legend.box.spacing = unit(0.005, "cm"),
        legend.key.size = unit(0.7, "cm"))+
  labs(y = "", x = "Points")+
  xlim(0,150)+
  scale_color_lancet(name=NULL,
                     labels=c("Bundesliga", "Premier League", "La Liga"))
pts_all


ggarrange(xg_all, pts_all, gf_all, ncol=3, common.legend = TRUE, legend="bottom")



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
