library(tidyverse)
library(ggthemr)
library(ggpubr)
library(ggsci)
library(lubridate)
library(scales)
library(yarrr)
library(paletteer)

spi <- read_csv("~/Documents/spi.csv")
teams <- read_csv("~/Documents/spi_teams.csv")

ggthemr('pale',layout = "scientific")

theme <-theme(axis.title = element_text(size = 15),
              axis.text = element_text(size = 15),
              panel.grid.minor = element_blank(),
              legend.position = "bottom",
              legend.background = element_blank(),
              legend.direction="horizontal",
              legend.text = element_text(size = 15),
              legend.key.size = unit(1.5, "cm", ),
              legend.margin=margin(t = 0, b = -0.5, unit='cm'),
              plot.title = element_text(size = 25, margin = margin(b = 10)),
              plot.subtitle = element_text(size = 15, color = "darkslategrey", margin = margin(b = 25)),
              plot.caption = element_text(size = 15, margin = margin(t = 10), color = "grey70", hjust = 0))

dortmund_home <- spi %>%
  filter(team1 == "Borussia Dortmund") %>%
  select(date, team = team1, opponent = team2, spi = spi1, prob1, prob2, probtie, proj_score1,
         proj_score2, importance1, importance2, 
         score1, score2, xg1, xg2) %>%
  add_column("home" = 1)

dortmund_away <- spi %>%
  filter(team2 == "Borussia Dortmund") %>%
  select(date, opponent = team1, team = team2, spi = spi2, prob1, prob2, probtie, proj_score1,
         proj_score2, importance1, importance2, 
         score1, score2, xg1, xg2) %>%
  add_column("home" = 0)

dortmund_full <- full_join(dortmund_home, dortmund_away) %>% 
  na.omit()

# Bayern

bayern_home <- spi %>%
  filter(team1 == "Bayern Munich") %>%
  select(date, team = team1, opponent = team2, spi = spi1, prob1, prob2, probtie, proj_score1,
         proj_score2, importance1, importance2, 
         score1, score2, xg1, xg2) %>%
  add_column("home" = 1)

bayern_away <- spi %>%
  filter(team2 == "Bayern Munich") %>%
  select(date, opponent = team1, team = team2, spi = spi2, prob1, prob2, probtie, proj_score1,
         proj_score2, importance1, importance2, 
         score1, score2, xg1, xg2) %>%
  add_column("home" = 0)

bayern_full <- full_join(bayern_home, bayern_away) %>% 
  na.omit()

#Leipzig

leipzig_home <- spi %>%
  filter(team1 == "RB Leipzig") %>%
  select(date, team = team1, opponent = team2, spi = spi1, prob1, prob2, probtie, proj_score1,
         proj_score2, importance1, importance2, 
         score1, score2, xg1, xg2) %>%
  add_column("home" = 1)

leipzig_away <- spi %>%
  filter(team2 == "RB Leipzig") %>%
  select(date, opponent = team1, team = team2, spi = spi2, prob1, prob2, probtie, proj_score1,
         proj_score2, importance1, importance2, 
         score1, score2, xg1, xg2) %>%
  add_column("home" = 0)

leipzig_full <- full_join(leipzig_home, leipzig_away) %>% 
  na.omit()
  
germany_full <- full_join(germany_full, bayern_full) %>%
  na.omit()


  
## Dortmund Quality ----------

ggplot(germany_full, aes(date, spi, color = team)) +
  geom_line(size = 1) +
  labs(title="Quality of Bundesliga's Contenders 2016 - 2019",
       subtitle = "Soccer Power Index Per Game Comparing Bayern, RB Leipzig, and Dortmund",
       caption = "Source: FiveThirtyEight",
       color = NULL, x = NULL, y = "Soccer Power Index (SPI)") +
  scale_color_manual(values = c("#145FA0", "#FCBD03", "#C6374D")) +
  theme(legend.position="bottom",
        plot.margin = margin(t = 10, b = 10, r = 15, l = 10)) + theme  + expand_limits(y = c(60, 90)) + 
  scale_x_date(date_breaks = "3 month", 
               labels=date_format("%b %Y"),
               limits = as.Date(c('2016-08-27','2019-11-09')))


ggplot(germany_full, aes(date, spi, color = team)) +
  geom_line(size = 1) +
  labs(title="Quality of Bundesliga's Contenders 2016 - 2019",
       subtitle = "Soccer Power Index Per Game Comparing Bayern, RB Leipzig, and Dortmund",
       caption = "Source: FiveThiryEight",
       color = NULL, x = NULL, y = "Soccer Power Index (SPI)") +
  scale_color_manual(values = c("#145FA0", "#FCBD03", "#C6374D"))+
  theme(legend.position="bottom",
        plot.margin = margin(30, 30, 30, 30)) + theme  + expand_limits(y = c(60, 90)) + 
  scale_x_date(date_breaks = "3 month", 
               labels=date_format("%b %Y"),
               limits = as.Date(c('2016-08-27','2019-11-09')))
  
## League Quality ----------
  
spi$League <- as.factor(spi$League)
teams$League <- as.factor(teams$League)

ggplot(spi, aes(Season, SPI, group = League, color = League)) +
  geom_line(size = 1)+
  labs(title="League Quality 2016 - 2019",
       subtitle = "Quality = Mean Average Soccer Power Index Per League-Season",
       caption = "Source: FiveThiryEight",
       color = NULL, x = NULL, y = "Soccer Power Index (SPI)") +
  scale_color_locuszoom()+
  theme(legend.position="bottom",
        plot.margin = margin(30, 30, 30, 30)) + theme 


ggplot(teams, aes(SPI, fill = Season))+
  geom_density(alpha=0.5)+
  theme(legend.title = element_blank(),
        legend.key.size = unit(1, "cm"))+
  xlim(0,120)+
  labs(title = "", y = "", x = "SPI")+
  scale_fill_locuszoom() + facet_wrap(~League)

p1 <- teams %>%
  filter(League == "Premier League") %>%
ggplot(aes(Season, SPI, group = Team, color = Team)) +
  geom_line(size = 1)+
  labs(title="Team Quality 2016 - 2019",
       subtitle = "Premier League",
       caption = "Source: FiveThiryEight",
       color = NULL, x = NULL, y = "Soccer Power Index (SPI)") +
  scale_color_locuszoom()+
  theme(legend.position="bottom",
        plot.margin = margin(30, 30, 30, 30)) + theme + expand_limits(y = c(60, 90))


p2 <- teams %>%
  filter(League != "Premier League") %>%
  ggplot(aes(Season, SPI, group = Team, color = Team)) +
  geom_line(size = 1)+
  labs(title="",
       subtitle = "La Liga & Bundesliga",
       color = NULL, x = NULL, y = NULL) +
  scale_color_locuszoom()+
  theme(legend.position="bottom",
        plot.margin = margin(30, 30, 30, 30)) + theme + expand_limits(y = c(60, 90))

ggarrange(p1, p2, align = "hv")
