library(foreign)
library(tidyverse)
library(ggthemr)
library(ggpubr)
library(ggsci)
library(stargazer)

# Data ----------

season <- read_csv("season.csv")
points <- read_csv("~/Documents/FTW/probs.csv")
bayernxg <- read_csv("~/Documents/FTW/bayernxg.csv")

dortmund <- points %>%
  filter(Team == "Borussia Dortmund")

bayern <- points %>%
  filter(Team == "Bayern Munchen")


hinrunde <- season %>%
  filter(Game < 18)

ruckrunde <- season %>%
  filter(Game > 17)

dortmund <- dortmund %>%
  mutate(cumprob = cumsum(Probable_Points)) %>%
  mutate(cumact = cumsum(Actual_Points))

bayern <- bayern %>%
  mutate(cumprob = cumsum(Probable_Points)) %>%
  mutate(cumact = cumsum(Actual_Points))

points <- full_join(bayern, dortmund)

# Distributions ----------

ggplot(season, aes(xDiff)) +
  geom_density()

ggplot(season, aes(Points)) +
  geom_density()


# Regression ----------

olsAbsence <- lm(Errors ~ Absence + Touches + Distance, data = season)
summary(olsAbsence)


olsPaco <- lm(xG ~ Paco_xG + Difficulty + Home, data = season)
summary(olsPaco)

olsDistance <- lm(xDiff ~ Distance + Difficulty + Home, data = season)
summary(olsDistance)

olsErrors1 <- lm(Points ~ Errors + Difficulty + Home, data = season)
summary(olsErrors1)

olsErrors2 <- lm(xG ~ Errors + Difficulty + Home, data = season)
summary(olsErrors2)

olsErrors3 <- lm(xGA ~ Errors + Difficulty + Home, data = season)
summary(olsErrors3)

stargazer(olsErrors1, olsErrors3, digits = 2,
          title = "The Effect of Errors on Performance and Results")

# Correlations ----------

cor(season$Game, season$xDiff)

cor(season$Game, season$Distance)

cor(season$Game, season$Errors)

cor(season$Game, season$Witsel, use = "complete.obs")

cor(season$Game, season$Paco_xG_Mins, use = "complete.obs")

  # t-tests ----------

xDiff_paired <- t.test(hinrunde$xDiff, ruckrunde$xDiff, paired = TRUE, alternative = "two.sided")

absence_paired <- t.test(hinrunde$Absence2, ruckrunde$Absence2, paired = TRUE, alternative = "two.sided")

xG_paired <- t.test(hinrunde$xG, ruckrunde$xG, paired = TRUE, alternative = "two.sided")

xGA_paired <- t.test(hinrunde$xGA, ruckrunde$xGA, paired = TRUE, alternative = "two.sided")

witsel_paired <- t.test(hinrunde$Witsel, ruckrunde$Witsel, paired = TRUE, alternative = "two.sided")

errors_paired <- t.test(hinrunde$Errors, ruckrunde$Errors, paired = TRUE, alternative = "two.sided")

touches_paired <- t.test(hinrunde$Touches, ruckrunde$Touches, paired = TRUE, alternative = "two.sided")

shots_paired <- t.test(hinrunde$Shots, ruckrunde$Shots, paired = TRUE, alternative = "two.sided")

conversion_paired <- t.test(hinrunde$`Conversion Rate`, ruckrunde$`Conversion Rate`, paired = TRUE, alternative = "two.sided")

distance_paired <- t.test(hinrunde$Distance, ruckrunde$Distance, paired = TRUE, alternative = "two.sided")

paco_paired <- t.test(hinrunde$Paco_xG_Mins, ruckrunde$Paco_xG_Mins, paired = TRUE, alternative = "two.sided")

# Plots ----------

ggthemr('greyscale', layout = "scientific", text_size = 25)

ggplot(season, aes(Points, Possession)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(hinrunde, aes(sum(hinrunde$xDiff))) +
  geom_bar() +
  geom_bar(ruckrunde, aes(sum(ruckrunde$xDiff)))

# Points
points2 <- points %>%
  gather(cumprob, cumact, key = type, value = points_total)

ggplot(points2, aes(Game, points_total, color = Team, linetype = type)) +
  geom_line(size = 1, alpha = 0.8) +
  scale_x_continuous(breaks=seq(2,34,2))+
  labs(title = "Cumulative Points Totals 18/19",
         subtitle = "Simulated vs Actual Points Totals for BVB & Bayern",
         x = "Match", y = "Points", linetype = NULL, color = NULL) +
  scale_color_lancet(labels = c("Bayern Munich","Borussia Dortmund")) +
  scale_linetype_discrete(labels = c("Actual","Simulated")) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 18, angle = 45),
        plot.title = element_text(face = "plain"),
        legend.position = "bottom",
        legend.box.spacing = unit(0.2, "cm"),
        legend.key.size = unit(0.8, "cm"))


# xG
season2 <- season %>%
    gather(xG, xGA, key = xG_Type, value = xG_Total)

ggplot(season2, aes(Game, xG_Total, fill = xG_Type)) +
  geom_col(position = "dodge", color = "black") +
  scale_fill_lancet() +
  scale_x_continuous(breaks=seq(1,34,1))+
  labs(title = "Borussia Dortmund Performance 18/19",
       subtitle = "Expected Goals For & Against Per Match ",
       x = "Match", y = "xG/xGA",
       fill = NULL) +
  theme(legend.position="bottom")

ggplot(season2, aes(Game, xG_Total, color = xG_Type)) +
  geom_point(size = 2) +
  stat_smooth(se = FALSE) +
  scale_x_continuous(breaks=seq(1,34,1))+
  scale_color_lancet() +
  labs(title = "Borussia Dortmund Performance 18/19",
       subtitle = "Expected Goals For & Against Per Match ",
       x = "Match", y = "xG/xGA",
       color = NULL) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 18, angle = 45),
        plot.title = element_text(face = "plain"),
        legend.position = "bottom",
        legend.box.spacing = unit(0.2, "cm"),
        legend.key.size = unit(0.8, "cm"))

bayernxg2 <- bayernxg %>%
  gather(xG, xGA, key = xG_Type, value = xG_Total)

ggplot(bayernxg2, aes(Game, xG_Total, color = xG_Type)) +
  geom_point(size = 2) +
  stat_smooth(se = FALSE) +
  scale_color_lancet() +
  scale_x_continuous(breaks=seq(1,34,1))+
  labs(title = "Bayern Munich Performance 18/19",
       subtitle = "Expected Goals For & Against Per Match ",
       x = "Match", y = "xG/xGA",
       color = NULL) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 18, angle = 45),
        plot.title = element_text(face = "plain"),
        legend.position = "bottom",
        legend.box.spacing = unit(0.2, "cm"),
        legend.key.size = unit(0.8, "cm"))

# Paco

season3 <- season %>%
  gather(Paco_xG, Paco_Shots, Paco_Goals, key = Paco_Type, value = Paco_Total, na.rm = TRUE)

ggplot(season3, aes(Game, Paco_Total, color = Paco_Type), size = 1) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_continuous(breaks = seq(2, 34, 2)) +
  scale_y_continuous(breaks = seq(1, 6, 1)) +
  labs(title = "Paco Alcacer's Performance 18/19",
       subtitle = "Goals, xG & Shots",
       x = "Match", y = "Totals", linetype = NULL, color = NULL) +
  scale_color_lancet(labels = c("Goals", "Shots", "xG")) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 18, angle = 45),
        plot.title = element_text(face = "plain"),
        legend.position = "bottom",
        legend.box.spacing = unit(0.2, "cm"),
        legend.key.size = unit(0.8, "cm"))

#Absences

ggplot(season, aes(Game, Absence)) +
  geom_col(color = "black", alpha = 0.6) +
  geom_smooth(se = FALSE, color = "#ED0000FF") +
  scale_x_continuous(breaks = seq(1, 34, 1)) +
  labs(title = "Borussia Dortmund Absences 18/19",
       subtitle = "Absences Ranked 1 - 4 Based on Player Importance",
       x = "Match", y = "Absences",
       fill = NULL) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 18, angle = 45),
        plot.title = element_text(face = "plain"),
        legend.position = "bottom",
        legend.box.spacing = unit(0.2, "cm"),
        legend.key.size = unit(0.8, "cm"))


# Distance

ggplot(season, aes(Game, Distance)) +
  geom_point(size = 2.5) +
  geom_smooth(se = FALSE, color = "#ED0000FF") +
  scale_x_continuous(breaks = seq(1, 34, 1)) +
  labs(title = "Borussia Dortmund Distance Covered 18/19",
       subtitle = "Total Distance Covered Per Game",
       x = "Match", y = "Distance (km)",
       fill = NULL) +
  theme(axis.text.x = element_text(size = 18, angle = 45),
        plot.title = element_text(face = "plain"))


# Errors

ggplot(season, aes(x=Game, y=Errors)) +
  geom_col(color = "black", alpha = 0.6) +
  geom_smooth(se = FALSE, color = "#ED0000FF") +
  scale_x_continuous(breaks = seq(1, 34, 1)) +
  labs(title = "Borussia Dortmund Errors 18/19",
       subtitle = "The Total Number of Errors Committed Per Match",
       x = "Match", y = "Errors",
       fill = NULL) +
  theme(axis.text.x = element_text(size = 18, angle = 45),
        plot.title = element_text(face = "plain"))

