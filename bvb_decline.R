library(foreign)
library(tidyverse)
library(ggthemr)
library(PairedData)

# Data ----------

season <- read_csv("season.csv")

hinrunde <- season %>%
  filter(Game < 18)

ruckrunde <- season %>%
  filter(Game > 17)

# Distributions ----------

ggplot(season, aes(xDiff)) +
  geom_density()

ggplot(season, aes(Points)) +
  geom_density()


# Regression ----------

olsAbsence <- lm(xDiff ~ Absence2 + Difficulty + Home, data = season)
summary(olsAbsence)

olsWitsel <- lm(xDiff ~ Witsel + Difficulty, data = season)
summary(olsWitsel)

# Correlations ----------

cor(hinrunde$xDiff, ruckrunde$xDiff, method = "spearman")

sum(hinrunde$xDiff)
sum(ruckrunde$xDiff)

cor(season$Witsel, season$Game, na.rm = TRUE)

cov(season[5:12], method = "pearson")

# t-tests ----------

xDiff_paired <- t.test(hinrunde$xDiff, ruckrunde$xDiff, paired = TRUE, alternative = "two.sided")

absence_paired <- t.test(hinrunde$Absence2, ruckrunde$Absence2, paired = TRUE, alternative = "two.sided")

xG_paired <- t.test(hinrunde$xG, ruckrunde$xG, paired = TRUE, alternative = "two.sided")

xGA_paired <- t.test(hinrunde$xGA, ruckrunde$xGA, paired = TRUE, alternative = "two.sided")

witsel_paired <- t.test(hinrunde$Witsel, ruckrunde$Witsel, paired = TRUE, alternative = "two.sided")
witsel_paired

absenceDF <- paired(hinrunde$Absence2, ruckrunde$Absence2)
sumHinrunde <- sum(absenceDF$`hinrunde$Absence2`)
sumRuckrunde <- sum(absenceDF$`ruckrunde$Absence2`)
dfSum <- tibble(sumHinrunde, sumRuckrunde)

# Plots ----------  

ggthemr('greyscale', layout = "scientific", text_size = 25)

ggplot(season, aes(Points, Possession)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(dfSum, aes(sumHinrunde)) +
  geom_bar() +
  geom_bar(aes(sumRuckrunde))

  geom_bar(ruckrunde, aes(Absence2))
