library(foreign)
library(tidyverse)
library(ggthemr)
library(ggpubr)
library(ggsci)
library(scales)
library(ggridges)
library(statsExpressions)
library(ggstatsplot)
library(RcppArmadillo)
library(WRS)

## Data ----------
    
refs <- read_csv("~/Documents/refs.csv")
performance <- read_csv("~/Documents/refs2.csv")

## Wrangling ----------

refs %>%
  group_by(Team) %>% 
  summarise(position = round(mean(Position, digits = 0)),
            corrections_diff = sum(`Corrections Difference`),
            points = sum(`Points`),
            true_points = sum(`True Points`),
            points_diff = sum(`Points Difference`),
            card_points = sum(`Card Points`, na.rm = TRUE),
            true_card_points = sum(`True Card Points`, na.rm = TRUE),
            card_diff = sum(`Card Difference`, na.rm = TRUE),
            goals_for = sum(`Goals For`),
            goals_denied = sum(`Goals Denied`),
            home_points = sum(`Home Points`),
            home_true_points = sum(`Home True Points`),
            home_points_diff = sum(`Home Points Difference`),
            away_points_diff = sum(`Away Points Difference`),
            value = round(mean(`Market Value`, digits = 0)),
            capacity = round(mean(Capacity, digits = 0))) %>%
  mutate(percent = round((goals_denied/goals_for)*100, digits = 2),
         card_percent = round((card_points/true_card_points)*100, digits = 2)) %>%
  arrange(desc(true_points)) ->  true_totals

refs2 <- refs %>%
  filter(Season == "18/19" | Season == "17/18" | Season == "16/17") %>%
  mutate(percent = round((`Goals Denied`/`Goals For`)*100, digits = 2),
         card_percent = round((`Card Points`/`True Card Points`)*100, digits = 2)) %>%
  filter_all(all_vars(!is.infinite(.)))

true_totals %>%
  filter(points > 230) %>%
  mutate(percentage = round((card_points/true_card_points)*100, digits = 2)) -> cards

performance %>%
  group_by(Referee) %>% 
  summarise(Games = sum(Games),
            Incidents = sum(Incidents),
            Corrections = sum(Corrections)) %>%
  mutate(Proportion = round(Corrections/Games, digits = 2)) %>%
  filter(Games > 34) %>%
  arrange(desc(Proportion)) ->  ref_totals



## Modeling ----------

corrections_ols <- lm(`Home Points Difference` ~ `Capacity` + Position, data = refs)
summary(corrections_ols)

home_ols <- lm(`Home Points Difference` ~ `Away Points Difference`, data = refs)
summary(home_ols)

goals_ols <- lm(`Corrections Denied` ~ `Goals For`, data = refs)
summary(goals_ols)

home_away_ttest <- t.test(refs$`Home Points Difference`,
                          refs$`Away Points Difference`,
                          paired = TRUE, alternative = "two.sided")
home_away_ttest

yuenbt(refs$`Home Points Difference`,
     refs$`Away Points Difference`,nboot=10000, tr = 0.2, side = TRUE)

yuen(refs$`Home Points Difference`,
       refs$`Away Points Difference`, tr = 0.2)


cards_ols <- lm(card_percent ~ Position, data = refs2)
summary(cards_ols)

t.test(refs$`Card Points`,
       refs$`True Card Points`,
       paired = TRUE, alternative = "two.sided")

proportion_ols <- lm(Proportion ~ Referee, data = performance)
proportion_ols


Bayern <- refs %>%
  filter(Team == "Bayern Munich")

Not.Bayern <- refs %>%
  filter(Team != "Bayern Munich")

yuenbt(Not.Bayern$`Points Difference`,
       Bayern$`Points Difference`,nboot=10000, tr = 0.2, side = TRUE)

## Plots ----------

ggthemr('greyscale')

# Circle plot

theme <-theme(axis.title = element_text(size = 20),
              axis.text = element_text(size = 15),
              panel.grid.minor = element_blank(),
              legend.position = "bottom",
              legend.background = element_blank(),
              legend.direction="horizontal",
              legend.text = element_text(size = 15),
              legend.key.size = unit(1.5, "cm"),
              plot.title = element_text(size = 25, margin = margin(b = 10)),
              plot.subtitle = element_text(size = 20, color = "darkslategrey", margin = margin(b = 25)),
              plot.caption = element_text(size = 15, margin = margin(t = 10), color = "grey70", hjust = 0))

ggplot(true_totals, aes(reorder(Team, corrections_diff),
                        corrections_diff,
                        label=corrections_diff)) + 
  geom_point(aes(color = corrections_diff > 0), stat='identity', size=13.5)  +
  geom_text(color="white", size=6.5) +
  scale_color_manual(name=NULL,
                     labels = c("Negative", "Positive"),
                     values = c("#C75D56", "#247593")) +
  labs(title="Which Team Lost the Most Goals as a Result of Referee Bias?", 
       subtitle="Net Corrections Per Bundesliga Team 07/08 - 18/19",
       caption = "Source: Wahre Tabelle",
       y = NULL, x = NULL) +
  coord_flip() + expand_limits(y = c(-22, 39)) + scale_y_discrete(limits = c(-20, -10, 0, 10, 20, 30)) + theme

# Ridge plot

theme <-theme(axis.title = element_text(size = 20),
              axis.text = element_text(size = 15), 
              panel.grid.minor.y = element_blank(),
              panel.grid.major.y = element_blank(),
              legend.position = "bottom",
              legend.background = element_blank(),
              legend.direction="horizontal",
              plot.title = element_text(size = 25, margin = margin(b = 10)),
              plot.subtitle = element_text(size = 20, color = "darkslategrey", margin = margin(b = 25)),
              plot.caption = element_text(size = 15, margin = margin(t = 10), color = "grey70", hjust = 0))

refs %>% 
  group_by(Team) %>%
  filter(sum(Points) > 300) %>%
  ungroup(all) %>%
  ggplot(aes(y=reorder(Team, Points), x=`Corrections Difference`)) +
  geom_density_ridges(alpha=0.9, size = 0.7, fill = "#C75D56", scale = 1.6) +
  labs(title="The Distribution of Net Corrections Per Team", 
       subtitle="Density Plots of Net Corrected Goals Per Bundesliga Team 07/08 - 18/19",
       caption = "Source: Wahre Tabelle",
       y = NULL) +
  theme

performance %>% 
  group_by(Referee) %>%
  filter(sum(Games) > 100) %>%
  ungroup(all) %>%
  ggplot(aes(y=reorder(Referee, Games), x= Proportion)) +
  geom_density_ridges(alpha=0.9, size = 0.7, fill = "#247593", scale = 1.6) +
  labs(title="The Proportion of Errors Per Referee", 
       subtitle="Density Plots of Each Referee's Proportion of Errors Per Season 07/08 - 18/19",
       caption = "Source: Wahre Tabelle",
       y = NULL) +
  theme

# Violin/Box plot

home_away_refs <- select(refs, Team, Season, `Home Points Difference`,
                         `Away Points Difference`) 
home_away_refs <- rename(home_away_refs,
                         Home = `Home Points Difference`,
                         Away = `Away Points Difference`)
home_away_refs <- gather(home_away_refs, `Home`, `Away`,
                key = "Home/Away", value = "Difference")

theme <-theme(axis.title = element_text(size = 20),
              axis.text = element_text(size = 15), 
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.position = "bottom",
              legend.background = element_blank(),
              legend.direction="horizontal",
              plot.title = element_text(size = 25, margin = margin(b = 10)),
              plot.subtitle = element_text(size = 20, color = "darkslategrey", margin = margin(b = 25)),
              plot.caption = element_text(size = 15, margin = margin(t = 10), color = "grey70", hjust = 0))

ggplot2::text


ggstatsplot::ggwithinstats(
  data = home_away_refs,
  x = `Home/Away`,
  y = Difference,
  sort = "descending",
  sort.fun = median,
    sphericity.correction = TRUE,
  pairwise.comparisons = TRUE,
  pairwise.display = "everything",
  pairwise.annotation = "p.value",
  type = "robust",
  title = "Home and Away Points Differences",
  caption = "Source: Wahr Tabelle",
  mean.label.size = 7,
  ggstatsplot.layer = FALSE,
  package = "ggsci",
  palette = "default_nejm",
  outlier.tagging = TRUE,
  messages = TRUE,
  ggtheme = theme) + ggplot2::labs(x = NULL)
