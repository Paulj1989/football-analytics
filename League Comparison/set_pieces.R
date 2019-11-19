library(foreign)
library(tidyverse)
library(ggthemr)
library(ggpubr)
library(ggsci)
library(scales)

# Data ----------

set_pieces <- read_csv("~/Documents/FTW/set_pieces.csv")
bvb_set_pieces <- read_csv("~/Documents/FTW/bvb_set_pieces.csv")

# Subset & Average ----------

set_pieces %>%
  group_by(Name) %>%
  summarise(fk_shots = sum(`FK Shots`),
            fk_goals = sum(`FK Goals`)) %>%
  mutate(avg_fk_conversion = round((fk_goals/fk_shots)*100, digits = 1)) %>%
  filter(fk_shots > 10, fk_goals > 2) %>%
  arrange(desc(avg_fk_conversion)) -> fk_avg

set_pieces %>%
  group_by(Name) %>%
  summarise(pen_shots = sum(`Pen Shots`),
            pen_goals = sum(`Pen Goals`)) %>%
  mutate(avg_pen_conversion = round((pen_goals/pen_shots)*100, digits = 1)) %>%
  filter(pen_shots > 10, pen_goals > 5) %>%
  arrange(desc(avg_pen_conversion)) -> pen_avg

mean(pen_avg$avg_pen_conversion)

  # Plots ----------
ggthemr('greyscale')

theme <-theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(1,1), legend.justification = c(1,1),
        legend.background = element_blank(),
        legend.direction="horizontal",
        legend.title = element_blank(),
        plot.title = element_text(size = 25, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 20, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 15, margin = margin(t = 10), color = "grey70", hjust = 0))

# Free Kicks

fk_avg %>%
  top_n(20, fk_goals) %>%
  ggplot(aes(x=reorder(Name, fk_goals), y=fk_goals, label = fk_goals)) +
  geom_col(color = "black", alpha = 0.9, fill = "#B9373B") +
  geom_text(position = position_dodge(0.9),
            hjust = 1.4, color = "white", size = 6) +
  labs(title = "The Highest Scoring Free Kick Takers in Football",
       subtitle = "Most Free Kicks Scored in the Big Five Leagues from 15/16 - 18/19",
       caption = "Source: Sofa Score",
       x = NULL, y = NULL,
       fill = NULL) +
  theme + coord_flip()

fk_avg %>%
  top_n(20, avg_fk_conversion) %>%
  ggplot(aes(x=reorder(Name, avg_fk_conversion), y=avg_fk_conversion, label = avg_fk_conversion)) +
  geom_col(color = "black", alpha = 0.9, fill = "#AA305D") +
  geom_text(position = position_dodge(0.9),
            hjust = 1.2, color = "white", size = 6, aes(label = paste(avg_fk_conversion, "%", sep = ""))) +
  labs(title = "The Most Efficient Free Kick Takers in Football",
       subtitle = "Highest Conversion Rates in the Big Five Leagues from 15/16 - 18/19",
       caption = "Source: Sofa Score",
       x = NULL, y = NULL,
       fill = NULL) +
  theme + coord_flip()

fk_avg %>%
  top_n(20, fk_shots) %>%
  ggplot(aes(x=reorder(Name, fk_shots), y=fk_shots, label = fk_shots)) +
  geom_col(color = "black", alpha = 0.9, fill = "#04545A") +
  geom_text(position = position_dodge(0.9),
            hjust = 1.4, color = "white", size = 6) +
  labs(title = "The Highest Number of Free Kicks Taken in Football",
       subtitle = "Most Free Kick Shots in the Big Five Leagues from 15/16 - 18/19",
       caption = "Source: Sofa Score",
       x = NULL, y = NULL,
       fill = NULL) +
  theme + coord_flip()


# Pens

pen_avg %>%
  top_n(20, pen_goals) %>%
  ggplot(aes(x=reorder(Name, pen_goals), y=pen_goals, label = pen_goals, fill = pen_shots)) +
  geom_col(color = "black", alpha = 0.9, fill = "#2D2548") +
  geom_text(position = position_dodge(0.9),
            hjust = 1.3, color = "white", size = 6) +
  labs(title = "The Highest Scoring Penalty Takers in Football",
       subtitle = "Most Penalties Scored in the Big Five Leagues from 15/16 - 18/19",
       caption = "Source: Sofa Score",
       x = NULL, y = NULL,
       fill = NULL) +
  theme + coord_flip()

pen_avg %>%
  top_n(20, avg_pen_conversion) %>%
  ggplot(aes(x=reorder(Name, avg_pen_conversion), y=avg_pen_conversion, label = avg_pen_conversion, fill = pen_goals)) +
  geom_col(color = "black", alpha = 0.9, fill = "#6B2D5D") +
  geom_text(position = position_dodge(0.9),
            hjust = 1.2, color = "white",
            size = 6, aes(label = paste(avg_pen_conversion, "%", sep = ""))) +
  labs(title = "The Most Efficient Penalty Takers in Football",
       subtitle = "Highest Conversion Rates in the Big Five Leagues from 15/16 - 18/19",
       caption = "Source: Sofa Score",
       x = NULL, y = NULL,
       fill = NULL) +
  theme + coord_flip()

pen_avg %>%
  top_n(20, pen_shots) %>%
  ggplot(aes(x=reorder(Name, pen_shots), y=pen_shots, label = pen_shots)) +
  geom_col(color = "black", alpha = 1, fill = "#53A16B") +
  geom_text(position = position_dodge(0.9),
            hjust = 1.4, color = "white", size = 6) +
  labs(title = "The Most Penalties Taken in Football",
       subtitle = "Most Penalties Taken in the Big Five Leagues from 15/16 - 18/19",
       caption = "Source: Sofa Score",
       x = NULL, y = NULL,
       fill = NULL) +
  theme + coord_flip()
