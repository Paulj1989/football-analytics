# 1. PACKAGE MANAGEMENT ----------

pacman::p_load(tidyverse, devtools, bbplot,
               scales, ggimage, ggrepel, ggalt,
               ggtext, ggforce, glue, concaveman)


# 2. DATA----------

team_df <- read_csv("~/Documents/Sport/Writing/worst_teams.csv")

# 3. WRANGLING----------

team_df <- team_df %>%
  mutate(xgdiff = xg-xga)


# 4. PLOT----------

ggplot(team_df, aes(gameweek, xgdiff, color = team)) + geom_point(size = 10, alpha = 0.8) +
  labs(title = "Comparing the Performance of the Four Worst Teams in Europe",
       subtitle = "Team xGDiff (xG - xGA) Per Game So Far This Season",
       x = "Gameweek",
       y = NULL,
       caption = "Data: FBref | StatsBomb") +
  scale_x_continuous(labels = seq(1, 11, 1),
                     breaks = seq(1, 11, 1),
                     limits = c(1, 11)) +
  scale_y_continuous(labels = seq(-5, 1, 1),
                     breaks = seq(-5, 1, 1),
                     limits = c(-5, 1)) +
  bbc_style() +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 14, color = "#a8a8a8"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  scale_color_manual(values = c("#ffc300", "#e01e37", "#023e8a", "#007f5f"))

ggplot(team_df, aes(gameweek, opponent_spi, color = team)) + geom_point(size = 10, alpha = 0.8) +
  labs(title = "Comparing the Quality of the Opposition Faced by the Four Worst Teams in Europe",
       subtitle = "Opponent SPI (Soccer Power Index) So Far This Season",
       x = "Gameweek",
       y = NULL,
       caption = "Data: FBref | StatsBomb") +
  scale_x_continuous(labels = seq(1, 11, 1),
                     breaks = seq(1, 11, 1),
                     limits = c(1, 11)) +
  scale_y_continuous(labels = seq(50, 100, 10),
                     breaks = seq(50, 100, 10),
                     limits = c(50, 100, 10)) +
  bbc_style() +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 14, color = "#a8a8a8"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  scale_color_manual(values = c("#ffc300", "#e01e37", "#023e8a", "#007f5f"))
