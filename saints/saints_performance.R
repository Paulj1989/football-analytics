pacman::p_load(tidyverse, ggbump, ggrepel, ggtext, glue, patchwork)

df <- read_csv(here::here("epl", "results.csv"))

df <- df %>%
  arrange(season, round) %>%
  mutate(
    lubridate::ymd(date),
    xg_diff = xg - xg_against,
    goal_diff = goals - goals_against,
    plus_minus = round(goal_diff - xg_diff, digits = 2))

saints <- df %>%
  filter(team == "Southampton") %>%
  select(season, round, xg, xg_against, xg_diff, goals, goals_against, goal_diff) %>%
  arrange(season, round) %>%
  mutate(
    xg_avg = zoo::rollmean(xg, k = 7, align = "right", fill = NA),
    xga_avg = zoo::rollmean(xg_against, k = 7, align = "right", fill = NA),
    g_avg = zoo::rollmean(goals, k = 7, align = "right", fill = NA),
    ga_avg = zoo::rollmean(goals_against, k = 7, align = "right", fill = NA),
    xgdiff_avg = zoo::rollmean(xg_diff, k = 7, align = "right", fill = NA),
    gdiff_avg = zoo::rollmean(goal_diff, k = 7, align = "right", fill = NA),
    cumsum_xg = cumsum(xg),
    cumsum_xga = cumsum(xg_against),
    cumsum_xgdiff = cumsum(xg_diff),
    cumsum_goals = cumsum(goals),
    cumsum_goals_against = cumsum(goals_against),
    cumsum_gdiff = cumsum(goal_diff),
    total_rounds = row_number())

##%######################################################%##
#                                                          #
####                         xG                         ####
#                                                          #
##%######################################################%##

saints %>%
  gather(key = "for_against", value = "xg", xg_avg, xga_avg) %>%
  ggplot() +
  geom_vline(xintercept = 38, linetype = "dashed", color = "grey85") +
  geom_vline(xintercept = 76, linetype = "dashed", color = "grey85") +
  geom_vline(xintercept = 114, linetype = "dashed", color = "grey85") +
  # plot xg
  geom_bump(
    aes(
      x = total_rounds, y = xg,
      color = for_against
    ), size = 0.8, smooth = 20
  ) +
  # title and caption
  annotate("text", x = 6, y = 0.6, label = "Southampton's xG Difference in the Premier League",
           hjust = 0, family = "Montserrat", size = 11) +
  annotate("text", x = 6, y = 0.425, label = "(Rolling) Mean Average Expected Goals (xG)",
           hjust = 0, family = "Montserrat", color = "grey40", size = 7) +
  annotate("text", x = 64.3, y = 0.425, label = "For",
           hjust = 0, family = "Montserrat", fontface = "bold",
           color = "#0466c8", size = 7) +
  annotate("text", x = 69.5, y = 0.425, label = "&",
           hjust = 0, family = "Montserrat", color = "grey40", size = 7) +
  annotate("text", x = 72.15, y = 0.425, label = "Against",
           hjust = 0, family = "Montserrat", fontface = "bold",
           color = "#ef233c", size = 7) +
  annotate("text", x = 83.7, y = 0.425, label = "Since 2017",
           hjust = 0, family = "Montserrat", color = "grey40", size = 7) +
  annotate("text", x = 6, y = 0.3, label = "Source: FB Ref/StatsBomb | Graphic: @paul_johnson89",
           hjust = 0, family = "Montserrat", color = "grey50", size = 4) +
  scale_y_continuous(breaks = seq(0, 2, 1), labels = seq(0, 2, 1), limits = c(0, 2.45), NULL) +
  scale_x_continuous(position = "top", limits = c(6, 143), breaks = c(19, 57, 95, 129), labels = c("17/18" ,"18/19", "19/20", "20/21")) +
  scale_colour_manual(values = c("#0466c8", "#ef233c")) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(colour = "#343854"),
    axis.text.x = element_text(hjust = 0.1, colour = "#343854"),
    axis.text.y = element_text(hjust = 1, colour = "#343854"),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color="#F8F9FA"),
    panel.grid.minor.y = element_blank())

ggsave(here::here("epl", "saints_xg.png"), dpi = 320, width = 16, height = 9)

##%######################################################%##
#                                                          #
####                       Goals                        ####
#                                                          #
##%######################################################%##


saints %>%
  gather(key = "for_against", value = "goals", g_avg, ga_avg) %>%
  ggplot() +
  geom_vline(xintercept = 38, linetype = "dashed", color = "grey85") +
  geom_vline(xintercept = 76, linetype = "dashed", color = "grey85") +
  geom_vline(xintercept = 114, linetype = "dashed", color = "grey85") +
  # plot xg
  geom_bump(
    aes(
      x = total_rounds, y = goals,
      color = for_against
    ), size = 0.8, smooth = 20
  ) +
  # title and caption
  annotate("text", x = 6, y = 3.2, label = "Southampton's Goal Difference in the Premier League",
           hjust = 0, family = "Montserrat", size = 11) +
  annotate("text", x = 6, y = 2.98, label = "(Rolling) Mean Average Goals",
           hjust = 0, family = "Montserrat", color = "grey40", size = 7) +
  annotate("text", x = 45.3, y = 2.98, label = "For",
           hjust = 0, family = "Montserrat", fontface = "bold",
           color = "#0466c8", size = 7) +
  annotate("text", x = 50.4, y = 2.98, label = "&",
           hjust = 0, family = "Montserrat", color = "grey40", size = 7) +
  annotate("text", x = 52.65, y = 2.98, label = "Against",
           hjust = 0, family = "Montserrat", fontface = "bold",
           color = "#ef233c", size = 7) +
  annotate("text", x = 64, y = 2.98, label = "Since 2017",
           hjust = 0, family = "Montserrat", color = "grey40", size = 7) +
  annotate("text", x = 6, y = 2.85, label = "Source: FB Ref/StatsBomb | Graphic: @paul_johnson89",
           hjust = 0, family = "Montserrat", color = "grey50", size = 4) +
  scale_y_continuous(breaks = seq(0, 3, 1), labels = seq(0, 3, 1), limits = c(0, 3.3), NULL) +
  scale_x_continuous(position = "top", limits = c(6, 143), breaks = c(19, 57, 95, 129), labels = c("17/18" ,"18/19", "19/20", "20/21")) +
  scale_colour_manual(values = c("#0466c8", "#ef233c")) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(colour = "#343854"),
    axis.text.x = element_text(hjust = 0.1, colour = "#343854"),
    axis.text.y = element_text(hjust = 1, colour = "#343854"),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color="#F8F9FA"),
    panel.grid.minor.y = element_blank())

ggsave(here::here("epl", "saints_goals.png"), dpi = 320, width = 16, height = 9)

##%######################################################%##
#                                                          #
####                      20/21                         ####
#                                                          #
##%######################################################%##

saints2021 <- df %>%
  filter(team == "Southampton" & season == 2021) %>%
  select(season, round, xg, xg_against, xg_diff, goals, goals_against, goal_diff) %>%
  arrange(season, round) %>%
  mutate(
    xg_avg = zoo::rollmean(xg, k = 3, align = "right", fill = NA),
    xga_avg = zoo::rollmean(xg_against, k = 3, align = "right", fill = NA),
    g_avg = zoo::rollmean(goals, k = 3, align = "right", fill = NA),
    ga_avg = zoo::rollmean(goals_against, k = 3, align = "right", fill = NA),
    cumsum_xg = cumsum(xg),
    cumsum_xga = cumsum(xg_against),
    cumsum_goals = cumsum(goals),
    cumsum_goals_against = cumsum(goals_against),
    total_rounds = row_number())


saints2021 %>%
  gather(key = "type", value = "diffs", xg_diff, goal_diff) %>%
  ggplot() +
  # plot xg
  geom_bump(
    aes(
      x = total_rounds, y = diffs, alpha = type, color = type
    ), size = 0.8, smooth = 10, alpha = 0.8
  ) +
  # title and caption
  annotate("text", x = 0.5, y = -6, label = "Southampton's Performances This Season",
           hjust = 0, family = "Montserrat", size = 12) +
  annotate("text", x = 0.5, y = -7.4, label = "Rolling Mean Average Goal Difference\n(xGDiff Plotted for Comparison)",
           hjust = 0, family = "Montserrat", color = "grey30", size = 8) +
  annotate("text", x = 0.5, y = -8.5, label = "Source: FB Ref/StatsBomb | Graphic: @paul_johnson89",
           hjust = 0, family = "Montserrat", color = "grey50", size = 4) +
  # annotate("text", x = 3, y = 2.8, label = "xG", hjust = 0, family = "Montserrat", size = 8) +
  scale_y_continuous(breaks = seq(-10, 4, 2), labels = seq(-10, 4, 2), limits = c(-10, 4), NULL) +
  scale_colour_manual(values = c("#0466c8", "#ef233c")) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(colour = "#343854"),
    axis.text.x = element_text(hjust = 0.1, colour = "#343854"),
    axis.text.y = element_text(hjust = 1, colour = "#343854"),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color="#F8F9FA"),
    panel.grid.minor.y = element_blank())

ggsave(here::here("epl", "saints_performance2021.png"), dpi = 320, width = 16, height = 9)

##%######################################################%##
#                                                          #
####               League Performance                   ####
#                                                          #
##%######################################################%##

totals <- df %>%
  arrange(date) %>%
  select(season, round, team, xg_diff, goal_diff, plus_minus) %>%
  group_by(team) %>%
  mutate(
    total_goal_diff = round(sum(goal_diff), digits = 2),
    total_xg_diff = round(sum(xg_diff), digits = 2),
    total_plus_minus = round(sum(plus_minus), digits = 2),
    # calculate running total
    cumsum_goal_diff = cumsum(goal_diff),
    cumsum_xg_diff = cumsum(xg_diff),
    cumsum_plus_minus = cumsum(plus_minus),
    total_rounds = row_number(),
    max_rounds = max(total_rounds)
  ) %>%
  ungroup() %>%
  distinct(season, round, total_rounds, max_rounds,
           team, total_goal_diff, total_xg_diff,
           total_plus_minus, cumsum_xg_diff,
           cumsum_goal_diff, cumsum_plus_minus) %>%
  mutate(
    line_colour = case_when(
      team == "Southampton" ~ "#EF233C",
      TRUE ~ "grey60"
    )
  )

team_totals <- totals %>%
  select(team, total_goal_diff, total_xg_diff,
         total_rounds, total_plus_minus) %>%
  group_by(team) %>%
  top_n(1, total_rounds) %>%
  mutate(
    line_colour = case_when(
      team == "Southampton" ~ "#EF233C",
      TRUE ~ "grey60"
    )
  ) %>%
  filter(total_rounds > 100)

# Cumulative Goal Difference

ggplot() +
  geom_vline(xintercept = 38, linetype = "dashed", color = "grey85") +
  geom_vline(xintercept = 76, linetype = "dashed", color = "grey85") +
  geom_vline(xintercept = 114, linetype = "dashed", color = "grey85") +
  # Plot not highlighted teams
  geom_bump(
    data = subset(totals, line_colour == "grey60"),
    aes(
      x = total_rounds, y = cumsum_goal_diff, group = team,
      colour = line_colour
    ), size = 0.6, smooth = 20, alpha = 0.3
  ) +
  # plot highlighted teams
  geom_bump(
    data = subset(totals, line_colour != "grey60"),
    aes(
      x = total_rounds, y = cumsum_goal_diff, group = team,
      colour = line_colour
    ), size = 1.2, smooth = 20
  ) +
  # labels for both highlighted and not highlighted players
  geom_text_repel(
    data = team_totals,
    aes(
      x = 145, y = total_goal_diff, colour = line_colour,
      label = paste0(team, ": ", total_goal_diff)
    ),
    nudge_x = 8, hjust = 0.5, direction = "y",
    box.padding = 1, point.padding = 1,
    min.segment.length = Inf,
    force = 1, max.overlaps = Inf,
    size = 4, family = "Montserrat"
  ) +
  # title and caption
  annotate("text", x = 2, y = 225, label = "Premier League Performances Over the Last Four Seasons",
           hjust = 0, family = "Montserrat", size = 9.5) +
  annotate("text", x = 2, y = 200, label = glue::glue(
    "Cumulative Sum of Team Goal Difference in the Premier League
    2017/18 - 2020/21 | Ranked Teams All Playing 100+ Games"),
           hjust = 0, family = "Montserrat", color = "grey40", size = 6
  ) +
  annotate("text", x = 2, y = 180, label = "Source: FB Ref/StatsBomb | Graphic: @paul_johnson89",
           hjust = 0, family = "Montserrat", color = "grey40", size = 4) +
  scale_y_continuous(breaks = seq(-100, 250, 50), labels = seq(-100, 250, 50), NULL) +
  scale_x_continuous(expand = expansion(add = c(0.05, 18)), position = "top",
                     limits = c(1, 145), breaks = c(19, 57, 95, 129),
                     labels = c("17/18" ,"18/19", "19/20", "20/21")) +
  scale_size_continuous(range = c(1, 7)) +
  scale_colour_identity() +
  scale_alpha_identity() +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(colour = "#343854"),
    axis.text.x = element_text(hjust = 0.1, colour = "#343854"),
    axis.text.y = element_text(hjust = 1, colour = "#343854"),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color="#F8F9FA"),
    panel.grid.minor.y = element_blank()
  )

ggsave(here::here("epl", "epl_rankings.png"), dpi = 320, width = 16, height = 9)

# xG Overperformance

ggplot() +
  geom_vline(xintercept = 38, linetype = "dashed", color = "grey85") +
  geom_vline(xintercept = 76, linetype = "dashed", color = "grey85") +
  geom_vline(xintercept = 114, linetype = "dashed", color = "grey85") +
  # Plot not highlighted teams
  geom_bump(
    data = subset(totals, line_colour == "grey60"),
    aes(
      x = total_rounds, y = cumsum_plus_minus, group = team,
      colour = line_colour
    ), size = 0.6, smooth = 20, alpha = 0.3
  ) +
  # plot highlighted teams
  geom_bump(
    data = subset(totals, line_colour != "grey60"),
    aes(
      x = total_rounds, y = cumsum_plus_minus, group = team,
      colour = line_colour
    ), size = 1.2, smooth = 20
  ) +
  # labels for both highlighted and not highlighted players
  geom_text_repel(
    data = team_totals,
    aes(
      x = 145, y = total_plus_minus, colour = line_colour,
      label = paste0(team, ": ", total_plus_minus)
    ),
    nudge_x = 8, hjust = 0.5, direction = "y",
    box.padding = 1, point.padding = 1,
    min.segment.length = Inf,
    force = 1, max.overlaps = Inf,
    size = 4, family = "Montserrat"
  ) +
  # title and caption
  annotate("text", x = 5, y = 55, label = "Premier League Over/Underperformance Over the Last Four Seasons",
           hjust = 0, family = "Montserrat", size = 8) +
  annotate("text", x = 5, y = 49, label = glue::glue(
    "Cumulative Sum of xG +/- (xG Difference - Goal Difference) in the Premier League
    2017/18 - 2020/21 | Ranked Teams All Playing 100+ Games"),
    hjust = 0, family = "Montserrat", color = "grey40", size = 5
  ) +
  annotate("text", x = 5, y = 44, label = "Source: FB Ref/StatsBomb | Graphic: @paul_johnson89",
           hjust = 0, family = "Montserrat", color = "grey40", size = 3.5) +
  scale_y_continuous(breaks = seq(-60, 40, 20), labels = seq(-60, 40, 20), limits = c(-40, 55), NULL) +
  scale_x_continuous(expand = expansion(add = c(0.05, 18)), position = "top",
                     limits = c(1, 145), breaks = c(19, 57, 95, 129),
                     labels = c("17/18" ,"18/19", "19/20", "20/21")) +
  scale_size_continuous(range = c(1, 7)) +
  scale_colour_identity() +
  scale_alpha_identity() +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(colour = "#343854"),
    axis.text.x = element_text(hjust = 0.1, colour = "#343854"),
    axis.text.y = element_text(hjust = 1, colour = "#343854"),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color="#F8F9FA"),
    panel.grid.minor.y = element_blank()
  )

ggsave(here::here("epl", "plus_minus.png"), dpi = 320, width = 16, height = 9)

