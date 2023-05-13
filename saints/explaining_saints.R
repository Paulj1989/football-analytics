# Setup ----

library(dplyr)
library(ggplot2)
library(glue)

# plot theme
# set minimal theme and specify font
theme_set(theme_minimal(base_family = "Montserrat", base_size = 18)) +
  # update theme to configure legend and increase plot margin
  theme_update(
    plot.background = element_rect(fill = "white", color = "white"),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_text(
      colour = "grey10", size = 10,
      family = "Fira Code"
    ),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.grid.major.y = element_line(
      color = "grey90", size = 0.4,
      linetype = "dashed"
    ),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

df <-
  read_csv(here::here("saints", "data", "results.csv")) |>
  arrange(season, round) |>
  mutate(
    date = lubridate::ymd(date),
    xg_diff = xg - xg_against,
    goal_diff = goals - goals_against,
    plus_minus = round(goal_diff - xg_diff, digits = 2)
  )

# Southampton Performance ----

## Cumulative xG + Goal Difference ----

totals <- df |>
  arrange(date) |>
  filter(season == "2021") |>
  select(season, round, team, xg_diff, goal_diff, plus_minus) |>
  group_by(team) |>
  mutate(
    total_goal_diff = round(sum(goal_diff), digits = 2),
    total_xg_diff = round(sum(xg_diff), digits = 2),
    total_plus_minus = round(sum(plus_minus), digits = 2),
    # calculate running totals
    cumsum_goal_diff = cumsum(goal_diff),
    cumsum_xg_diff = cumsum(xg_diff),
    cumsum_plus_minus = cumsum(plus_minus),
    total_rounds = row_number(),
    max_rounds = max(total_rounds)
  ) |>
  ungroup() |>
  distinct(
    season, round, total_rounds, max_rounds,
    team, total_goal_diff, total_xg_diff,
    total_plus_minus, cumsum_xg_diff,
    cumsum_goal_diff, cumsum_plus_minus
  ) |>
  mutate(
    line_colour = case_when(
      team == "Southampton" ~ "#EF233C",
      TRUE ~ "grey50"
    )
  )

team_totals <- totals |>
  select(
    team, total_goal_diff, total_xg_diff,
    total_rounds, total_plus_minus
  ) |>
  group_by(team) |>
  top_n(1, total_rounds) |>
  mutate(
    line_colour = case_when(
      team == "Southampton" ~ "#EF233C",
      TRUE ~ "grey50"
    )
  )

ggplot() +
  # Plot not highlighted teams
  ggbump::geom_bump(
    data = subset(totals, line_colour == "grey50"),
    aes(
      x = total_rounds, y = cumsum_xg_diff,
      group = team, colour = line_colour
    ),
    size = 0.5, smooth = 12, alpha = 0.3
  ) +
  ggbump::geom_bump(
    data = subset(totals, line_colour == "grey50"),
    aes(
      x = total_rounds, y = cumsum_goal_diff,
      group = team, colour = line_colour
    ),
    size = 0.5, smooth = 12, alpha = 0.3
  ) +
  # plot highlighted teams
  ggbump::geom_bump(
    data = subset(totals, line_colour != "grey50"),
    aes(
      x = total_rounds, y = cumsum_xg_diff,
      group = team, colour = line_colour
    ),
    size = 1.2, smooth = 12
  ) +
  ggbump::geom_bump(
    data = subset(totals, line_colour != "grey50"),
    aes(
      x = total_rounds, y = cumsum_goal_diff,
      group = team, colour = "#0466c8"
    ),
    size = 1.2, smooth = 12
  ) +
  # labels for highlighted teams
  ggrepel::geom_text_repel(
    data = subset(team_totals, line_colour != "grey50"),
    aes(
      x = 30, y = total_xg_diff, colour = line_colour,
      label = paste0(team, "\nxGDiff: ", total_xg_diff)
    ),
    nudge_x = 1.2, nudge_y = 2, hjust = 0.5, direction = "y",
    box.padding = 1, point.padding = 1, min.segment.length = Inf,
    max.overlaps = Inf, size = 4, family = "Montserrat"
  ) +
  ggrepel::geom_text_repel(
    data = subset(team_totals, line_colour != "grey50"),
    aes(
      x = 30, y = total_goal_diff, colour = "#0466c8",
      label = paste0(team, "\nGDiff: ", total_goal_diff)
    ),
    nudge_x = 1.2, hjust = 0.5, direction = "y", box.padding = 0.3,
    point.padding = 0.3, min.segment.length = Inf, max.overlaps = Inf,
    size = 4, family = "Montserrat"
  ) +
  # title and caption
  annotate("text",
    x = 2, y = 36,
    label = "Southampton's Performance in the Premier League, 2020/21",
    hjust = 0, family = "Montserrat", size = 8, colour = "grey10"
  ) +
  annotate("text",
    x = 2, y = 30,
    label = glue(
      "Cumulative Sum of xG Difference (xG For - xG Against) & Goal ",
      "Difference\n(Goals For - Goals Against) in the Premier League ",
      "(As Of Matchday 30)"
    ),
    hjust = 0, family = "Montserrat", color = "grey40", size = 5
  ) +
  annotate("text",
    x = 2, y = 25.5,
    label = glue(
      "Source: FB Ref/StatsBomb | ",
      "Graphic: Paul Johnson (@paul_johnson89)"
    ),
    hjust = 0, family = "Montserrat", color = "grey50", size = 3
  ) +
  annotate("text",
    x = 2.75, y = -30,
    label = glue(
      "Southampton significantly overperformed their xG for ",
      "much\nof the first half of the season, and this ",
      "began to slide, before\nplummeting after the 9-0 ",
      "loss to Manchester United.\n\nThey continue to ",
      "struggle, but during this time, their xG\nDifference ",
      "has remained pretty stable."
    ),
    hjust = 0, family = "Montserrat", color = "grey10", size = 4
  ) +
  scale_y_continuous(
    breaks = seq(-40, 40, 20),
    labels = seq(-40, 40, 20), NULL
  ) +
  scale_x_continuous(
    limits = c(1, 32), labels = c(5, 10, 15, 20, 25, 30),
    breaks = c(5, 10, 15, 20, 25, 30), NULL
  ) +
  scale_size_continuous(range = c(1, 7)) +
  scale_colour_identity() +
  scale_alpha_identity() +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1, -1, 1, 1), "cm"))

ggsave(here::here("saints", "figures", "saints_performance.png"),
  dpi = 320, width = 16, height = 9
)


## Rolling Mean xG Difference ----

df |>
  filter(team == "Southampton") |>
  select(team, season, round, xg, xg_against) |>
  mutate(
    xg_avg = zoo::rollmean(xg, k = 7, align = "right", fill = NA),
    xga_avg = zoo::rollmean(xg_against, k = 7, align = "right", fill = NA),
    total_rounds = row_number()
  ) |>
  gather(key = "for_against", value = "xg", xg_avg, xga_avg) |>
  ggplot() +
  geom_vline(
    xintercept = 38, linetype = "dashed",
    size = .5, color = "grey85"
  ) +
  geom_vline(
    xintercept = 76, linetype = "dashed",
    size = .5, color = "grey85"
  ) +
  geom_vline(
    xintercept = 114, linetype = "dashed",
    size = .5, color = "grey85"
  ) +
  # plot xg
  geom_bump(
    aes(
      x = total_rounds, y = xg,
      color = for_against
    ),
    size = 0.8, smooth = 20
  ) +
  # title and caption
  annotate("text",
    x = 6, y = 0.6,
    label = "Southampton's xG Difference in the Premier League",
    hjust = 0, size = 11, colour = "grey10"
  ) +
  annotate("text",
    x = 6, y = 0.425,
    label = "(Rolling) Mean Average Expected Goals (xG)",
    hjust = 0, color = "grey40", size = 7
  ) +
  annotate("text",
    x = 64.3, y = 0.425, label = "For",
    hjust = 0, fontface = "bold",
    color = "#0466c8", size = 7
  ) +
  annotate("text",
    x = 69.5, y = 0.425, label = "&",
    hjust = 0, color = "grey40", size = 7
  ) +
  annotate("text",
    x = 72.15, y = 0.425, label = "Against",
    hjust = 0, fontface = "bold",
    color = "#ef233c", size = 7
  ) +
  annotate("text",
    x = 83.7, y = 0.425, label = "Since 2017",
    hjust = 0, color = "grey40", size = 7
  ) +
  annotate("text",
    x = 6, y = 0.3,
    label = glue(
      "Source: FB Ref/StatsBomb | ",
      "Graphic: Paul Johnson (@paul_johnson89)"
    ),
    hjust = 0, color = "grey50", size = 4
  ) +
  scale_y_continuous(
    breaks = seq(0, 2, 1), labels = seq(0, 2, 1),
    limits = c(0, 2.45), NULL
  ) +
  scale_x_continuous(
    position = "top", limits = c(6, 143),
    breaks = c(19, 57, 95, 129),
    labels = c("17/18", "18/19", "19/20", "20/21")
  ) +
  scale_colour_manual(values = c("#0466c8", "#ef233c")) +
  coord_cartesian(clip = "off") +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 12)
  )

ggsave(here::here("saints", "figures", "saints_xg.png"),
  dpi = 320, width = 16, height = 9
)


## Rolling Mean Goal Difference ----

df |>
  filter(team == "Southampton") |>
  select(team, season, round, goals, goals_against) |>
  mutate(
    g_avg = zoo::rollmean(goals, k = 7, align = "right", fill = NA),
    ga_avg = zoo::rollmean(goals_against, k = 7, align = "right", fill = NA),
    total_rounds = row_number()
  ) |>
  gather(key = "for_against", value = "goals", g_avg, ga_avg) |>
  ggplot() +
  geom_vline(
    xintercept = 38, linetype = "dashed",
    size = .5, color = "grey85"
  ) +
  geom_vline(
    xintercept = 76, linetype = "dashed",
    size = .5, color = "grey85"
  ) +
  geom_vline(
    xintercept = 114, linetype = "dashed",
    size = .5, color = "grey85"
  ) +
  # plot goals
  geom_bump(
    aes(
      x = total_rounds, y = goals,
      color = for_against
    ),
    size = 0.8, smooth = 20
  ) +
  # title and caption
  annotate("text",
    x = 6, y = 3.2,
    label = "Southampton's Goal Difference in the Premier League",
    hjust = 0, size = 11, colour = "grey10"
  ) +
  annotate("text",
    x = 6, y = 2.98, label = "(Rolling) Mean Average Goals",
    hjust = 0, color = "grey40", size = 7
  ) +
  annotate("text",
    x = 45.3, y = 2.98, label = "For",
    hjust = 0, fontface = "bold",
    color = "#0466c8", size = 7
  ) +
  annotate("text",
    x = 50.4, y = 2.98, label = "&",
    hjust = 0, color = "grey40", size = 7
  ) +
  annotate("text",
    x = 53.25, y = 2.98, label = "Against",
    hjust = 0, fontface = "bold",
    color = "#ef233c", size = 7
  ) +
  annotate("text",
    x = 64.5, y = 2.98, label = "Since 2017",
    hjust = 0, color = "grey40", size = 7
  ) +
  annotate("text",
    x = 6, y = 2.82,
    label = glue(
      "Source: FB Ref/StatsBomb | ",
      "Graphic: Paul Johnson (@paul_johnson89)"
    ),
    hjust = 0, color = "grey50", size = 4
  ) +
  scale_y_continuous(
    breaks = seq(0, 2, 1), labels = seq(0, 2, 1),
    limits = c(0, 3.25), NULL
  ) +
  scale_x_continuous(
    position = "top", limits = c(6, 143),
    breaks = c(19, 57, 95, 129),
    labels = c("17/18", "18/19", "19/20", "20/21")
  ) +
  scale_colour_manual(values = c("#0466c8", "#ef233c")) +
  coord_cartesian(clip = "off") +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 12)
  )

ggsave(here::here("saints", "figures", "saints_goals.png"),
  dpi = 320, width = 16, height = 9
)

## Performance 2020/21 Season ----

df |>
  filter(team == "Southampton" & season == 2021) |>
  select(season, round, xg_diff, goal_diff) |>
  arrange(season, round) |>
  mutate(
    xgdiff_avg = zoo::rollmean(xg_diff, k = 7, align = "right", fill = NA),
    gdiff_avg = zoo::rollmean(goal_diff, k = 7, align = "right", fill = NA),
    total_rounds = row_number()
  ) |>
  gather(key = "type", value = "diffs", xg_diff, goal_diff) |>
  ggplot() +
  # plot xg
  geom_bump(
    aes(
      x = total_rounds, y = diffs, alpha = type, color = type
    ),
    size = 0.8, smooth = 10, alpha = 0.8
  ) +
  # title and caption
  annotate("text",
    x = 0.5, y = -5.8,
    label = "Southampton's",
    hjust = 0, size = 10.5, colour = "grey10"
  ) +
  annotate("text",
    x = 7, y = -5.8,
    label = "Goal",
    hjust = 0, size = 10.5, colour = "#0466c8"
  ) +
  annotate("text",
    x = 9.1, y = -5.8,
    label = "&",
    hjust = 0, size = 10.5, colour = "grey10"
  ) +
  annotate("text",
    x = 10, y = -5.8,
    label = "xG",
    hjust = 0, size = 10.5, colour = "#ef233c"
  ) +
  annotate("text",
    x = 11.25, y = -5.8,
    label = "Difference, 2020/21",
    hjust = 0, size = 10.5, colour = "grey10"
  ) +
  annotate("text",
    x = 0.5, y = -7.1,
    label = glue(
      "Comparing Southampton's Rolling Mean Goal Difference ",
      "and\nRolling Mean xG Difference in the Premier League",
      " This Season"
    ),
    hjust = 0, color = "grey30", size = 6
  ) +
  annotate("text",
    x = 0.5, y = -8.05,
    label = glue(
      "Source: FB Ref/StatsBomb | ",
      "Graphic: Paul Johnson (@paul_johnson89)"
    ),
    hjust = 0, color = "grey50", size = 4
  ) +
  scale_y_continuous(
    breaks = seq(-10, 4, 2), labels = seq(-10, 4, 2),
    limits = c(-10, 4), NULL
  ) +
  scale_colour_manual(values = c("#0466c8", "#ef233c")) +
  coord_cartesian(clip = "off") +
  theme(axis.text = element_text(size = 12))

ggsave(here::here("saints", "figures", "saints_2021.png"),
  dpi = 320, width = 16, height = 9
)

# League Performance ----

totals <-
  totals |>
  mutate(
    line_colour = case_when(
      team == "Southampton" ~ "#EF233C",
      team == "Liverpool" ~ "#00B2A9",
      team == "Manchester City" ~ "#6CABDD",
      team == "Tottenham" ~ "#ffa600",
      team == "Everton" ~ "#0466c8",
      TRUE ~ "grey60"
    )
  )

team_totals <-
  team_totals |>
  mutate(line_colour = case_when(
    team == "Southampton" ~ "#EF233C",
    team == "Liverpool" ~ "#00B2A9",
    team == "Manchester City" ~ "#6CABDD",
    team == "Tottenham" ~ "#ffa600",
    team == "Everton" ~ "#0466c8",
    TRUE ~ "grey60"
  )) |>
  filter(total_rounds > 100)

## Cumulative Goal Difference ----

ggplot() +
  geom_vline(
    xintercept = 38, linetype = "dashed",
    size = .5, color = "grey85"
  ) +
  geom_vline(
    xintercept = 76, linetype = "dashed",
    size = .5, color = "grey85"
  ) +
  geom_vline(
    xintercept = 114, linetype = "dashed",
    size = .5, color = "grey85"
  ) +
  # Plot not highlighted teams
  geom_bump(
    data = subset(totals, line_colour == "grey60"),
    aes(
      x = total_rounds, y = cumsum_goal_diff,
      group = team, colour = line_colour
    ),
    size = 0.6, smooth = 20, alpha = 0.4
  ) +
  # plot highlighted teams
  geom_bump(
    data = subset(totals, line_colour != "grey60"),
    aes(
      x = total_rounds, y = cumsum_goal_diff,
      group = team, colour = line_colour
    ),
    size = 1.2, smooth = 20
  ) +
  # labels for both highlighted and not highlighted teams
  geom_text_repel(
    data = team_totals,
    aes(
      x = 147, y = total_goal_diff, colour = line_colour,
      label = paste0(team, ": ", total_goal_diff)
    ),
    nudge_x = 8, hjust = 0.5, direction = "y",
    box.padding = 1, point.padding = 1,
    min.segment.length = Inf, max.overlaps = Inf,
    size = 4
  ) +
  # title and caption
  annotate("text",
    x = 7, y = 230,
    label = "Premier League Performances Over the Last Four Seasons",
    hjust = 0, size = 9.5
  ) +
  annotate("text",
    x = 7, y = 205,
    label = glue(
      "Cumulative Sum of Team Goal Difference in the Premier",
      " League\n2017/18 - 2020/21 | Ranked Teams All Playing",
      " 100+ Games"
    ),
    hjust = 0, color = "grey40", size = 6
  ) +
  annotate("text",
    x = 7, y = 185,
    label = glue(
      "Source: FB Ref/StatsBomb | ",
      "Graphic: Paul Johnson (@paul_johnson89)"
    ),
    hjust = 0, color = "grey40", size = 4
  ) +
  scale_y_continuous(
    breaks = seq(-100, 250, 50),
    labels = seq(-100, 250, 50), NULL
  ) +
  scale_x_continuous(
    expand = expansion(add = c(0.05, 18)), position = "top",
    limits = c(1, 155), breaks = c(19, 57, 95, 129),
    labels = c("17/18", "18/19", "19/20", "20/21")
  ) +
  scale_size_continuous(range = c(1, 7)) +
  scale_colour_identity() +
  scale_alpha_identity() +
  coord_cartesian(clip = "off") +
  theme(
    panel.grid.major.y = element_blank(),
    plot.margin = unit(c(1, -1, 1, 1), "cm")
  )

ggsave(here::here("saints", "figures", "epl_rankings.png"),
  dpi = 320, width = 16, height = 9
)

## xG Overperformance ----

ggplot() +
  geom_vline(
    xintercept = 38, linetype = "dashed",
    size = .5, color = "grey85"
  ) +
  geom_vline(
    xintercept = 76, linetype = "dashed",
    size = .5, color = "grey85"
  ) +
  geom_vline(
    xintercept = 114, linetype = "dashed",
    size = .5, color = "grey85"
  ) +
  # Plot not highlighted teams
  geom_bump(
    data = subset(totals, line_colour == "grey60"),
    aes(
      x = total_rounds, y = cumsum_plus_minus,
      group = team, colour = line_colour
    ),
    size = 0.6, smooth = 20, alpha = 0.4
  ) +
  # plot highlighted teams
  geom_bump(
    data = subset(totals, line_colour != "grey60"),
    aes(
      x = total_rounds, y = cumsum_plus_minus,
      group = team, colour = line_colour
    ),
    size = 1.2, smooth = 20
  ) +
  # labels for both highlighted and not highlighted teams
  geom_text_repel(
    data = team_totals,
    aes(
      x = 147, y = total_plus_minus, colour = line_colour,
      label = paste0(team, ": ", total_plus_minus)
    ),
    nudge_x = 8, hjust = 0.5, direction = "y",
    box.padding = 1, point.padding = 1,
    min.segment.length = Inf, max.overlaps = Inf,
    size = 4,
  ) +
  # title and caption
  annotate("text",
    x = 7, y = 55,
    label = glue(
      "Premier League Over/Underperformance",
      "Over the Last Four Seasons"
    ),
    hjust = 0, size = 8
  ) +
  annotate("text",
    x = 7, y = 49,
    label = glue(
      "Cumulative Sum of xG +/- (xG Difference - Goal ",
      "Difference) in the Premier League\n2017/18 - 2020/21",
      " | Ranked Teams All Playing 100+ Games"
    ),
    hjust = 0, color = "grey40", size = 5
  ) +
  annotate("text",
    x = 7, y = 44,
    label = glue(
      "Source: FB Ref/StatsBomb | ",
      "Graphic: Paul Johnson (@paul_johnson89)"
    ),
    hjust = 0, color = "grey40", size = 3.5
  ) +
  scale_y_continuous(
    breaks = seq(-60, 40, 20), labels = seq(-60, 40, 20),
    limits = c(-40, 55), NULL
  ) +
  scale_x_continuous(
    expand = expansion(add = c(0.05, 18)), position = "top",
    limits = c(1, 155), breaks = c(19, 57, 95, 129),
    labels = c("17/18", "18/19", "19/20", "20/21")
  ) +
  scale_size_continuous(range = c(1, 7)) +
  scale_colour_identity() +
  scale_alpha_identity() +
  coord_cartesian(clip = "off") +
  theme(
    panel.grid.major.y = element_blank(),
    plot.margin = unit(c(1, -1, 1, 1), "cm")
  )

ggsave(here::here("saints", "figures", "plus_minus.png"),
  dpi = 320, width = 16, height = 9
)
