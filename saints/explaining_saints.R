pacman::p_load(tidyverse, ggbump, ggrepel, ggtext, glue, patchwork)

df <- read_csv(here::here("epl", "results.csv"))

df <- df %>%
  arrange(season, round) %>%
  mutate(
    lubridate::ymd(date),
    xg_diff = xg - xg_against,
    goal_diff = goals - goals_against,
    plus_minus = round(goal_diff - xg_diff, digits = 2))

# Southampton xGDiff + GDiff

totals <- df %>%
  arrange(date) %>%
  filter(season == "2021") %>%
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
      TRUE ~ "grey80"
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
      TRUE ~ "grey80"
    )
  )


ggplot() +
  # Plot not highlighted teams
  geom_bump(
    data = subset(totals, line_colour == "grey80"),
    aes(
      x = total_rounds, y = cumsum_xg_diff, group = team,
      colour = line_colour
    ), size = 1, smooth = 12, alpha = 0.3
  ) +
  geom_bump(
    data = subset(totals, line_colour == "grey80"),
    aes(
      x = total_rounds, y = cumsum_goal_diff, group = team,
      colour = line_colour
    ), size = 1, smooth = 12, alpha = 0.3
  ) +
  # plot highlighted teams
  geom_bump(
    data = subset(totals, line_colour != "grey80"),
    aes(
      x = total_rounds, y = cumsum_xg_diff, group = team,
      colour = line_colour
    ), size = 1.4, smooth = 12
  ) +
  geom_bump(
    data = subset(totals, line_colour != "grey80"),
    aes(
      x = total_rounds, y = cumsum_goal_diff, group = team,
      colour = "#0466c8"
    ), size = 1.4, smooth = 12
  ) +
  # labels for highlighted teams
  geom_text_repel(
    data = subset(team_totals, line_colour != "grey80"),
    aes(
      x = 30, y = total_xg_diff, colour = line_colour,
      label = paste0(team, "\nxGDiff: ", total_xg_diff)
    ),
    nudge_x = 2, nudge_y = 2, hjust = 0.5, direction = "y",
    box.padding = 1, point.padding = 1,
    min.segment.length = Inf,
    force = 1, max.overlaps = Inf,
    size = 4.5, family = "Montserrat"
  ) +
  geom_text_repel(
    data = subset(team_totals, line_colour != "grey80"),
    aes(
      x = 30, y = total_goal_diff, colour = "#0466c8",
      label = paste0(team, "\nGDiff: ", total_goal_diff)
    ),
    nudge_x = 2, hjust = 0.5, direction = "y",
    box.padding = 1, point.padding = 1,
    min.segment.length = Inf,
    force = 1, max.overlaps = Inf,
    size = 4.5, family = "Montserrat"
  ) +
  # title and caption
  annotate("text", x = 2, y = 36, label = "Southampton's Performance in the Premier League",
           hjust = 0, family = "Montserrat", size = 9) +
  annotate("text", x = 2, y = 30, label = glue::glue(
    "Cumulative Sum of xG Difference & Goal Difference This Season
    2020/21 (As Of 03/23)"),
    hjust = 0, family = "Montserrat", color = "grey40", size = 5.5
  ) +
  annotate("text", x = 2, y = 25.5, label = "Source: FB Ref/StatsBomb | Graphic: @paul_johnson89",
           hjust = 0, family = "Montserrat", color = "grey40", size = 3.5) +
  annotate("text", x = 2.75, y = -30,
           label = glue::glue("Southampton significantly overperformed their xG
                              for much of the first half of the season, and this
                              began to slide, before plummeting after the 9-0 loss
                              to Manchester United.
                              
                              They continue to struggle, but during this time,
                              their xG Difference has remained pretty stable."),
           hjust = 0, family = "Montserrat", color = "grey10", size = 4) +
  scale_y_continuous(breaks = seq(-40, 40, 20), labels = seq(-40, 40, 20), NULL) +
  scale_x_continuous(expand = expansion(add = c(0.05, 3.5)),
                     limits = c(1, 30), labels = c(5, 10, 15, 20, 25, 30),
                     breaks = c(5, 10, 15, 20, 25, 30), NULL) +
  scale_size_continuous(range = c(1, 7)) +
  scale_colour_identity() +
  scale_alpha_identity() +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(colour = "#343854", size = 10),
    axis.text.y = element_text(hjust = 1, colour = "#343854",
                               size = 10),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color="#F8F9FA"),
    panel.grid.minor.y = element_blank())

ggsave(here::here("epl", "saints_performance.png"), dpi = 320, width = 16, height = 9)
