pacman::p_load(tidyverse, ggbump, ggrepel, here, ggtext, glue)

df <- readr::read_csv("buli_xg.csv")

df <- df %>%
  mutate(xg_diff = xg - xg_against,
         goal_diff = goals - goals_against,
         xg_plus_minus = round(goal_diff - xg_diff, digits = 2))

xg_totals <- df %>%
  arrange(date) %>%
  select(season, round, team, xg_plus_minus) %>%
  group_by(team) %>%
  mutate(
    total_xg_plus_minus = sum(xg_plus_minus),
    # calculate running total
    cumsum_xg_plus_minus = cumsum(xg_plus_minus),
    total_rounds = row_number()
  ) %>%
  ungroup() %>%
  distinct(season, round, total_rounds, team, total_xg_plus_minus, cumsum_xg_plus_minus) %>%
  mutate(
    line_colour = case_when(
      team == "Dortmund" ~ "#fdc500",
      team == "Bayern Munich" ~ "#ef233c",
      team == "RB Leipzig" ~ "#0466c8",
      team == "M'Gladbach" ~ "#212529",
      TRUE ~ "#DEE2E6"
    )
  )

team_totals <- df %>%
  group_by(team) %>%
  summarize(
    total_xg_plus_minus = sum(xg_plus_minus),
    rounds = 81
  ) %>%
  mutate(
    line_colour = case_when(
      team == "Dortmund" ~ "#fdc500",
      team == "Bayern Munich" ~ "#ef233c",
      team == "RB Leipzig" ~ "#0466c8",
      team == "M'Gladbach" ~ "#212529",
      TRUE ~ "#DEE2E6"
    )
  )

ggplot() +
  # Plot not highlighted players
  geom_bump(
    data = subset(xg_totals, line_colour == "#DEE2E6"),
    aes(
      x = total_rounds, y = cumsum_xg_plus_minus, group = team,
      colour = line_colour
    ), size = 0.8, smooth = 20
  ) +
  # plot highlighted players
  geom_bump(
    data = subset(xg_totals, line_colour != "#DEE2E6"),
    aes(
      x = total_rounds, y = cumsum_xg_plus_minus, group = team,
      colour = line_colour
    ), size = 2, smooth = 20
  ) +
  # labels for both highlighted and not highlighted players
  geom_text_repel(
    data = team_totals,
    aes(
      x = rounds, y = total_xg_plus_minus, colour = line_colour,
      label = paste0(team, ": ", total_xg_plus_minus)
    ),
    nudge_x = 8, hjust = 0.5, direction = "y",
    box.padding = 1.5, point.padding = 1,
    min.segment.length = Inf,
    force = 1,
    size = 3.5, family = "IBM Plex Sans"
  ) +
  geom_vline(xintercept = 34, linetype = "dashed", color = "grey80") +
  geom_vline(xintercept = 68, linetype = "dashed", color = "grey80") +
  
  # draw axis arrow
  geom_segment(aes(x = Inf, xend = Inf, y = -25, yend = 45),
               size = 0.7, arrow = arrow(length = unit(0.6, "cm"))
  ) +
  # title and caption
  annotate("text", x = 2, y = 50, label = "Team xG Over/Underperformance in the Bundesliga", hjust = 0, family = "IBM Plex Sans Bold", size = 6) +
  annotate("text", x = 2, y = 47, label = "Cumulative Sum of Team xGDiff +/- (Goal Difference - xG Difference) in the Bundesliga since 2018",
           hjust = 0, family = "IBM Plex Sans Bold", color = "grey20", size = 3.75
  ) +
  annotate("text", x = 2, y = 44.5, label = "Source: FB Ref/StatsBomb", hjust = 0, family = "IBM Plex Sans", color = "grey40", size = 2.5) +
  scale_y_continuous(breaks = seq(-30, 40, 10), labels = seq(-30, 40, 10), "xGDiff +/- \n") +
  scale_x_continuous(expand = expansion(add = c(0.05, 15)), position = "top", breaks = c(17, 51, 80), labels = c("18/19", "19/20", "20/21")) +
  scale_size_continuous(range = c(1, 7)) +
  scale_colour_identity() +
  scale_alpha_identity() +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Fira Code", base_size = 12) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(colour = "#343854"),
    axis.text.x = element_text(hjust = 0.1, colour = "#343854"),
    axis.text.y = element_text(hjust = 1, colour = "#343854"),
    plot.margin = margin(40, 30, 40, 40),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color="#F8F9FA"),
    panel.grid.minor.y = element_blank()
  ) + ggsave(here::here("rankings.png"), dpi = 320, width = 12, height = 8)

