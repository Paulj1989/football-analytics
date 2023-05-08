pacman::p_load(tidyverse, ggbump, ggrepel, here, ggtext, glue, grid)

fantasy <- readr::read_csv("~/Documents/fantasy.csv")

totals <- fantasy %>%
  group_by(team) %>%
  summarize(score = sum(score))

points_to_victory <- fantasy %>%
  select(week, team, score) %>%
  group_by(team) %>%
  mutate(
    total_pts = sum(score),
    # calculate running total
    cumsum_pts = cumsum(score),
  ) %>%
  ungroup() %>%
  distinct(team, week, total_pts, cumsum_pts) %>%
  mutate(
    line_colour = case_when(
      team == "Paul Johnson" ~ "#DB3A2F",
      team == "Owen Johnson" ~ "#275D8E",
      team == "Paul Ward" ~ "#902A57",
      TRUE ~ "grey60"
    )
  )

totals <- fantasy %>%
  group_by(team) %>%
  summarize(
    score = sum(score),
    week = 16
  ) %>%
  mutate(
    line_colour = case_when(
      team == "Paul Johnson" ~ "#DB3A2F",
      team == "Owen Johnson" ~ "#275D8E",
      team == "Paul Ward" ~ "#902A57",
      TRUE ~ "grey60"
    )
  )

ggplot() +
  # Plot not highlighted players
  geom_bump(
    data = subset(points_to_victory, line_colour == "grey60"),
    aes(
      x = week, y = cumsum_pts, group = team,
      colour = line_colour
    ), size = 1, smooth = 7
  ) +
  # plot highlighted players
  geom_bump(
    data = subset(points_to_victory, line_colour != "grey60"),
    aes(
      x = week, y = cumsum_pts, group = team,
      colour = line_colour
    ), size = 2.5, smooth = 7
  ) +
  # labels for both highlighted and not highlighted players
  geom_text_repel(
    data = totals,
    aes(
      x = week, y = score, colour = line_colour,
      label = paste0(team, ": ", score)
    ),
    nudge_x = 3, hjust = 0.5, direction = "y",
    box.padding = 1.5, point.padding = 1,
    min.segment.length = Inf,
    force = 1,
    size = 6, family = "IBM Plex Sans"
  ) +
  geom_vline(xintercept = 14, linetype = "dashed", color = "grey40") +
  # draw axis arrow
  geom_segment(aes(x = Inf, xend = Inf, y = 100, yend = 2400),
    size = 0.7,
    arrow = arrow(length = unit(0.6, "cm"))
  ) +
  scale_x_continuous(expand = expansion(add = c(0.05, 4)), position = "top", breaks = c(6.5, 16.5), labels = c("Regular Season", "Playoffs")) +
  # title and caption
  annotate("text", x = 1.05, y = 2150, label = "Path to Victory: Cumulative Fantasy Points", hjust = 0, family = "IBM Plex Sans Bold", size = 10) +
  geom_richtext(aes(
    x = 1.05,
    y = 2275,
    label = str_glue(
      "The Accumulation of Points & Final Rankings (<b style='color:#DB3A2F'>1st</b>",
      ", <b style='color:#902A57'>2nd</b>, & <b style='color:#275D8E'>3rd</b>)"
    )
  ),
  fill = NA, label.color = NA,
  label.padding = grid::unit(rep(0, 4), "pt"),
  hjust = 0, family = "IBM Plex Sans Bold", color = "grey40", size = 6
  ) +
  annotate("text", x = 1.05, y = 2350, label = "Source: The Immortal Bortles (ESPN Fantasy)", hjust = 0, family = "IBM Plex Sans", color = "grey60", size = 4) +
  scale_y_reverse(breaks = seq(500, 2400, 500), labels = seq(500, 2400, 500), position = "right", "Points Total\n") +
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
    plot.margin = margin(40, 30, 40, 40),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  ggsave(here::here("rankings.png"), dpi = 320, width = 20, height = 12)
