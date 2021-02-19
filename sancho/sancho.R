pacman::p_load(tidyverse)

# load data
sancho <- read_csv(here::here("sancho", "sancho.csv"))

# create labels for y axis
labels <- c(
  "Non-Penalty Goals +\nAssists (npG + A)",
  "Non-Penalty Expected\nGoals (npxG)",
  "Expected Assists (xA)",
  "npxG/Shot",
  "Shots",
  "Shot-Creating Actions",
  "Passes into\nthe Penalty Area",
  "Progressive Passes",
  "Total Progressive Yards\n(Passes + Carries)",
  "Dribble Success %",
  "Players Dribbled Past",
  "Touches",
  "Touches in Opponent's\nPenalty Area",
  "Pass Targets"
)

# plot
sancho %>%
  mutate(
    # create plus/minus for defining segment color
    plus_minus = ifelse(change > 0, "plus", "minus"),
    # create id variable for ordering variables in plot
    id = cur_group_rows()
  ) %>%
  ggplot(aes(x = reorder(stat, desc(id)), y = change)) +
  # create segments starting at 0 and ending at change value
  # with colors defined by plus_minus variable
  geom_segment(aes(
    x = 0, xend = change,
    y = reorder(stat, desc(id)),
    yend = reorder(stat, desc(id)),
    color = plus_minus
  ), size = 16.5) +
  # draw thicker line at 0 to represent starting/pivot point
  geom_vline(xintercept = 0, color = "grey20", size = 1.2) +
  # annotate figure to add title, subtitle and caption
  annotate("text", 0.035, 14,
           label = "Jadon Sancho's Offensive Struggles This Season",
           family = "Montserrat", color = "grey10",
           hjust = 0, size = 7, lineheight = 0.5) +
  annotate("text", 0.035, 13.1,
           label = "Comparing the % Change in Jadon Sancho's\n/90 Offensive Statistics from 19/20 to 20/21",
           family = "Montserrat", color = "grey30",
           hjust = 0, size = 5, lineheight = 1.2) +
  annotate("text", 0.035, 12.3,
           label = "Source: FB Ref/StatsBomb (01/21) | Graphic: @paul_johnson89",
           family = "Montserrat", color = "grey30",
           hjust = 0, size = 3.5, lineheight = 0.5) +
  # set scales for x and y
  scale_y_discrete(labels = rev(labels)) +
  scale_x_continuous(labels = function(x) scales::percent(x, accuracy = 1), breaks = seq(-0.5, 0.7, 0.1), limits = c(-0.55, 0.7)) +
  # set theme and customize to fit our requirements here
  theme_minimal(base_family = "Fira Code", base_size = 14) +
  scale_color_manual(values = c(plus = "#007F5F", minus ="#DE2141")) +
  theme(
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "#F3F5F6", size = 0.4),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

# save image
ggsave(here::here("sancho", "sancho.png"), dpi = 320, width = 16, height = 9)
