pacman::p_load(tidyverse)

# load data
sancho <-
  readr::read_csv("/home/paulj1989/Repositories/football-analytics/ftw/sancho/sancho.csv")

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
    change = change * 100,
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
  ), size = 15) +
  # draw thicker line at 0 to represent starting/pivot point
  geom_vline(xintercept = 0, color = "grey20", size = 1.2) +
  # annotate figure to add title, subtitle and caption
  annotate("text", 3.5, 14,
           label = "Jadon Sancho's Offensive Struggles This Season",
           family = "IBM Plex Sans", color = "black",
           hjust = 0, size = 5.5, lineheight = 0.5) +
  annotate("text", 3.5, 13.2,
           label = "Comparing the % Change in Jadon Sancho's\n/90 Offensive Statistics from 19/20 to 20/21",
           family = "IBM Plex Sans", color = "black",
           hjust = 0, size = 4, lineheight = 1.2) +
  annotate("text", 3.5, 12.5,
           label = "Source: FB Ref/StatsBomb | Graphic: @paul_johnson89",
           family = "IBM Plex Sans", color = "black",
           hjust = 0, size = 2.5, lineheight = 0.5) +
  # set scales for x and y
  scale_y_discrete(labels = rev(labels)) +
  scale_x_continuous(expand = expansion(add = c(5, 10)),
                     breaks = seq(-70, 70, 10),
                     labels = seq(-70, 70, 10),
                     limits = c(-70, 70)) +
  # set theme and customize to fit our requirements here
  theme_minimal(base_family = "Fira Code", base_size = 14) +
  scale_color_manual(values = c(plus = "#007F5F", minus ="#DE2141")) +
  labs(x = "% Change") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    axis.title.x = element_text(size = 10, color = "grey30"),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "#F3F5F6", size = 0.4),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

# save image
ggsave(here::here("/home/paulj1989/Repositories/football-analytics/ftw/sancho/sancho.png"), dpi = 320, width = 16, height = 9)
