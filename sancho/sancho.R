pacman::p_load(tidyverse, ggchicklet, ggtext)

##%######################################################%##
#                                                          #
####                  /90 Performance                   ####
#                                                          #
##%######################################################%##

# load data
sancho <- read_csv(here::here("sancho", "sancho.csv"))

# create labels for y axis
labels <- c(
  "Non-Penalty Goals +\nAssists (npG + A)",
  "Non-Penalty Expected\nGoals (npxG)",
  "Expected Assists (xA)",
  "Shots",
  "npxG/Shot",
  "Shot-Creating Actions",
  "Progressive Passes",
  "Total Progressive Yards\n(Passes + Carries)",
  "Passes + Carries into\nthe Penalty Area",
  "Touches",
  "Touches in Opponent's\nPenalty Area",
  "Pass Targets",
  "Turnover %",
  "Dribble Success %",
  "Players Dribbled Past"
)

# plot
sancho %>%
  mutate(
    # create plus/minus for defining segment color
    plus_minus = ifelse(change > 0, "plus", "minus"),
    # create id variable for ordering variables in plot
    id = cur_group_rows()
  ) %>%
  ggplot(aes(x = reorder(stat, desc(id)), y = change, fill = plus_minus)) +
  geom_chicklet(alpha = 0.8) + coord_flip() +
  # annotate figure to add title, subtitle and caption
  annotate("text", 15, 0.035, 
           label = "Jadon Sancho's BVB Performances",
           family = "Montserrat", color = "grey10",
           hjust = 0, size = 8, lineheight = 0.5) +
  annotate("text", 14.1, 0.035, 
           label = "Comparing the % Change in Sancho's /90 Bundesliga Stats\n2019/20 vs 2020/21 (Matchday 23)",
           family = "Montserrat", color = "grey30",
           hjust = 0, size = 5, lineheight = 1.2) +
  annotate("text", 13.3, 0.035, 
           label = "Source: FB Ref/StatsBomb | Graphic: @paul_johnson89",
           family = "Montserrat", color = "grey30",
           hjust = 0, size = 3.5, lineheight = 0.5) +
  # draw thicker line at 0 to represent starting/pivot point
  geom_hline(yintercept = 0, color = "grey20", size = 1.5) +
  # set scales for x and y
  scale_x_discrete(labels = rev(labels)) +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1),
                     breaks = seq(-0.5, 0.5, 0.1), limits = c(-0.5, 0.5)) +
  # set theme and customize to fit our requirements here
  theme_minimal(base_family = "Fira Code", base_size = 14) +
  scale_fill_manual(values = c("#FF0D57", "#1E88E5")) +
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

##%######################################################%##
#                                                          #
####                  Goals + Assists                   ####
#                                                          #
##%######################################################%##

# load data
sancho <- read_csv(here::here("sancho", "goals.csv"))

goals <- sancho %>%
  gather(key = "type", value = "value", np_goals90, assists90) %>%
  mutate(player = fct_reorder(player, value, sum, .desc=FALSE)) %>%
  select(player, type, value)

ggplot(goals, aes(x = reorder(player, value), y = value, group = type, fill = type)) +
  geom_chicklet(position = "stack", alpha = 0.8) + 
  coord_flip() +
  geom_hline(yintercept = 0, color = "grey30", size = 1.5) + 
  scale_y_continuous(labels = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4)) +
  scale_x_discrete(labels = c("Jadon Sancho" = expression(bold("Jadon Sancho")),
                              "Erling Haaland" = expression(bold("Erling Haaland")),
                              "Raphaël Guerreiro" = expression(bold("Raphaël Guerreiro")),
                              parse = TRUE)) +
  labs(
    title = glue::glue("The Most Productive Offensive Players in the Bundesliga So Far This Season"),
    subtitle = glue::glue("<span style = 'color:#1E88E5;'>Non-Penalty Goals</span> +
                          <span style = 'color:#FF0D57;'>Assists</span> Per 90 (npG+A/90) in the Bundesliga\n
                          2020/21 (Matchday 23) | Minimum 1000 Mins"),
    caption = "Data: FB Ref/StatsBomb | Graphic: @paul_johnson89") +
  theme_minimal(base_family = "Fira Code", base_size = 16) +
  scale_fill_manual(values = c("#FF0D57", "#1E88E5")) +
  theme(
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.position = "none",
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_markdown(size = 16, family = "Montserrat",
                                 color = "grey40"),
    plot.caption = element_text(size = 12, family = "Montserrat",
                                color = "grey60"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.title = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.8),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank())

ggsave(here::here("sancho", "buli_npga90.png"),
       dpi = 320, width = 16, height = 9)

##%######################################################%##
#                                                          #
####                  Chance Creation                   ####
#                                                          #
##%######################################################%##

# load data
sancho <- read_csv(here::here("sancho", "creation.csv"))


creation <- sancho %>%
  gather(key = "type", value = "value", pass_live, pass_dead,
         dribble, shot, fouled, defense) %>%
  mutate(player = fct_reorder(player, value, sum, .desc=FALSE)) %>%
  select(player, type, value)

ggplot(creation, aes(x = reorder(player, value), y = value, group = type, fill = type)) +
  geom_chicklet(position = "stack", alpha = 0.9) + 
  coord_flip() +
  geom_hline(yintercept = 0, color = "grey30", size = 1.5) +
  scale_y_continuous(labels = seq(0, 6, 1),
                     breaks = seq(0, 6, 1)) +
  scale_x_discrete(labels = c("Jadon Sancho" = expression(bold("Jadon Sancho")),
                              "Julian Brandt" = expression(bold("Julian Brandt")),
                              "Raphaël Guerreiro" = expression(bold("Raphaël Guerreiro")),
                              parse = TRUE)) +
  labs(
    title = glue::glue("The Leading Shot Creators in the Bundesliga So Far This Season"),
    subtitle = glue::glue("Shot-Creating Actions Per 90 (SCA/90), Split by Type of SCA
                          2020/21 (Matchday 23) | Minimum 1000 Mins"),
    caption = "Data: FB Ref/StatsBomb | Graphic: @paul_johnson89") +
  theme_minimal(base_family = "Fira Code", base_size = 16) +
  scale_fill_manual(
    labels = c("Defensive Actions", "Dribbles", "Fouls Won", "Dead-Balls", "Live Passes", "Shots"),
    values = c("#1D1D1F", "#4A4A4A", "#B31945",
               "#FF0D57", "#1E88E5", "#1D264F"),
    guide = guide_legend(reverse=TRUE, nrow =1)) +
  theme(
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, family = "Montserrat",
                               color = "grey20"),
    legend.spacing = unit(1, "mm"),
    legend.background = element_blank(),
    legend.margin = margin(0, 0.2, 0.2, 0.2, unit = "cm"),
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_text(size = 16, family = "Montserrat",
                                     color = "grey40"),
    plot.caption = element_text(size = 12, family = "Montserrat",
                                color = "grey60"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.title = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.8),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank())

ggsave(here::here("sancho", "sca90.png"),
       dpi = 320, width = 16, height = 9)
