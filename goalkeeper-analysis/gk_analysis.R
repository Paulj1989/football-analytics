## %######################################################%##
#                                                          #
####                Goalkeeper Analysis                 ####
#                                                          #
## %######################################################%##

# packages
pacman::p_load(tidyverse, ggrepel)

# data
df <- read_csv(here::here("sports", "goalkeeper-analysis", "big_five.csv"))

# wrangling
big5 <- df %>%
  group_by(full_name, surname, league) %>%
  na.omit() %>%
  summarize(
    nineties = sum(`90s`),
    mins = sum(mins),
    goals_against = sum(goals_against),
    own_goals = sum(own_goals),
    ga90 = goals_against / nineties,
    sota = sum(sota),
    sota90 = sota / nineties,
    psxg = sum(psxg),
    psxg_sot = psxg/sota,
    psxg_plus_minus = sum(psxg_plus_minus),
    psxg90 = psxg / nineties,
    psxg_percent = (psxg / (goals_against-own_goals)) - 1,
    crosses_faced = sum(crosses_faced),
    crosses_stopped = sum(crosses_stopped),
    cross_stopped_percent = crosses_stopped / crosses_faced,
    opa = sum(opa),
    opa_avg_dist = mean(opa_avg_dist),
    opa90 = opa / nineties,
    pass_att90 = sum(pass_att) / nineties,
    launched = sum(launched),
    launch_att = sum(launch_att),
    launch_att90 = launch_att / nineties,
    launch_comp_percent = launched / launch_att,
    progressive_passes = sum(progressive_passes),
    progressive_distance = sum(progressive_distance),
    .groups = "keep"
  ) %>%
  filter(mins > 6000) %>%
  select(
    full_name, surname, league, mins, nineties, sota, sota90, goals_against, own_goals,
    ga90, psxg, psxg_sot, psxg_plus_minus, psxg90, psxg_percent, pass_att90, launch_att90,
    launch_comp_percent, crosses_faced, crosses_stopped, cross_stopped_percent, opa,
    opa_avg_dist, opa90, progressive_passes, progressive_distance
  )

## %######################################################%##
#                                                          #
####          Analyzing Goalkeeper Performance          ####
####                from 17/18 to 20/21                 ####
#                                                          #
## %######################################################%##

# psxg %
big5 %>%
  mutate(comp_color = ifelse(psxg_percent > 0, "type1", "type2")) %>%
  ggplot(aes(y = reorder(full_name, psxg_percent), x = psxg_percent, label = scales::percent(psxg_percent))) +
  geom_segment(aes(
    x = 0, xend = psxg_percent,
    y = reorder(full_name, psxg_percent),
    yend = reorder(full_name, psxg_percent),
    color = comp_color
  ), size = 5, alpha = 0.8) +
  geom_vline(xintercept = 0, color = "grey20", size = 1) +
  scale_x_continuous(labels = function(x) scales::percent(x, accuracy = 1), breaks = seq(-0.1, 0.4, 0.1), limits = c(-0.135, 0.4)) +
  scale_y_discrete(labels = c("Roman Bürki" = expression(bold("Roman Bürki")), parse = TRUE)) +
  labs(
    title = glue::glue("Goalkeeper Shot-Stopping Over/Underperformance in the Big Five Leagues"),
    subtitle = glue::glue("Post-Shot Expected Goals as % of Goals (PSxG %)\n2017/18 - 2020/21 | Minimum 6000 Mins"),
    caption = "Data: FB Ref | StatsBomb"
  ) +
  theme_minimal(base_family = "Fira Code", base_size = 14) +
  scale_color_manual(values = c("#457b9d", "#e63946")) +
  theme(
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.position = "none",
    plot.title = element_text(size = 20, family = "Montserrat"),
    plot.subtitle = element_text(size = 16, family = "Montserrat", color = "grey40"),
    plot.caption = element_text(size = 12, color = "grey60"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.title = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(here::here("sports", "goalkeeper-analysis", "figures", "psxg.png"), dpi = 320, width = 14, height = 16)

# league psxg %
df %>%
  group_by(full_name, surname, league) %>%
  na.omit() %>%
  summarize(
    mins = sum(mins),
    goals_against = sum(goals_against),
    psxg = sum(psxg),
    psxg_percent = (psxg / goals_against) - 1,
    .groups = "keep"
  ) %>%
  filter(mins > 3000 & league == "Premier League") %>%
  select(full_name, psxg_percent) %>%
  mutate(comp_color = ifelse(psxg_percent > 0, "type1", "type2")) %>%
  ggplot(aes(y = reorder(full_name, psxg_percent), x = psxg_percent, label = scales::percent(psxg_percent))) +
  geom_segment(aes(
    x = 0, xend = psxg_percent,
    y = reorder(full_name, psxg_percent),
    yend = reorder(full_name, psxg_percent),
    color = comp_color
  ), size = 6, alpha = 0.8) +
  geom_vline(xintercept = 0, color = "grey20", size = 1) +
  scale_x_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(
    title = glue::glue("Goalkeeper Shot-Stopping Over/Underperformance in the Premier League"),
    subtitle = glue::glue("Post-Shot Expected Goals as % of Goals (PSxG %)
                          2017/18 - 2020/21 | Minimum 3000 Mins"),
    caption = "Data: FB Ref | StatsBomb") +
  theme_minimal(base_family = "Fira Code", base_size = 14) +
  scale_color_manual(values = c("#457b9d", "#e63946")) +
  theme(
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.position = "none",
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_text(size = 16, family = "Montserrat", color = "grey40"),
    plot.caption = element_text(size = 12, color = "grey60"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.title = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.8),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank())

# shot-stopping & workload
ggplot(big5, aes(x = psxg_percent, y = sota90, color = league,
                 label = scales::percent(psxg_percent))) +
  geom_vline(xintercept = mean(big5$psxg_percent),
             linetype = "dashed", color = "grey80") +
  geom_hline(yintercept = mean(big5$sota90),
             linetype = "dashed", color = "grey80") +
  geom_point(
    aes(),
    shape = 21, size = 2,
    alpha = 0.8, stroke = 1.5) +
  geom_text_repel(
    aes(label = surname),
    min.segment.length = 0.3, seed = 15, size = 3.5,
    point.padding = 0.2, segment.alpha = 0, color = "grey20") +
  labs(
    title = glue::glue("Goalkeeper Shot-Stopping Performance & Workload in the Big Five Leagues"),
    subtitle = glue::glue("Post-Shot Expected Goals as % of Goals (PSxG %) & Shots on Target Against Per 90 (SoTA/90)
                          2017/18 - 2020/21 | Minimum 6000 Mins"),
    x = "PSxG %",
    y = "SoTA/90",
    caption = "Data: FBref | StatsBomb") +
  annotate("text",
           x = -0.13, y = 5.5, hjust = 0, size = 4,
           label = "Busy & Underperforming",
           family = "Fira Code") +
  annotate("text",
           x = -0.13, y = 2.1, hjust = 0, size = 4,
           label = "Not Busy & Underperforming",
           family = "Fira Code") +
  annotate("text",
           x = 0.12, y = 2.1, hjust = 0, size = 4,
           label = "Not Busy & Overperforming",
           family = "Fira Code") +
  annotate("text",
           x = 0.12, y = 5.5, hjust = 0, size = 4,
           label = "Busy & Overperforming",
           family = "Fira Code") +
  scale_color_manual(values = c("#457b9d", "#e63946", "#B34B7D", "#4D8C60", "#F7BC4D")) +
  scale_x_continuous(labels = function(x) scales::percent(x, accuracy = 1),
                     breaks = seq(-0.1, 0.4, 0.1), limits = c(-0.135, 0.4)) +
  scale_y_continuous(
    labels = seq(2, 5, 1),
    breaks = seq(2, 5, 1),
    limits = c(2, 5.5)) +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  theme(
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.title = element_blank(),
    legend.position = c(0.95, 0.75),
    legend.direction = "vertical",
    legend.spacing = unit(1, "mm"),
    legend.background = element_blank(),
    legend.justification = "right",
    legend.margin = margin(0, 0.2, 0.2, 0.2, unit = "cm"),
    legend.box.background = element_rect(colour = "black"),
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_text(size = 16, family = "Montserrat", color = "grey30"),
    plot.caption = element_text(size = 14, color = "#a8a8a8"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    panel.grid.major = element_line(color = "#F3F5F6", size = 0.4),
    panel.grid.minor = element_blank())

ggsave(here::here("sports", "goalkeeper-analysis", "figures", "gk_workload.png"), dpi = 320, width = 16, height = 9)

## %######################################################%##
#                                                          #
#### Considering a Goalkeeper's Other Responsibilities  ####
#                                                          #
## %######################################################%##

# sweepers
ggplot(big5, aes(x = opa90, y = opa_avg_dist, color = league)) +
  geom_vline(xintercept = mean(big5$opa90), linetype = "dashed", color = "grey80") +
  geom_hline(yintercept = mean(big5$opa_avg_dist), linetype = "dashed", color = "grey80") +
  geom_point(
    aes(),
    shape = 21, size = 2,
    alpha = 0.8, stroke = 1.5) +
  geom_text_repel(
    aes(label = surname),
    min.segment.length = 0.3, seed = 15, size = 3.5,
    point.padding = 0.3, segment.alpha = 0, color = "grey20") +
  labs(
    title = glue::glue("Sweeper Keeper's Defensive Actions in the Big Five Leagues"),
    subtitle = glue::glue("Defensive Actions Outside the Penalty Area Per 90 (#OPA/90) & Average Distance from Box (Average Distance OPA)
                          2017/18 - 2020/21 | Minimum 6000 Mins"),
    x = "#OPA/90",
    y = "Average Distance OPA",
    caption = "Data: FBref | StatsBomb") +
  scale_x_continuous(
    labels = seq(0.5, 1.5, 0.5),
    breaks = seq(0.5, 1.5, 0.5),
    limits = c(0.23, 1.5)) +
  scale_y_continuous(
    labels = seq(12, 18, 1),
    breaks = seq(12, 18, 1),
    limits = c(12, 18)) +
  scale_color_manual(values = c("#457b9d", "#e63946", "#B34B7D", "#4D8C60", "#F7BC4D")) +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  theme(
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.title = element_blank(),
    legend.position = c(0.95, 0.25),
    legend.direction = "vertical",
    legend.spacing = unit(1, "mm"),
    legend.background = element_blank(),
    legend.justification = "right",
    legend.margin = margin(0, 0.2, 0.2, 0.2, unit = "cm"),
    legend.box.background = element_rect(colour = "black"),
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_text(size = 16, family = "Montserrat", color = "grey30"),
    plot.caption = element_text(size = 14, color = "#a8a8a8"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    panel.grid.major = element_line(color = "#F3F5F6", size = 0.4),
    panel.grid.minor = element_blank())

ggsave(here::here("sports", "goalkeeper-analysis", "figures", "sweepers.png"), dpi = 320, width = 16, height = 9)

# long ball completion %
big5 %>%
  mutate(comp_color = ifelse(surname == "Bürki", "type1", "type2")) %>%
  ggplot(aes(x = reorder(surname, launch_comp_percent), y = launch_comp_percent), label = scales::percent(launch_comp_percent)) +
  geom_segment(aes(
    x = 0, xend = launch_comp_percent,
    y = reorder(full_name, launch_comp_percent),
    yend = reorder(full_name, launch_comp_percent),
    color = comp_color), size = 5, alpha = 0.9) +
  geom_vline(xintercept = 0, color = "grey20", size = 1) +
  scale_x_continuous(labels = function(x) scales::percent(x, accuracy = 1), breaks = seq(0, 0.6, 0.1), limits = c(0, 0.61)) +
  scale_y_discrete(labels=c("Roman Bürki"= expression(bold("Roman Bürki")), parse=TRUE)) +
  labs(
    title = glue::glue("Goalkeeper Long Ball Distribution in the Big Five Leagues"),
    subtitle = glue::glue("% of Goalkeeper Passes Longer than 40 Yards Completed
                          2017/18 - 2020/21 | Minimum 6000 Mins")) +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  scale_color_manual(values = c("#e63946", "#457b9d")) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_text(size = 16, family = "Montserrat", color = "grey30"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.title = element_blank(),
    panel.grid.major.x = element_line(color = "#CED6DE", size = 0.8),
    panel.grid.minor.x = element_line(color = "#F3F5F7", size = 0.5),
    panel.grid.major.y = element_blank())

ggsave(here::here("sports", "goalkeeper-analysis", "figures", "pass_completion.png"), dpi = 320, width = 12, height = 16)


# passing & distribution
ggplot(big5, aes(x = launch_att90, y = pass_att90, color = league)) +
  geom_vline(xintercept = mean(big5$launch_att90), linetype = "dashed", color = "grey80") +
  geom_hline(yintercept = mean(big5$pass_att90), linetype = "dashed", color = "grey80") +
  geom_point(
    aes(),
    shape = 21, size = 2,
    alpha = 0.8, stroke = 1.5
  ) +
  geom_text_repel(
    aes(label = surname),
    min.segment.length = 0.3, seed = 15, size = 3.5,
    point.padding = 0.3, segment.alpha = 0, color = "grey20"
  ) +
  labs(
    title = glue::glue("Goalkeeper Passing & Distribution in the Big Five Leagues"),
    subtitle = glue::glue("Passes Longer than 40 Feet Attempted Per 90 (Long Passes/90) & Passes Attempted Per 90 (Passes/90)
                          2017/18 - 2020/21 | Minimum 4500 Minutes Played"),
    x = "Long Passes/90",
    y = "Passes/90",
    caption = "Data: FBref | StatsBomb") +
  scale_color_manual(values = c("#457b9d", "#e63946", "#B34B7D", "#4D8C60", "#F7BC4D")) +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  theme(
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.title = element_blank(),
    legend.position = c(0.95, 0.15),
    legend.direction = "vertical",
    legend.spacing = unit(1, "mm"),
    legend.background = element_blank(),
    legend.justification = "right",
    legend.margin = margin(0, 0.2, 0.2, 0.2, unit = "cm"),
    legend.box.background = element_rect(colour = "black"),
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_text(size = 16, family = "Montserrat", color = "grey30"),
    plot.caption = element_text(size = 14, color = "#a8a8a8"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    panel.grid.major = element_line(color = "#F3F5F6", size = 0.4),
    panel.grid.minor = element_blank())

ggsave(here::here("sports", "goalkeeper-analysis", "figures", "distribution.png"), dpi = 320, width = 16, height = 9)

# crosses stopped
big5 %>%
  mutate(comp_color = ifelse(surname == "Bürki", "type1", "type2")) %>%
  ggplot(aes(x = reorder(surname, cross_stopped_percent), y = cross_stopped_percent), label = scales::percent(cross_stopped_percent)) +
  geom_segment(aes(
    x = 0, xend = cross_stopped_percent,
    y = reorder(full_name, cross_stopped_percent),
    yend = reorder(full_name, cross_stopped_percent),
    color = comp_color
  ), size = 5, alpha = 0.9) +
  geom_vline(xintercept = 0, color = "grey20", size = 1) +
  scale_x_continuous(labels = function(x) scales::percent(x, accuracy = 1), breaks = seq(0, 0.15, 0.05), limits = c(0, 0.15)) +
  scale_y_discrete(labels=c("Roman Bürki"= expression(bold("Roman Bürki")), parse=TRUE)) +
  labs(
    title = glue::glue("Goalkeeper Success Stopping Crosses in the Big Five Leagues"),
    subtitle = glue::glue("% of Crosses into the Penalty Area Successfully Stopped by Goalkeeper
                          2017/18 - 2020/21 | Minimum 6000 Mins")) +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  scale_color_manual(values = c("#e63946", "#457b9d")) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_text(size = 16, family = "Montserrat", color = "grey30"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.title = element_blank(),
    panel.grid.major.x = element_line(color = "#CED6DE", size = 0.8),
    panel.grid.minor.x = element_line(color = "#F3F5F7", size = 0.5),
    panel.grid.major.y = element_blank())

ggsave(here::here("sports", "goalkeeper-analysis", "figures", "crosses.png"), dpi = 320, width = 12, height = 16)

## %######################################################%##
#                                                          #
####              Performance This Season               ####
#                                                          #
## %######################################################%##

# wrangling
season <- df %>%
  filter(season == 2021, mins > 1400) %>%
  na.omit() %>%
  mutate(
    ga90 = goals_against / `90s`,
    sota90 = sota / `90s`,
    psxg90 = psxg / `90s`,
    psxg_percent = (psxg / (goals_against-own_goals)) - 1,
    cross_stopped_percent = crosses_stopped / crosses_faced,
    opa90 = opa / `90s`,
    pass_att90 = sum(pass_att) / `90s`,
    launch_att90 = launch_att / `90s`,
    launch_comp_percent = launched / launch_att
  ) %>%
  select(
    full_name, surname, league, mins, sota, sota90, goals_against, ga90,
    psxg, psxg_sot, psxg_plus_minus, psxg90, psxg_percent, pass_att90, launch_att90,
    launch_comp_percent, crosses_faced, crosses_stopped, cross_stopped_percent, opa,
    opa_avg_dist, opa90
  )

# psxg %
season %>%
  mutate(comp_color = ifelse(psxg_percent > 0, "type1", "type2")) %>%
  ggplot(aes(y = reorder(full_name, psxg_percent), x = psxg_percent, label = scales::percent(psxg_percent))) +
  geom_segment(aes(
    x = 0, xend = psxg_percent,
    y = reorder(full_name, psxg_percent),
    yend = reorder(full_name, psxg_percent),
    color = comp_color
  ), size = 5, alpha = 0.8) +
  geom_vline(xintercept = 0, color = "grey20", size = 1) +
  scale_x_continuous(labels = function(x) scales::percent(x, accuracy = 1), breaks = seq(-0.3, 1, 0.1), limits = c(-0.3, 1)) +
  scale_y_discrete(labels = c("Roman Bürki" = expression(bold("Roman Bürki")), parse = TRUE)) +
  labs(
    title = glue::glue("Goalkeeper Shot-Stopping Over/Underperformance in the Big Five Leagues"),
    subtitle = glue::glue("Post-Shot Expected Goals as % of Goals (PSxG %)\n2020/21 | Minimum 1350 Mins"),
    caption = "Data: FB Ref | StatsBomb"
  ) +
  theme_minimal(base_family = "Fira Code", base_size = 14) +
  scale_color_manual(values = c("#457b9d", "#e63946")) +
  theme(
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.position = "none",
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_text(size = 16, family = "Montserrat", color = "grey40"),
    plot.caption = element_text(size = 12, color = "grey60"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 14),
    axis.title = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(here::here("sports", "goalkeeper-analysis", "figures", "psxg2021.png"), dpi = 320, width = 14, height = 16)

# shot-stopping & workload
ggplot(
  season, aes(x = psxg_percent, y = sota90, color = league, label = scales::percent(psxg_percent))
) +
  geom_vline(xintercept = mean(season$psxg_percent), linetype = "dashed", color = "grey80") +
  geom_hline(yintercept = mean(season$sota90), linetype = "dashed", color = "grey80") +
  geom_point(
    aes(),
    shape = 21, size = 2,
    alpha = 0.8, stroke = 1.5
  ) +
  geom_text_repel(
    aes(label = surname),
    min.segment.length = 0.3, seed = 15, size = 4,
    point.padding = 0.2, segment.alpha = 0, color = "grey20"
  ) +
  labs(
    title = glue::glue("Goalkeeper Shot-Stopping Performance & Workload in the Big Five Leagues"),
    subtitle = glue::glue("Post-Shot Expected Goals as % of Goals (PSxG %) & Shots on Target Against Per 90 (SoTA/90)
                          2020/21 | Minimum 1350 Mins"),
    x = "PSxG %",
    y = "SoTA/90",
    caption = "Data: FBref | StatsBomb") +
  annotate("text",
           x = -0.28, y = 6.2, hjust = 0, size = 5,
           label = "Busy & Underperforming",
           family = "Fira Code") +
  annotate("text",
           x = -0.29, y = 2.1, hjust = 0, size = 5,
           label = "Not Busy & Underperforming",
           family = "Fira Code") +
  annotate("text",
           x = 0.18, y = 2.1, hjust = 0, size = 5,
           label = "Not Busy & Overperforming",
           family = "Fira Code") +
  annotate("text",
           x = 0.18, y = 6.2, hjust = 0, size = 5,
           label = "Busy & Overperforming",
           family = "Fira Code") +
  scale_color_manual(values = c("#457b9d", "#e63946", "#B34B7D", "#4D8C60", "#F7BC4D")) +
  scale_x_continuous(labels = function(x) scales::percent(x, accuracy = 1), breaks = seq(-0.3, 0.4, 0.1), limits = c(-0.3, 0.4)) +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  theme(
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.title = element_blank(),
    legend.position = c(0.95, 0.75),
    legend.direction = "vertical",
    legend.spacing = unit(1, "mm"),
    legend.background = element_blank(),
    legend.justification = "right",
    legend.margin = margin(0, 0.2, 0.2, 0.2, unit = "cm"),
    legend.box.background = element_rect(colour = "black"),
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_text(size = 16, family = "Montserrat", color = "grey30"),
    plot.caption = element_text(size = 14, color = "#a8a8a8"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    panel.grid.major = element_line(color = "#F3F5F6", size = 0.4),
    panel.grid.minor = element_blank())

ggsave(here::here("sports", "goalkeeper-analysis", "figures", "workload2021.png"), dpi = 320, width = 16, height = 9)
