##%######################################################%##
#                                                          #
####          Identifying Burki's Replacement           ####
#                                                          #
##%######################################################%##

# packages
pacman::p_load(tidyverse, gt, ggrepel)

# data
df <- read_csv(here::here("goalkeeper-analysis", "big_five.csv"))

##%######################################################%##
#                                                          #
####    Analyzing the Shortlist of Transfer Targets     ####
#                                                          #
##%######################################################%##

# wrangling

comp <- df %>%
  group_by(full_name, surname) %>%
  summarize(
    nineties = sum(`90s`),
    mins = sum(mins),
    goals_against = sum(goals_against),
    own_goals = sum(own_goals),
    ga90 = goals_against / nineties,
    set_piece_goals = sum(goals_ck)+sum(goals_fk),
    sp_goal_percent = set_piece_goals/sum(goals_against),
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
    pass_completion = sum(passes_completed)/sum(passes_attempted),
    launched = sum(launched),
    launch_att = sum(launch_att),
    launch_att90 = launch_att / nineties,
    launch_comp_percent = launched / launch_att,
    progressive_passes = sum(progressive_passes),
    progressive_distance = sum(progressive_distance),
    touches90 = sum(touches)/nineties,
    .groups = "keep"
  ) %>%
  filter(full_name %in% c(
    "Mike Maignan", "Martin Dúbravka",
    "Manuel Neuer", "Péter Gulácsi",
    "Yann Sommer", "Lukáš Hrádecký",
    "Walter Benitez", "Koen Casteels",
    "Roman Bürki", "Predrag Rajković",
    "Alphonse Areola", "Bartłomiej Drągowski",
    "Walter Benítez", "Thomas Strakosha",
    "Neto", "Rafał Gikiewicz", "Paulo Gazzaniga"
  )) %>%
  select(
    full_name, surname, mins, nineties, sota, sota90, goals_against, sp_goal_percent, own_goals,
    ga90, psxg, psxg_sot, psxg_plus_minus, psxg90, psxg_percent, pass_att90, pass_completion,
    launch_att90, launch_comp_percent, crosses_faced, crosses_stopped, cross_stopped_percent,
    opa, opa_avg_dist, opa90, progressive_passes, progressive_distance, touches90
  )

# psxg %
comp %>%
  mutate(comp_color = ifelse(psxg_percent > 0, "type1", "type2")) %>%
  ggplot(aes(y = reorder(full_name, psxg_percent), x = psxg_percent, label = scales::percent(psxg_percent))) +
  geom_segment(aes(
    x = 0, xend = psxg_percent,
    y = reorder(full_name, psxg_percent),
    yend = reorder(full_name, psxg_percent),
    color = comp_color
  ), size = 12, alpha = 0.8) +
  
  geom_vline(xintercept = 0, color = "grey20", size = 1) +
  scale_x_continuous(labels = function(x) scales::percent(x, accuracy = 1), breaks = seq(-0.05, 0.25, 0.05), limits = c(-0.05, 0.25)) +
  scale_y_discrete(labels = c("Roman Bürki" = expression(bold("Roman Bürki")),
                              "Manuel Neuer" = expression(bold("Manuel Neuer")),
                              "Yann Sommer" = expression(bold("Yann Sommer")),
                              "Péter Gulácsi" = expression(bold("Péter Gulácsi")),
                              "Lukáš Hrádecký" = expression(bold("Lukáš Hrádecký")),
                              parse = TRUE)) +
  labs(
    title = glue::glue("Goalkeeper Shot-Stopping Over/Underperformance"),
    subtitle = glue::glue("Post-Shot Expected Goals as % of Goals (PSxG %)\n2017/18 - 2020/21"),
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

ggsave(here::here("goalkeeper-analysis", "figures", "psxg_comp.png"), dpi = 320, width = 16, height = 9)

# distribution

ggplot(comp, aes(x = pass_att90, y = touches90)) +
  geom_vline(xintercept = mean(comp$pass_att90), linetype = "dashed", color = "grey80") +
  geom_hline(yintercept = mean(comp$touches90), linetype = "dashed", color = "grey80") +
  geom_point(
    aes(),
    shape = 21, size = 2.5, stroke = 2,
    color = ifelse(comp$surname %in%
                     c("Bürki", "Neuer", "Sommer",
                       "Gulácsi", "Hrádecký"),
                   "#e63946", "grey20")
  ) +
  geom_text_repel(
    aes(label = full_name),
    nudge_y = 1,
    seed = 15, size = 5,
    direction = "both",
    point.padding = 0.5, segment.alpha = 0,
    color = ifelse(comp$surname %in%
                     c("Bürki", "Neuer", "Sommer",
                       "Gulácsi", "Hrádecký"),
                   "#e63946", "grey20")
  ) +
  scale_x_continuous(limits = c(15, 40)) +
  labs(
    title = glue::glue("Goalkeeper Contribution to Team Buildup"),
    subtitle = glue::glue("Passes Attempted Per 90 & Touches Per 90
                          2017/18 - 2020/21"),
    x = "Passes Attempted/90",
    y = "Touches/90",
    caption = "Data: FBref | StatsBomb") +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  theme(
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.position = "none",
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_text(size = 16, family = "Montserrat", color = "grey30"),
    plot.caption = element_text(size = 14, color = "#a8a8a8"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    panel.grid.major = element_line(color = "#F3F5F6", size = 0.4),
    panel.grid.minor = element_blank())

ggsave(here::here("goalkeeper-analysis", "figures", "distribution_comp.png"), dpi = 320, width = 16, height = 9)

ggplot(comp, aes(x = pass_completion, y = launch_comp_percent)) +
  geom_vline(xintercept = mean(comp$pass_completion), linetype = "dashed", color = "grey80") +
  geom_hline(yintercept = mean(comp$launch_comp_percent), linetype = "dashed", color = "grey80") +
  geom_point(
    aes(),
    shape = 21, size = 2.5, stroke = 2,
    color = ifelse(comp$surname %in%
                     c("Bürki", "Neuer", "Sommer",
                       "Gulácsi", "Hrádecký"),
                   "#e63946", "grey20")
  ) +
  geom_text_repel(
    aes(label = full_name),
    nudge_y = 0.01,
    min.segment.length = 1, seed = 15, size = 5,
    point.padding = 0.1, segment.alpha = 0,
    color = ifelse(comp$surname %in%
                     c("Bürki", "Neuer", "Sommer",
                       "Gulácsi", "Hrádecký"),
                   "#e63946", "grey20")
  ) +
  scale_x_continuous(labels = function(x) scales::percent(x, accuracy = 1),
                     breaks = seq(0.55, 0.85, 0.05), limits = c(0.55, 0.865)) +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1),
                     breaks = seq(0.3, 0.55, 0.05), limits = c(0.3, 0.58)) +
  labs(
    title = glue::glue("Goalkeeper Contribution to Buildup & Long-Ball Distribution"),
    subtitle = glue::glue("Total Pass Completion % & Long Pass Completion %
                          2017/18 - 2020/21"),
    x = "Pass Completion %",
    y = "Long Pass Completion %",
    caption = "Data: FBref | StatsBomb") +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  theme(
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.position = "none",
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_text(size = 16, family = "Montserrat", color = "grey30"),
    plot.caption = element_text(size = 14, color = "#a8a8a8"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    panel.grid.major = element_line(color = "#F3F5F6", size = 0.4),
    panel.grid.minor = element_blank())

ggsave(here::here("goalkeeper-analysis", "figures", "long_ball_comp.png"), dpi = 320, width = 16, height = 9)

# sweepers
ggplot(comp, aes(x = opa90, y = opa_avg_dist)) +
  geom_vline(xintercept = mean(big5$opa90), linetype = "dashed", color = "grey80") +
  geom_hline(yintercept = mean(big5$opa_avg_dist), linetype = "dashed", color = "grey80") +
  geom_point(
    aes(),
    shape = 21, size = 2.5, stroke = 2,
    color = ifelse(comp$surname %in%
                     c("Bürki", "Neuer", "Sommer",
                       "Gulácsi", "Hrádecký"),
                   "#e63946", "grey20")
  ) +
  geom_text_repel(
    aes(label = full_name),
    nudge_y = 0.15,
    min.segment.length = 1, seed = 15, size = 5,
    point.padding = 0.1, segment.alpha = 0,
    color = ifelse(comp$surname %in%
                     c("Bürki", "Neuer", "Sommer",
                       "Gulácsi", "Hrádecký"),
                   "#e63946", "grey20")
  ) +
  labs(
    title = glue::glue("Goalkeeper Defensive Actions Outside the Penalty Area"),
    subtitle = glue::glue("Defensive Actions Outside the Penalty Area Per 90 (#OPA/90) & Average Distance from Box (Average Distance OPA)
                          2017/18 - 2020/21"),
    x = "#OPA/90",
    y = "Average Distance OPA",
    caption = "Data: FBref | StatsBomb") +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  theme(
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.position = "none",
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_text(size = 16, family = "Montserrat", color = "grey30"),
    plot.caption = element_text(size = 14, color = "#a8a8a8"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    panel.grid.major = element_line(color = "#F3F5F6", size = 0.4),
    panel.grid.minor = element_blank())

ggsave(here::here("goalkeeper-analysis", "figures", "sweepers_comp.png"), dpi = 320, width = 16, height = 9)

# crosses stopped
comp %>%
  mutate(comp_color = ifelse(surname %in% c("Bürki", "Neuer", "Sommer",
                                            "Gulácsi", "Hrádecký"),
                             "type1", "type2")) %>%
  ggplot(aes(x = reorder(surname, cross_stopped_percent), y = cross_stopped_percent), label = scales::percent(cross_stopped_percent)) +
  geom_segment(aes(
    x = 0, xend = cross_stopped_percent,
    y = reorder(full_name, cross_stopped_percent),
    yend = reorder(full_name, cross_stopped_percent),
    color = comp_color
  ), size = 12, alpha = 0.9) +
  geom_vline(xintercept = 0, color = "grey20", size = 1) +
  scale_x_continuous(labels = function(x) scales::percent(x, accuracy = 1), breaks = seq(0, 0.1, 0.05), limits = c(0, 0.11)) +
  scale_y_discrete(labels = c("Roman Bürki" = expression(bold("Roman Bürki")),
                              "Manuel Neuer" = expression(bold("Manuel Neuer")),
                              "Yann Sommer" = expression(bold("Yann Sommer")),
                              "Péter Gulácsi" = expression(bold("Péter Gulácsi")),
                              "Lukáš Hrádecký" = expression(bold("Lukáš Hrádecký")),
                              parse = TRUE)) +
  labs(
    title = glue::glue("Goalkeeper Success Stopping Crosses"),
    subtitle = glue::glue("% of Crosses into the Penalty Area Successfully Stopped by Goalkeeper
                          2017/18 - 2020/21")) +
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

ggsave(here::here("goalkeeper-analysis", "figures", "crosses_comp.png"), dpi = 320, width = 16, height = 9)

##%######################################################%##
#                                                          #
####              Comparing the Finalists               ####
#                                                          #
##%######################################################%##

# wrangling
comp <- df %>%
  group_by(full_name, team) %>%
  summarize(
    nineties = sum(`90s`),
    mins = sum(mins),
    ga90 = sum(goals_against)/nineties,
    psxg_plus_minus90 = sum(psxg_plus_minus)/nineties,
    psxg_percent = (sum(psxg)/sum(goals_against-own_goals)) - 1,
    cross_stopped_percent = sum(crosses_stopped)/sum(crosses_faced),
    opa_avg_dist = mean(opa_avg_dist),
    opa90 = sum(opa)/nineties,
    launch_comp_percent = sum(launched)/sum(launch_att),
    pass_completion = sum(passes_completed)/sum(passes_attempted),
    touches90 = sum(touches)/nineties,
    .groups = "keep"
  ) %>%
  filter(full_name %in% c("Walter Benítez", "Koen Casteels",
                          "Roman Bürki", "Predrag Rajković",
                          "Alphonse Areola", "Thomas Strakosha"
                          )) %>%
  select(
    full_name, team, mins, ga90, psxg_plus_minus90,
    psxg_percent, opa90, opa_avg_dist, cross_stopped_percent,
    touches90, pass_completion, launch_comp_percent, 
  )

# gt table

comp_tab <-
  comp %>%
  select(-team) %>%
  group_by(full_name) %>%
  gt(rowname_col = "team") %>%
  tab_header(
    title = "Scouting Borussia Dortmund's Next Goalkeeper",
    subtitle = "Comparing Potential Goalkeeper Replacements with Roman Bürki"
  ) %>% 
  tab_source_note(
    source_note = "Data: FB Ref/StatsBomb"
  ) %>%
  tab_spanner(
    label = "Shot-Stopping",
    columns = vars(ga90, psxg_plus_minus90, psxg_percent)) %>%
  tab_spanner(
    label = "Shot Prevention",
    columns = vars(cross_stopped_percent, opa_avg_dist, opa90)) %>%
  tab_spanner(
    label = "Distribution",
    columns = vars(touches90, pass_completion,
                   launch_comp_percent)) %>%
  tab_footnote(footnote = "Post-Shot Expected Goals +/- Per 90",
               locations = cells_column_labels(
                 columns = vars(psxg_plus_minus90)
               )) %>%
  tab_footnote(footnote = "Defensive Actions Outside the Penalty Area Per 90",
               locations = cells_column_labels(
                 columns = vars(opa90)
               )) %>%
  cols_label(
    mins = html("Mins"),
    ga90 = html("GA/90"),
    psxg_plus_minus90 = html("PSxG/90"),
    psxg_percent = html("PSxG %"),
    touches90 = html("Touches/90"),
    pass_completion = html("Pass Completion %"),
    launch_comp_percent = html("Long-Pass Completion %"),
    opa90 = html("#OPA/90"),
    opa_avg_dist = html("Avg. Distance OPA"),
    cross_stopped_percent = html("Crosses Stopped %")) %>%
  tab_style(
    style = list(
      cell_fill(color = "#e01e37", alpha = 0.8),
      cell_text(color = "white")),
    locations = cells_body(
      columns = vars(opa90),
      rows = opa90 < 0.85)) %>%
  tab_style(
    style = list(
      cell_fill(color = "#1a936f", alpha = 0.8),
      cell_text(color = "white")),
    locations = cells_body(
      columns = vars(opa90),
      rows = opa90 > 0.85)) %>%
  tab_style(
    style = list(
      cell_fill(color = "#e01e37", alpha = 0.8),
      cell_text(color = "white")),
    locations = cells_body(
      columns = vars(psxg_percent),
      rows = psxg_percent < -0.033)) %>%
  tab_style(
    style = list(
      cell_fill(color = "#1a936f", alpha = 0.8),
      cell_text(color = "white")),
    locations = cells_body(
      columns = vars(psxg_percent),
      rows = psxg_percent > -0.033)) %>%
  tab_style(
    style = list(
      cell_fill(color = "#e01e37", alpha = 0.8),
      cell_text(color = "white")),
    locations = cells_body(
      columns = vars(pass_completion),
      rows = pass_completion < 0.7458848)) %>%
  tab_style(
    style = list(
      cell_fill(color = "#1a936f", alpha = 0.8),
      cell_text(color = "white")),
    locations = cells_body(
      columns = vars(pass_completion),
      rows = pass_completion > 0.7458848)) %>%
  tab_style(
    style = list(
      cell_fill(color = "#e01e37", alpha = 0.8),
      cell_text(color = "white")),
    locations = cells_body(
      columns = vars(launch_comp_percent),
      rows = launch_comp_percent < 0.366)) %>%
  tab_style(
    style = list(
      cell_fill(color = "#1a936f", alpha = 0.8),
      cell_text(color = "white")),
    locations = cells_body(
      columns = vars(launch_comp_percent),
      rows = launch_comp_percent > 0.366)) %>%
  tab_style(
    style = list(
      cell_fill(color = "grey30", alpha = 0.8),
      cell_text(color = "white",
                weight = "bold")),
    locations = cells_body(
      columns = vars(
        psxg_percent,
        opa90,
        pass_completion,
        launch_comp_percent),
      rows = team == "Dortmund")) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")),
    locations = cells_body(
      rows = full_name == "Roman Bürki")) %>%
  row_group_order(
    groups = c("Roman Bürki", "Alphonse Areola",
               "Walter Benítez", "Koen Casteels",
               "Predrag Rajković", "Thomas Strakosha")) %>%
  fmt_percent(columns = c(6, 9, 11, 12), decimals=1, scale_values = TRUE) %>%
  fmt_number(columns = c(4, 5, 7, 8, 10), decimals = 2) %>%
  tab_options(row_group.font.weight = "bold") %>%
  opt_table_font(
    font = list(
      google_font(name = "Montserrat")))

gtsave(comp_tab, here::here("goalkeeper-analysis", "figures", "table.png"), expand = 12, vwidth = 1600, vheight = 900)