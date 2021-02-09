
##%######################################################%##
#                                                          #
####               Potential Replacements               ####
#                                                          #
##%######################################################%##

# packages
pacman::p_load(tidyverse, gt)

# data
df <- read_csv(here::here("sports", "goalkeeper-analysis", "big_five.csv"))


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
    progressive_distance90 = sum(progressive_distance)/nineties,
    .groups = "keep"
  ) %>%
  filter(full_name %in% c("Walter Benitez", "Koen Casteels",
                          "Roman Bürki", "Martin Dúbravka",
                          "Predrag Rajković", "Alphonse Areola",
                          "Bartłomiej Drągowski", "Mike Maignan",
                          "Walter Benítez"
                          )) %>%
  select(
    full_name, team, mins, ga90, psxg_plus_minus90,
    psxg_percent, opa90, opa_avg_dist, cross_stopped_percent,
    progressive_distance90, launch_comp_percent
  )

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
    label = "Shot-Prevention",
    columns = vars(opa90, opa_avg_dist,
                   cross_stopped_percent)) %>%
  tab_spanner(
    label = "Distribution",
    columns = vars(progressive_distance90,
                   launch_comp_percent)) %>%
  tab_footnote(footnote = "Post-Shot Expected Goals +/- Per 90",
               locations = cells_column_labels(
                 columns = vars(psxg_plus_minus90)
               )) %>%
  tab_footnote(footnote = "Progressive Passing Yards Per 90",
               locations = cells_column_labels(
                 columns = vars(progressive_distance90)
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
    progressive_distance90 = html("Progressive Distance/90"),
    launch_comp_percent = html("Long-Pass Completion %"),
    opa90 = html("#OPA/90"),
    opa_avg_dist = html("Average Distance OPA"),
    cross_stopped_percent = html("Crosses Stopped %")) %>%
  tab_style(
    style = list(
      cell_fill(color = "#e01e37", alpha = 0.8),
      cell_text(color = "white")),
    locations = cells_body(
      columns = vars(cross_stopped_percent),
      rows = cross_stopped_percent < 0.076)) %>%
  tab_style(
    style = list(
      cell_fill(color = "#1a936f", alpha = 0.8),
      cell_text(color = "white")),
    locations = cells_body(
      columns = vars(cross_stopped_percent),
      rows = cross_stopped_percent > 0.076)) %>%
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
        launch_comp_percent,
        cross_stopped_percent),
      rows = team == "Dortmund")) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")),
    locations = cells_body(
      rows = full_name == "Roman Bürki")) %>%
  row_group_order(
    groups = c("Roman Bürki", "Alphonse Areola",
               "Walter Benítez", "Koen Casteels",
               "Martin Dúbravka", "Bartłomiej Drągowski",
               "Mike Maignan", "Predrag Rajković")) %>%
  fmt_percent(columns = c(6, 9, 11), decimals=1, scale_values = TRUE) %>%
  fmt_number(columns = c(4, 5, 7, 8, 10), decimals = 2) %>%
  tab_options(row_group.font.weight = "bold") %>%
  opt_table_font(
    font = list(
      google_font(name = "Montserrat")))

gtsave(comp_tab, here::here("sports", "goalkeeper-analysis", "figures", "table.png"), expand = 12, vwidth = 1600, vheight = 900)
