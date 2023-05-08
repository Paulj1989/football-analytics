pacman::p_load(tidyverse, ggbump, ggrepel, ggtext, glue, patchwork)

df <- read_csv(here::here("epl", "results.csv"))

df <- df %>%
  mutate(
    lubridate::ymd(date),
    xg_diff = xg - xg_against,
    goal_diff = goals - goals_against,
    xg_plus_minus = round(goal_diff - xg_diff, digits = 2))

bvb <- df %>%
  filter(team == "Dortmund") %>%
  select(season, round, xg, xg_against, goals, goals_against) %>%
  arrange(season, round) %>%
  mutate(
    xg_avg = zoo::rollmean(xg, k = 5, align = "right", fill = NA),
    xga_avg = zoo::rollmean(xg_against, k = 5, align = "right", fill = NA),
    g_avg = zoo::rollmean(goals, k = 5, align = "right", fill = NA),
    ga_avg = zoo::rollmean(goals_against, k = 5, align = "right", fill = NA),
    cumsum_xg = cumsum(xg),
    cumsum_xga = cumsum(xg_against),
    cumsum_goals = cumsum(goals),
    cumsum_goals_against = cumsum(goals_against),
    total_rounds = row_number())

##%######################################################%##
#                                                          #
####                         xG                         ####
#                                                          #
##%######################################################%##

bvb %>%
  gather(key = "for_against", value = "xg", xg_avg, xga_avg) %>%
  ggplot() +
  # plot xg
  geom_bump(
    aes(
      x = total_rounds, y = xg,
      color = for_against
    ), size = 0.8, smooth = 10
  ) +
  geom_vline(xintercept = 34, linetype = "dashed", color = "grey85") +
  geom_vline(xintercept = 68, linetype = "dashed", color = "grey85") +
  # title and caption
  annotate("text", x = 6, y = 0.42, label = "BVB's xG Difference in the Bundesliga", hjust = 0, family = "Montserrat", size = 10) +
  annotate("text", x = 6, y = 0.25, label = "(Rolling) Mean Average Expected Goals (xG)",
           hjust = 0, family = "Montserrat", color = "grey40", size = 6.5) +
  annotate("text", x = 40.5, y = 0.25, label = "For",
           hjust = 0, family = "Montserrat", fontface = "bold",
           color = "#0466c8", size = 6.5) +
  annotate("text", x = 43.5, y = 0.25, label = "&",
           hjust = 0, family = "Montserrat", color = "grey40", size = 6.5) +
  annotate("text", x = 45, y = 0.25, label = "Against",
           hjust = 0, family = "Montserrat", fontface = "bold",
           color = "#ef233c", size = 6.5) +
  annotate("text", x = 51.8, y = 0.25, label = "Since 2018",
           hjust = 0, family = "Montserrat", color = "grey40", size = 6.5) +
  annotate("text", x = 6, y = 0.12, label = "Source: FB Ref/StatsBomb | Graphic: @paul_johnson89", hjust = 0, family = "Montserrat", color = "grey50", size = 4) +
  scale_y_continuous(breaks = seq(0, 3, 1), labels = seq(0, 3, 1), limits = c(0, 3), NULL) +
  scale_x_continuous(position = "top", limits = c(5, 88), breaks = c(17, 51, 80), labels = c("18/19", "19/20", "20/21")) +
  scale_colour_manual(values = c("#0466c8", "#ef233c")) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Fira Code", base_size = 15) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(colour = "#343854"),
    axis.text.x = element_text(hjust = 0.1, colour = "#343854"),
    axis.text.y = element_text(hjust = 1, colour = "#343854"),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color="#F8F9FA"),
    panel.grid.minor.y = element_blank())

ggsave(here::here("bvb-xg", "xg_for_against.png"), dpi = 320, width = 16, height = 9)

##%######################################################%##
#                                                          #
####                       Goals                        ####
#                                                          #
##%######################################################%##


bvb %>%
  gather(key = "for_against", value = "goals", g_avg, ga_avg) %>%
  ggplot() +
  # plot xg
  geom_bump(
    aes(
      x = total_rounds, y = goals,
      color = for_against
    ), size = 0.8, smooth = 10
  ) +
  geom_vline(xintercept = 34, linetype = "dashed", color = "grey85") +
  geom_vline(xintercept = 68, linetype = "dashed", color = "grey85") +
  # title and caption
  annotate("text", x = 6, y = 0.35, label = "BVB's Goal Difference in the Bundesliga", hjust = 0, family = "Montserrat", size = 10) +
  annotate("text", x = 6, y = 0.12, label = "(Rolling) Mean Average Goals",
           hjust = 0, family = "Montserrat", color = "grey40", size = 6.5) +
  annotate("text", x = 29.2, y = 0.12, label = "For",
           hjust = 0, family = "Montserrat", fontface = "bold",
           color = "#0466c8", size = 6.5) +
  annotate("text", x = 32.2, y = 0.12, label = "&",
           hjust = 0, family = "Montserrat", color = "grey40", size = 6.5) +
  annotate("text", x = 33.8, y = 0.12, label = "Against",
           hjust = 0, family = "Montserrat", fontface = "bold",
           color = "#ef233c", size = 6.5) +
  annotate("text", x = 40.5, y = 0.12, label = "Since 2018",
           hjust = 0, family = "Montserrat", color = "grey40", size = 6.5) +
  annotate("text", x = 6, y = -0.06, label = "Source: FB Ref/StatsBomb | Graphic: @paul_johnson89", hjust = 0, family = "Montserrat", color = "grey50", size = 4) +
  scale_y_continuous(breaks = seq(0, 4, 1), labels = seq(0, 4, 1), limits = c(-0.06, 4.4), NULL) +
  scale_x_continuous(position = "top", limits = c(5, 88), breaks = c(17, 51, 80), labels = c("18/19", "19/20", "20/21")) +
  scale_colour_manual(values = c("#0466c8", "#ef233c")) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Fira Code", base_size = 15) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(colour = "#343854"),
    axis.text.x = element_text(hjust = 0.1, colour = "#343854"),
    axis.text.y = element_text(hjust = 1, colour = "#343854"),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color="#F8F9FA"),
    panel.grid.minor.y = element_blank())

ggsave(here::here("bvb-xg", "goals_for_against.png"), dpi = 320, width = 16, height = 9)

##%######################################################%##
#                                                          #
####                      20/21                         ####
#                                                          #
##%######################################################%##

bvb2021 <- df %>%
  filter(team == "Dortmund" & season == 2021) %>%
  select(season, round, xg, xg_against, goals, goals_against) %>%
  arrange(season, round) %>%
  mutate(
    xg_avg = zoo::rollmean(xg, k = 3, align = "right", fill = NA),
    xga_avg = zoo::rollmean(xg_against, k = 3, align = "right", fill = NA),
    g_avg = zoo::rollmean(goals, k = 3, align = "right", fill = NA),
    ga_avg = zoo::rollmean(goals_against, k = 3, align = "right", fill = NA),
    cumsum_xg = cumsum(xg),
    cumsum_xga = cumsum(xg_against),
    cumsum_goals = cumsum(goals),
    cumsum_goals_against = cumsum(goals_against),
    total_rounds = row_number())

xg_plot <-
  bvb2021 %>%
  gather(key = "for_against", value = "xg", xg_avg, xga_avg) %>%
  ggplot() +
  # plot xg
  geom_bump(
    aes(
      x = total_rounds, y = xg,
      color = for_against
    ), size = 0.8, smooth = 10
  ) +
  geom_vline(xintercept = 11, linetype = "dashed", color = "grey70") +
  # title and caption
  annotate("text", x = 3, y = 2.8, label = "xG", hjust = 0, family = "Montserrat", size = 8) +
  scale_y_continuous(breaks = seq(0, 3, 1), labels = seq(0, 3, 1), limits = c(0, 3), NULL) +
  scale_x_continuous(position = "top", limits = c(3, 20), breaks = c(5.5, 15), labels = c("Lucien Favre", "Edin Terzic")) +
  scale_colour_manual(values = c("#0466c8", "#ef233c")) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(colour = "#343854"),
    axis.text.x = element_text(hjust = 0.1, colour = "#343854"),
    axis.text.y = element_text(hjust = 1, colour = "#343854"),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color="#F8F9FA"),
    panel.grid.minor.y = element_blank())

goals_plot <- 
  bvb2021 %>%
  gather(key = "for_against", value = "goals", g_avg, ga_avg) %>%
  ggplot() +
  # plot xg
  geom_bump(
    aes(
      x = total_rounds, y = goals,
      color = for_against
    ), size = 0.8, smooth = 10
  ) +
  geom_vline(xintercept = 11, linetype = "dashed", color = "grey70") +
  # title and caption
  annotate("text", x = 3, y = 2.8, label = "Goals", hjust = 0, family = "Montserrat", size = 8) +
  scale_y_continuous(breaks = seq(0, 3, 1), labels = seq(0, 3, 1), limits = c(0, 3), NULL) +
  scale_x_continuous(position = "top", limits = c(3, 20), breaks = c(5.5, 15), labels = NULL) +
  scale_colour_manual(values = c("#0466c8", "#ef233c")) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Fira Code", base_size = 18) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(colour = "#343854"),
    axis.text.x = element_text(hjust = 0.1, colour = "#343854"),
    axis.text.y = element_text(hjust = 1, colour = "#343854"),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color="#F8F9FA"),
    panel.grid.minor.y = element_blank())

# combine both plots
season_plot <- xg_plot + goals_plot + plot_layout(ncol=1)

season_plot <- season_plot + plot_annotation(
  title = "The Decline in BVB's Performances From Favre to Terzic",
  subtitle = glue("(Rolling) Mean Average xG & Goals
                  <span style = 'color:#0466c8;'>**For**</span>/<span style = 'color:#ef233c;'>**Against**</span>
                  This Season"),
  caption = "Source: FB Ref/StatsBomb | Graphic: @paul_johnson89",
  theme = theme(
    plot.title = element_markdown(family = "Montserrat", color = "grey10", size = 26),
    plot.subtitle = element_markdown(family = "Montserrat", color = "grey30", size = 20),
    plot.caption = element_text(family = "Montserrat", color = "grey50", size = 14)))

season_plot

ggsave(here::here("bvb-xg", "manager_performance.png"), dpi = 320, width = 16, height = 9)
