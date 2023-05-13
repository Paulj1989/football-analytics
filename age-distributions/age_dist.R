library(dplyr)
library(ggplot2)
library(glue)

age_data <- readr::read_csv(here::here("age-distributions", "data", "age.csv"))

# plot theme
# set minimal theme and specify font
theme_set(theme_minimal(base_family = "Inter", base_size = 14)) +
  # update theme to configure legend and increase plot margin
  theme_update(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 22),
    plot.subtitle = element_text(size = 16, color = "grey40"),
    plot.caption = element_text(size = 12, color = "grey60"),
    strip.text = element_text(size = 14, margin = margin(t = 10, b = 20)),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14, color = "grey20"),
    legend.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    legend.spacing = unit(1, "mm"),
    axis.text = element_text(size = 12),
    plot.margin = unit(c(0.5, 1, 1, 1), "cm"),
    panel.grid.major.y = element_line(color = "grey90", size = 0.8),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

## %######################################################%##
#                                                          #
####             Age Distribution by League             ####
#                                                          #
## %######################################################%##

league_age <-
  age_data |>
  count(league, age)

age_data |>
  group_by(league) |>
  summarize(mean_age = mean(age))

facet_labels <- as_labeller(
  c(
    "Bundesliga" = "Bundesliga\n\nMean = 25.5",
    "La Liga" = "La Liga\n\nMean = 26.6",
    "Ligue 1" = "Ligue 1\n\nMean = 25.1",
    "Premier League" = "Premier League\n\nMean = 26.3",
    "Serie A" = "Serie A\n\nMean = 26.7"
  )
)

# Comparing the Big Five Leagues
league_age |>
  ggplot(aes(x = age, y = n, fill = league)) +
  ggchicklet::geom_chicklet(alpha = 0.9) +
  geom_hline(yintercept = 0, color = "grey20", linewidth = 1.2) +
  scale_y_continuous(breaks = seq(0, 60, 10), labels = seq(0, 60, 10)) +
  labs(
    title = glue("The Age Distribution of Players in the Big Five Leagues"),
    subtitle = glue("Players Receiving Any Minutes in the Big Five ",
                    "Leagues This Season 2020/21 (As of Mar. 03, 2021)"),
    x = NULL, y = NULL,
    caption = "Data: FB Ref/StatsBomb | Graphic: @paul_johnson89"
  ) +
  scale_fill_manual(values = c("#4A4A4A", "#B31945","#FF0D57", 
                               "#1E88E5", "#1D264F")) +
  facet_wrap(vars(league), ncol = 5, labeller = facet_labels)

ggsave(here::here("age-distributions", "figures", "big_five_age.png"),
       dpi = 320, width = 16, height = 9)

# Comparing the Bundesliga & the Premier League
ggplot() +
  ggchicklet::geom_chicklet(
    data = league_age |>
      filter(league == "Bundesliga"),
    aes(x = age, y = -n, fill = league),
    alpha = 0.9
  ) +
  ggchicklet::geom_chicklet(
    data = league_age |>
      filter(league == "Premier League"),
    aes(x = age, y = n, fill = league),
    alpha = 0.9
  ) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "grey20", size = 2) +
  scale_y_continuous(breaks = seq(-50, 50, 10),
                     labels = abs(seq(-50, 50, 10))) +
  scale_x_continuous(breaks = seq(16, 40, 2),
                     labels = seq(16, 40, 2)) +
  labs(
    title = 
      "The Age Distribution of Players in the Premier League & the Bundesliga",
    subtitle = glue("Players Receiving Any Minutes in the Bundesliga or the ",
                    "Premier League This Season 2020/21 (As of Mar. 03, 2021)"),
    x = NULL, y = NULL,
    caption = "Data: FB Ref/StatsBomb | Graphic: @paul_johnson89"
  ) +
  annotate(
    "text", 37, 16,
    label = glue("The Age Distributions are Similar Between\n",
                 "the Two Leagues, but the Bundesliga is Slightly\n",
                 "Younger than the Premier League Because Players\n",
                 "First-Team Minutes from a Younger Age"),
    color = "grey20",
    hjust = 0, size = 5.5
  ) +
  scale_fill_manual(values = c("#FF0D57", "#1E88E5"))

ggsave(here::here("age-distributions", "figures", "buli_epl_age.png"),
       dpi = 320, width = 16, height = 9)

# Stream Plot
ggplot(league_age, aes(age, n, fill = league)) +
  ggstream::geom_stream(type = "mirror", bw = 0.65, alpha = 0.8) +
  labs(
    title = "The Age Distribution of Players in the Big Five Leagues",
    subtitle = glue("Players Receiving Any Minutes in the Big Five Leagues ",
                    "This Season 2020/21 (As of Mar. 03, 2021)"),
    x = NULL, y = NULL,
    caption = "Data: FB Ref/StatsBomb | Graphic: @paul_johnson89"
  ) +
  scale_fill_manual(values = c("#4A4A4A", "#B31945", "#FF0D57",
                               "#1E88E5", "#1D264F"))

ggsave(here::here("age-distributions", "figures", "stream.png"),
       dpi = 320, width = 16, height = 9)

## %######################################################% ##
#                                                            #
####            Age Distribution by Position              ####
#                                                            #
## %######################################################% ##

pos_age <-
  age_data |>
  count(position, age) |>
  mutate(position = forcats::as_factor(position)) |>
  mutate(position = forcats::fct_relevel(position, c("GK", "DF", "MF", "FW")))

ggplot(pos_age, aes(x = age, y = n, fill = position)) +
  ggchicklet::geom_chicklet(alpha = 0.9) +
  geom_hline(yintercept = 0, color = "grey20", size = 1.2) +
  scale_y_continuous(
    breaks = seq(0, 250, 50), labels = (seq(0, 250, 50)),
    limits = c(0, 250)
  ) +
  scale_x_continuous(breaks = seq(16, 43, 1), labels = seq(16, 43, 1)) +
  labs(
    title =
      "The Age Distribution of Players, by Position, in the Big Five Leagues",
    subtitle = glue("Players Receiving Any Minutes in the Big Five ",
                    "Leagues This Season 2020/21 (As of Mar. 03, 2021)"),
    x = NULL, y = NULL,
    caption = "Data: FB Ref/StatsBomb | Graphic: @paul_johnson89"
  ) +
  annotate(
    "text", 33, 200,
    label = glue("Goalkeepers - Mean = 30.5, Std. Dev. = 6.89\n",
                 "Defenders - Mean = 28.1, Std. Dev. = 7.58\n",
                 "Midfielders - Mean = 27.1, Std. Dev. = 6.48\n",
                 "Forwards - Mean = 27.5, Std. Dev. = 7.07\n"),
    color = "grey20", hjust = 0, size = 6
  ) +
  scale_fill_manual(values = c("#4A4A4F", "#FF0D57", "#8E1A53", "#1E88E5"))

ggsave(here::here("age-distributions", "figures", "positions.png"),
       dpi = 320, width = 16, height = 9)
