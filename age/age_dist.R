pacman::p_load(tidyverse, ggchicklet, ggtext, ggstream)

age_data <- read_csv(here::here("age", "age.csv"))

##%######################################################%##
#                                                          #
####             Age Distribution by League             ####
#                                                          #
##%######################################################%##

league_age <- 
  age_data %>% 
  count(league, age)

# Comparing the Big Five Leagues
league_age %>%
  ggplot(aes(x = age, y = n, fill = league)) +
  geom_chicklet(alpha = 0.9) +
  geom_hline(yintercept = 0, color = "grey30", size = 1.2) +
  scale_y_continuous(breaks = seq(0, 60, 10), labels = seq(0, 60, 10)) +
  labs(
    title = glue::glue("The Age Distribution of Players in the Big Five Leagues"),
    subtitle = glue::glue("Players Receiving Any Minutes in the Big Five Leagues This Season
                          2020/21 (As of Mar. 03, 2021)"),
    caption = "Data: FB Ref/StatsBomb | Graphic: @paul_johnson89") +
  theme_minimal(base_family = "Fira Code", base_size = 16) +
  scale_fill_manual(values = c("#4A4A4A", "#B31945",
                               "#FF0D57", "#1E88E5", "#1D264F")) +
  theme(
    legend.position = "none",
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_text(size = 16, family = "Montserrat",
                                 color = "grey40"),
    plot.caption = element_text(size = 12, family = "Montserrat",
                                color = "grey60"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.title = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.8),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()) +
  facet_wrap(~league, ncol = 5)

ggsave(here::here("age", "big_five_age.png"),
       dpi = 320, width = 16, height = 9)

# Comparing the Bundesliga & the Premier League
ggplot() +
  geom_chicklet(data = league_age %>% 
                  filter(league == "Bundesliga"),
                aes(x = age, y = -n, fill = league),
                alpha = 0.9) +
  geom_chicklet(data = league_age %>% 
                  filter(league == "Premier League"),
                aes(x = age, y = n, fill = league),
                alpha = 0.9) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "grey30", size = 2) +
  scale_y_continuous(breaks = seq(-50, 50, 10), labels = abs(seq(-50, 50, 10))) +
  scale_x_continuous(breaks=seq(16, 40, 2), labels = seq(16, 40, 2)) +
  labs(
    title = glue::glue("The Age Distribution of Players in the Premier League & the Bundesliga"),
    subtitle = glue::glue("Players Receiving Any Minutes in the Bundesliga or the Premier League This Season
                          2020/21 (As of Mar. 03, 2021)"),
    caption = "Data: FB Ref/StatsBomb | Graphic: @paul_johnson89") +
  annotate("text", 37, 16, 
  label = glue::glue("The Age Distributions are Similar Between 
                     the Two Leagues, but the Bundesliga is Slightly
                     Younger than the Premier League Because Players
                     First-Team Minutes from a Younger Age"),
  family = "Montserrat", color = "grey20",
  hjust = 0, size = 5.5) +
  theme_minimal(base_family = "Fira Code", base_size = 16) +
  scale_fill_manual(values = c("#FF0D57", "#1E88E5")) +
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
    panel.grid.major.y = element_line(color = "grey90", size = 0.8),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

ggsave(here::here("age", "buli_epl_age.png"),
     dpi = 320, width = 16, height = 9)

# Stream Plot
ggplot(league_age, aes(age, n, fill = league)) +
  geom_stream(type = "mirror", bw = 0.65, alpha = 0.8) +
  labs(
    title = glue::glue("The Age Distribution of Players in the Big Five Leagues"),
    subtitle = glue::glue("Players Receiving Any Minutes in the Big Five Leagues This Season
                          2020/21 (As of Mar. 03, 2021)"),
    caption = "Data: FB Ref/StatsBomb | Graphic: @paul_johnson89") +
  theme_minimal(base_family = "Fira Code", base_size = 16) +
  scale_fill_manual(values = c("#4A4A4A", "#B31945",
                               "#FF0D57", "#1E88E5", "#1D264F")) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, family = "Montserrat",
                               color = "grey20"),
    legend.spacing = unit(1, "mm"),
    legend.background = element_blank(),
    legend.margin = margin(0, 0.2, 0.2, 0.2, unit = "cm"),
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_text(size = 16, family = "Montserrat",
                                 color = "grey40"),
    plot.caption = element_text(size = 12, family = "Montserrat",
                                color = "grey60"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.title = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.8),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

ggsave(here::here("age", "stream.png"),
       dpi = 320, width = 16, height = 9)

##%######################################################%##
#                                                          #
####            Age Distribution by Position            ####
#                                                          #
##%######################################################%##

pos_age <- 
  age %>%
  count(position, age) %>%
  mutate(position = as_factor(position)) %>%
  mutate(position = fct_relevel(position, c("GK", "DF", "MF", "FW")))

ggplot(pos_age, aes(x = age, y = n, fill = position)) +
  geom_chicklet(alpha = 0.9) +
  geom_hline(yintercept = 0, color = "grey30", size = 1.2) +
  scale_y_continuous(breaks = seq(0, 250, 50), labels = (seq(0, 250, 50)),
                     limits = c(0, 250)) +
  scale_x_continuous(breaks=seq(16, 43, 1), labels = seq(16, 43, 1)) +
  labs(
    title = glue::glue("The Age Distribution of Players, by Position, in the Big Five Leagues"),
    subtitle = glue::glue("Players Receiving Any Minutes in the Big Five Leagues This Season
                          2020/21 (As of Mar. 03, 2021)"),
    caption = "Data: FB Ref/StatsBomb | Graphic: @paul_johnson89") +
  annotate("text", 33, 200, 
           label = glue::glue("Goalkeepers - Mean = 30.5, Std. Dev. = 6.89
                              Defenders - Mean = 28.1, Std. Dev. = 7.58
                              Midfielders - Mean = 27.1, Std. Dev. = 6.48
                              Forwards - Mean = 27.5, Std. Dev. = 7.07"),
           family = "Montserrat", color = "grey20",
           hjust = 0, size = 6) +
  theme_minimal(base_family = "Fira Code", base_size = 16) +
  scale_fill_manual(values = c("#4A4A4F", "#FF0D57", "#8E1A53", "#1E88E5")) +
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
    panel.grid.major.y = element_line(color = "grey90", size = 0.8),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

ggsave(here::here("age", "distribution.png"),
       dpi = 320, width = 16, height = 9)
