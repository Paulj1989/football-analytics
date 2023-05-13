pacman::p_load(tidyverse, ggstream)

touches <- read_csv(here::here("guerreiro", "touches.csv"))

touches <- touches %>%
  gather(
    key = "type", value = "touches",
    def_pen, def_third, mid_third,
    att_third, att_pen
  ) %>%
  mutate(type = as_factor(type)) %>%
  mutate(
    type =
      fct_relevel(
        type,
        c(
          "def_pen", "def_third",
          "mid_third", "att_third",
          "att_pen")))

ggplot(touches, aes(round, touches, fill = type)) +
  geom_stream(type = "mirror", alpha = 0.8, bw = 0.6, extra_span = 0.1) +
  labs(
    title = glue::glue("Where Guerriero is Touching the Ball in the Bundesliga This Season"),
    subtitle = glue::glue("The Distribution of Guerreiro's Touches, by Pitch Zone, in Bundesliga Matches
                          2020/21 (Matchday 23) | Minimum 45 mins"),
    caption = "Data: FB Ref/StatsBomb | Graphic: @paul_johnson89") +
  theme_minimal(base_family = "Fira Code", base_size = 16) +
  scale_fill_manual(values = c("#4A4A4A", "#B31945", 
                               "#FF0D57", "#1E88E5", "#1D264F"),
                    labels = c("Defensive Pen", "Defensive Third",
                               "Mid Third", "Attacking Third",
                               "Attacking Pen")) +
  scale_y_continuous(breaks = seq(-100, 100, 50), labels = abs(seq(-100, 100, 50))) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(
      size = 12, family = "Montserrat",
      color = "grey20"),
    legend.spacing = unit(1, "mm"),
    legend.background = element_blank(),
    legend.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm"),
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    plot.title = element_text(size = 22, family = "Montserrat"),
    plot.subtitle = element_text(
      size = 16, family = "Montserrat",
      color = "grey40"
    ),
    plot.caption = element_text(
      size = 12, family = "Montserrat",
      color = "grey60"
    ),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.title = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.8),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

ggsave(here::here("guerreiro", "touches.png"),
  dpi = 320, width = 16, height = 9)
