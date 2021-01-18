pacman::p_load(tidyverse, here, ggrepel, ggtext)

shots <-readr::read_csv("shot_value.csv")


shots <- shots %>%
  mutate(
    text_color = case_when(
      team == "Borussia Dortmund"~ "#343a40",
      team == "Bayern Munich"~ "#343a40",
      TRUE ~ "grey60"
    )
  )

ggplot(shots, aes(x = npxg, y = npxga, color=league)) +
  geom_vline(xintercept = 1.16, linetype = "dashed", color="grey80") +
  geom_hline(yintercept = 1.18, linetype = "dashed", color="grey80") +
  geom_point(aes(),
             shape = 21, size = 2.5,
             alpha = ifelse(shots$text_color == "#343a40", 1, 0.7),
             stroke = 1.2
  ) +
  geom_text_repel(
    aes(label = team),
    min.segment.length = 0, seed = 15, size = ifelse(shots$text_color == "#343a40", 4, 3.2),
    point.padding = 0.2, box.padding = 0.3, segment.alpha = 0, color = shots$text_color
  ) +
  coord_cartesian(ylim = c(0.6, 2.2), xlim = c(0.6, 2.2), clip = "off") +
  # y arrows & labels
  geom_segment(aes(x = 1.2, xend = 1, y = 0.4, yend = 0.4),
               size = 0.5, alpha=0.6, arrow = arrow(length = unit(0.3, "cm")), color="#DE2141") +
  geom_segment(aes(x = 1.6, xend = 1.8, y = 0.4, yend = 0.4),
               size = 0.5, alpha=0.6, arrow = arrow(length = unit(0.3, "cm")), color="#007F5F") +
  annotate("text", 1.4, 0.4, label="npxG/90",
           size = 3.5, color="grey30", family = "Fira Code") +
  # x arrows & labels
  geom_segment(aes(x = 0.45, xend = 0.45, y = 1.2, yend = 0.9),
               size = 0.5, alpha=0.6, arrow = arrow(length = unit(0.3, "cm")), color="#007F5F") +
  geom_segment(aes(x = 0.45, xend = 0.45, y = 1.6, yend = 1.9),
               size = 0.5, alpha=0.6, arrow = arrow(length = unit(0.3, "cm")), color="#DE2141") +
  annotate("text", 0.45, 1.4, angle=90, label="npxGA/90",
           size = 3.5, color="grey30", family = "Fira Code") +
  # annotation arrows
  annotate("curve", x = 1.78, xend = 1.89, y = 1.38, yend = 1.31,
           arrow = arrow(length = unit(.2,"cm"))) +
  annotate("text", 1.78, 1.45, label="Bayern Munich's\nnpxGDiff/90 = 0.58",
           size = 3.5, color="grey30", family = "Fira Code") +
  annotate("curve", x = 1.9, xend = 1.99, y = 0.7, yend = 0.96,
           curvature = -0.3, arrow = arrow(length = unit(.2,"cm"))) +
  annotate("text", 1.9, 0.625, label="Borussia Dortmund's\nnpxGDiff/90 = 1.04",
           size = 3.5, color="grey30", family = "Fira Code") +
  # title labels
  annotate("text", 1.25, 2.2,
           label = "Team Performance in Europe's Top 5 Leagues",
           family = "IBM Plex Sans Bold", color = "black",
           hjust = 0, size = 8, lineheight = 0.8) +
  annotate("text", 1.25, 2.1,
           label = "Comparing Team Non-Penalty Expected Goals (npxG) For & Against This Season",
           family = "IBM Plex Sans", color = "black",
           hjust = 0, size = 5.5, lineheight = 1) +
  annotate("text", 1.25, 2,
           label = "Source: FB Ref/StatsBomb | Graphic: @paul_johnson89",
           family = "IBM Plex Sans", color = "black",
           hjust = 0, size = 3.5, lineheight = 0.5) +
  theme_minimal(base_family = "Fira Code", base_size = 14) +
  scale_color_manual(values = c("#457b9d", "#e63946", "#B34B7D", "#4D8C60", "#F7BC4D")) +
  theme(
    plot.margin = margin(0.8, 0.5, 1.8, 1.8, unit = "cm"),
    legend.title = element_blank(),
    legend.position = c(0.92, 0.7),
    legend.direction = "vertical",
    legend.spacing = unit(1, "mm"),
    legend.background = element_blank(),
    legend.justification = "right",
    legend.margin = margin(0, 0.2, 0.2, 0.2, unit='cm'),
    legend.box.background = element_rect(colour = "black"),
    axis.text = element_markdown(size = 10),
    axis.title = element_blank(),
    panel.grid.major = element_line(color = "#F3F5F6", size = 0.4),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    oob = scales::squish,
    labels = seq(0.6, 2.2, 0.2),
    breaks = seq(0.6, 2.2, 0.2),
    limits = c(0.35, 2.2)) +
  scale_y_continuous(
    oob = scales::squish,
    labels = seq(0.6, 2.2, 0.2),
    breaks = seq(0.6, 2.2, 0.2),
    limits = c(0.4, 2.2)) +
  # save image
  ggsave(here::here("npxg.png"), dpi = 320, width = 16, height = 9)


