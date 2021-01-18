pacman::p_load(tidyverse, here, ggrepel, ggtext)

shots <-readr::read_csv("shot_value.csv")


shots <- shots %>%
  mutate(
    text_color = case_when(
      league == "Bundesliga"~ "#343a40",
      TRUE ~ "grey60"
    )
  )

ggplot(shots, aes(x = npxg_per_shot, y = shots_per90, color=league)) +
  geom_vline(xintercept = 0.1053061, linetype = "dashed", color="grey80") +
  geom_hline(yintercept = 11.68469, linetype = "dashed", color="grey80") +
  geom_point(aes(),
             shape = 21, size = 2.5,
             alpha = ifelse(shots$league == "Bundesliga", 1, 0.7),
             stroke = 1.2
  ) +
  geom_text_repel(
    aes(label = team),
    min.segment.length = 0, seed = 15, size = ifelse(shots$league == "Bundesliga", 4, 3.2),
    point.padding = 0.2, box.padding = 0.3, segment.alpha = 0, color = shots$text_color
  ) +
  coord_cartesian(ylim = c(5, 19), xlim = c(0.07, 0.14), clip = "off") +
  # y arrows & labels
  geom_segment(aes(x = 0.063, xend = 0.063, y = 9.5, yend = 7.5),
               size = 0.5, alpha=0.6, arrow = arrow(length = unit(0.3, "cm")), color="#DE2141") +
  geom_segment(aes(x = 0.063, xend = 0.063, y = 13.5, yend = 15.5),
               size = 0.5, alpha=0.6, arrow = arrow(length = unit(0.3, "cm")), color="#007F5F") +
  annotate("text", 0.063, 11.68469, angle=90, label="Volume of Shots \n(Shots/90)",
           size = 3.5, color="grey30", family = "Fira Code") +
  # x arrows & labels
  geom_segment(aes(x = 0.0975, xend = 0.0875, y = 3.3, yend = 3.3),
               size = 0.5, alpha=0.6, arrow = arrow(length = unit(0.3, "cm")), color="#DE2141") +
  geom_segment(aes(x = 0.1125, xend = 0.1225, y = 3.3, yend = 3.3),
               size = 0.5, alpha=0.6, arrow = arrow(length = unit(0.3, "cm")), color="#007F5F") +
  annotate("text", 0.1053061, 3.3, label="Value of Shots\n(npxG/Shot)",
           size = 3.5, color="grey30", family = "Fira Code") +
  # title labels
  annotate("text", 0.107, 19.5,
           label = "Chance Creation in Europe's Top 5 Leagues",
           family = "IBM Plex Sans Bold", color = "black",
           hjust = 0, size = 8, lineheight = 0.8) +
  annotate("text", 0.107, 18.8,
           label = "Comparing Quantity & Quality of Shots Teams Are Creating This Season",
           family = "IBM Plex Sans", color = "black",
           hjust = 0, size = 5.5, lineheight = 1) +
  annotate("text", 0.107, 18.2,
           label = "Source: FB Ref/StatsBomb | Graphic: @paul_johnson89",
           family = "IBM Plex Sans", color = "black",
           hjust = 0, size = 3.5, lineheight = 0.5) +
  theme_minimal(base_family = "Fira Code", base_size = 14) +
  scale_color_manual(values = c("#457b9d", "#e63946", "#B34B7D", "#4D8C60", "#F7BC4D")) +
  theme(
    plot.margin = margin(0.8, 0.5, 1.5, 1.8, unit = "cm"),
    legend.title = element_blank(),
    legend.position = c(0.95, 0.14),
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
    labels = seq(0.07, 0.14, 0.01),
    breaks = seq(0.07, 0.14, 0.01),
    limits = c(0.055, 0.14)) +
  scale_y_continuous(
    oob = scales::squish,
    labels = seq(6, 18, 2),
    breaks = seq(6, 18, 2),
    limits = c(3, 19.5)) +
  # save image
  ggsave(here::here("shot_value.png"), dpi = 320, width = 16, height = 9)
