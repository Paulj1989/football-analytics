pacman::p_load(tidyverse, here, ggrepel, ggtext)

players <-readr::read_csv("player_stats.csv")


players <- players %>%
  mutate(total_progression = progressive_passes+progressive_carries,
         mins = nineties*90) %>%
  filter(mins>800) %>%
  drop_na()
  
df <- players %>%
  filter(total_progression > mean(total_progression) |
           sca > mean(sca))


ggplot(players, aes(x = total_progression, y = sca, color = league)) +
  geom_vline(xintercept = quantile(players$total_progression, .9), linetype = "dashed", color="grey80") +
  geom_hline(yintercept = quantile(players$sca, .9), linetype = "dashed", color="grey80") +
  geom_point(data = players %>%
               filter(total_progression > quantile(players$total_progression, .9) | sca > quantile(players$sca, .9)),
             aes(),
             shape = 21, size = 2,
             stroke = 1.5
  ) +
  geom_point(data = players %>%
               filter(total_progression < quantile(players$total_progression, .9) | sca < quantile(players$sca, .9)),
             aes(),
             shape = 21, size = 1.5,
             alpha = 0.1,
             color = "grey30",
             stroke = 1
  ) +
  geom_text_repel(
    data = players %>%
      filter(total_progression > quantile(players$total_progression, .9) | sca > quantile(players$sca, .9)),
    aes(label = last_name),
    seed = 15, size = 3,
    segment.alpha = 0, color = "grey30"
  ) +
  coord_cartesian(ylim = c(0.5, 7.5), xlim = c(4.5, 24), clip = "off") +
  # y arrows & labels
  geom_segment(aes(x = 2, xend = 2, y = 2.5, yend = 1.5),
               size = 0.5, alpha=0.6, arrow = arrow(length = unit(0.3, "cm")), color="#DE2141") +
  geom_segment(aes(x = 2, xend = 2, y = 4.5, yend = 5.5),
               size = 0.5, alpha=0.6, arrow = arrow(length = unit(0.3, "cm")), color="#007F5F") +
  annotate("text", 2, 3.5, angle=90, label="Chance Creation \n(SCA/90)",
           size = 3.5, color="grey30", family = "Fira Code") +
  # x arrows & labels
  geom_segment(aes(x = 10, xend = 5, y = 0, yend = 0),
               size = 0.5, alpha=0.6, arrow = arrow(length = unit(0.3, "cm")), color="#DE2141") +
  geom_segment(aes(x = 15, xend = 20, y = 0, yend = 0),
               size = 0.5, alpha=0.6, arrow = arrow(length = unit(0.3, "cm")), color="#007F5F") +
  annotate("text", 12.5, -0.8, label="Ball Progression\n(Progressive Carries + Passes /90)",
           size = 3.5, color="grey30", family = "Fira Code") +
  # title labels
  annotate("text", -0.2, 7.2,
           label = "Creativity & Ball Progression in Europe's Top 5 Leagues",
           family = "IBM Plex Sans Bold", color = "black",
           hjust = 0, size = 8.5, lineheight = 0.8) +
  annotate("text", -0.2, 6.8,
           label = "Players in the 90th Percentile for Progression (Progressive Passes & Carries) &\nChance Creation (Shot-Creating Actions) This Season (Minimum 800 Mins)",
           family = "IBM Plex Sans", color = "black",
           hjust = 0, size = 5, lineheight = 1) +
  annotate("text", -0.2, 6.5,
           label = "Source: FB Ref/StatsBomb | Graphic: @paul_johnson89",
           family = "IBM Plex Sans", color = "black",
           hjust = -0, size = 3.5, lineheight = 0.5) +
  theme_minimal(base_family = "Fira Code", base_size = 14) +
  scale_color_manual(values = c("#457b9d", "#e63946", "#B34B7D", "#4D8C60", "#F7BC4D")) +
  theme(
    plot.margin = margin(0.5, 0.2, 1.5, 1.5, unit = "cm"),
    legend.title = element_blank(),
    legend.position = c(0.95, 0.15),
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
    labels = seq(4.5, 25, 5),
    breaks = seq(4.5, 25, 5),
    limits = c(2, 25)) +
  scale_y_continuous(
    oob = scales::squish,
    labels = seq(1, 7.5, 1),
    breaks = seq(1, 7.5, 1),
    limits = c(0, 7.5))


# save image
ggsave(here::here("guerreiro.png"), dpi = 320, width = 19.2, height = 10.8)

  
  
  
