pacman::p_load(tidyverse, ggchicklet, ggtext)

comp <- read_csv(here::here("buli", "haaland_lewandowski_comp.csv"))

##%######################################################%##
#                                                          #
####              Side-By-Side Comparison               ####
#                                                          #
##%######################################################%##
# Haaland 43% and Lewa 41%

comp_gather <- comp %>%
  gather(key = "player", value = "value",
         Haaland, Lewandowski) %>%
  mutate(stat = as_factor(stat)) %>%
  mutate(stat = fct_relevel(stat, c("touches_pen", "progressive_received",
                                    "cpa_ppa", "sca", "shots", "npxg_xa",
                                    "assists", "goals")))

# create labels for y axis
labels <- c(
  "Goals",
  "Assists",
  "Non-Penalty Expected\nGoals + Expected Assists\n",
  "Shots",
  "Shot-Creating Actions",
  "Passes + Carries into\nthe Penalty Area",
  "Progressive Passes\nReceived",
  "Touches in the Opponent's\nPenalty Area"
)

ggplot() +
  geom_chicklet(data = comp_gather %>% 
                  filter(player == "Haaland"),
                aes(x = stat, y = -value, fill = player),
                alpha = 0.9) +
  geom_chicklet(data = comp_gather %>% 
                  filter(player == "Lewandowski"),
                aes(x = stat, y = value, fill = player),
                alpha = 0.9) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "grey30", size = 2) +
  labs(
    title = glue::glue("Comparing Erling Haaland & Robert Lewandowski's Performances This Season"),
    subtitle = glue::glue("Haaland & Lewandowski's /90 Statistical Contributions Across Key Offensive Metrics
                          2020/21 (Matchday 23)"),
    caption = "Data: FB Ref/StatsBomb | Graphic: @paul_johnson89") +
  annotate("text", 7, 2, 
           label = glue::glue("Both players have similar profiles,
                              though Lewandowski is a little more
                              well-rounded, and benefits from playing
                              in a more dangerous offensive machine"),
           family = "Montserrat", color = "grey20",
           hjust = 0, size = 6) +
  theme_minimal(base_family = "Fira Code", base_size = 16) +
  scale_x_discrete(labels = rev(labels)) +
  scale_y_continuous(breaks = seq(-10, 10, 5),
                     labels = abs(seq(-10, 10, 5)),
                     limits = c(-10, 10)) +
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
    panel.grid.major.x = element_line(color = "grey90", size = 0.8),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank())

ggsave(here::here("buli", "side_comp.png"),
       dpi = 320, width = 16, height = 9)

##%######################################################%##
#                                                          #
####                 Facet Comparison                   ####
#                                                          #
##%######################################################%##

comp_gather %>%
  ggplot(aes(x = stat, y = value, fill = player)) +
  geom_chicklet(alpha = 0.9) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "grey30", size = 1.2) +
  labs(
    title = glue::glue("Comparing Erling Haaland & Robert Lewandowski's Performances This Season"),
    subtitle = glue::glue("Haaland & Lewandowski's /90 Statistical Contributions Across Key Offensive Metrics
                          2020/21 (Matchday 23)"),
    caption = "Data: FB Ref/StatsBomb | Graphic: @paul_johnson89") +
  theme_minimal(base_family = "Fira Code", base_size = 16) +
  scale_x_discrete(labels = rev(labels)) +
  scale_y_continuous(breaks = seq(0, 10, 2),
                     labels = seq(0, 10, 2)) +
  scale_fill_manual(values = c("#FF0D57", "#1E88E5")) +
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
    panel.grid.major.x = element_line(color = "grey90", size = 0.8),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()) +
  facet_wrap(~player)

##%######################################################%##
#                                                          #
####               Percentage Difference                ####
#                                                          #
##%######################################################%##

# create labels for y axis
labels <- c(
  "Goals",
  "Assists",
  "Non-Penalty Expected\nGoals + Expected Assists\n",
  "% Share of Team Goals",
  "Shots",
  "Shot-Creating Actions",
  "Passes + Carries into\nthe Penalty Area",
  "Progressive Passes\nReceived",
  "Touches in the Opponent's\nPenalty Area"
)

ggplot(comp_gather, aes(x = stat, y = value, fill = player)) +
  geom_chicklet(alpha = 0.8, position = "dodge") +
  coord_flip() +
  # annotate figure to add title, subtitle and caption
  annotate("text", 8.2, 2,
           label = "Erling Haaland vs Robert Lewandowski",
           family = "Montserrat", color = "grey10",
           hjust = 0, size = 10, lineheight = 0.5) +
  annotate("text", 7.6, 2,
           label = glue::glue("Comparing Haaland & Lewandowski's /90 Stats This Season
                              2020/21 (Matchday 23)"),
           family = "Montserrat", color = "grey30",
           hjust = 0, size = 6, lineheight = 1) +
  annotate("text", 7.1, 2,
           label = "Source: FB Ref/StatsBomb | Graphic: @paul_johnson89",
           family = "Montserrat", color = "grey30",
           hjust = 0, size = 4.5, lineheight = 0.5) +
  # draw thicker line at 0 to represent starting/pivot point
  geom_hline(yintercept = 0, color = "grey20", size = 1.5) +
  # set theme and customize to fit our requirements here
  theme_minimal(base_family = "Fira Code", base_size = 14) +
  # set scales for x and y
  scale_y_continuous(
    labels = seq(0, 10, 1),
    breaks = seq(0, 10, 1),
    limits = c(0, 10)) +
  scale_x_discrete(labels = rev(labels)) +
  scale_fill_manual(values = c("#FF0D57", "#1E88E5")) +
  theme(
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size = unit(10, "mm"),
    legend.text = element_text(size = 16, family = "Montserrat",
                               color = "grey20"),
    legend.spacing = unit(1, "mm"),
    legend.background = element_blank(),
    legend.margin = margin(0, 0.2, 0.2, 0.2, unit = "cm"),
    axis.text = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "#F3F5F6", size = 0.4),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank())

# save image
ggsave(here::here("buli", "comp.png"), dpi = 320, width = 16, height = 9)  
