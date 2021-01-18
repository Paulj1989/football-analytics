pacman::p_load(tidyverse, here)

goals <- readr::read_csv('most_contributions.csv')

goals <- goals %>%
mutate(
  line_colour = case_when(
    season == "20/21" & player == "Erling Haaland" ~ "#e63946",
    TRUE ~ "#343a40"
  )
)

# goal_totals <- involvement %>%
#  group_by(season) %>%
#  summarize(sum_goals = sum(goals)) %>%
#  right_join(involvement, by="season") %>%
#  mutate(contribution = round((goals/sum_goals)*100, digits=1)) 

# goals <- goal_totals %>% 
#  filter(mins > 500) %>%
#  group_by(season) %>%
#  top_n(4, contribution) %>% 
#  arrange(contribution) %>%
#  rowwise() %>%
#  mutate(x = list(c(-10, 0))) %>%
#  mutate(y = list(c(n*4 - 1.4, n*2 - 0.7, n*2 + 0.7, n*4 + 1.4))) %>% 
#  unnest(cols = c(n, x, y))


ggplot(goals) +
  geom_rect(aes(
    xmin = -10, ymin = n*4 - 1.4,
    xmax = -32, ymax = n*4 + 1.4),
    fill = goals$line_colour, color = NA) +
  geom_polygon(aes(x, y, group = n), fill = goals$line_colour, color = NA) +
  geom_rect(aes(xmin = 0, ymin = n*2 - 0.7,
                xmax = contribution*100, ymax = n*2 + 0.7), fill = goals$line_colour, color = NA) +
  geom_text(aes(-31, n*4, label = paste0(player, " (", season, ")")), family = "Fira Code", color = "white", hjust = 0, size = 4, check_overlap = TRUE) +
  geom_text(aes(contribution*100 - 2, n*2, label = scales::percent(contribution)), family = "Fira Code", color = "white", hjust = 1, size = 4, check_overlap = TRUE) +
  annotate("text", 48, 17, label = "Haaland's Oversized Share of the Offensive Workload", family = "IBM Plex Sans", color = "black", hjust = 1, size = 7, lineheight = 0.9) +
  annotate("text", 48, 15, label = "Goals Scored as a % of Total Team Goals (Per Season) Since 2018", family = "IBM Plex Sans", color = "black", hjust = 1, size = 5, lineheight = 0.9) +
  annotate("text", 1, -9.5, label = "Source: FB Ref/StatsBomb", family = "IBM Plex Sans", color = "black", hjust = 0, size = 2.5, lineheight = 0.9) +
  scale_x_continuous(breaks = seq(0, 50, 10), labels = seq(0, 50, 10)) +
  theme_minimal(base_family = "Fira Code") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = "Fira Code", size = 12),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", size = 0.2),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) + ggsave(here::here("haaland_workload.png"), dpi = 320, width = 12, height = 8)
