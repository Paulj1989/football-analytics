# 1. PACKAGE MANAGEMENT ----------

pacman::p_load(tidyverse, devtools, bbplot,
               scales, ggimage, ggrepel, ggalt,
               ggtext, ggforce, glue, concaveman)



# 2. DATA----------

burki <- read_csv("Burki/buli_keepers_1819_1920.csv")
view(burki)


# 3. WRANGLING ----------

keeper_comp <- burki %>%
  select(Player, Name, Surname, Season, Squad, Born,
  Mins, GA, GA90, SoTA, SoTA90, PSxG, PsxG_90,
  PsxG_PlusMinus, PsxG_SoT, Launch_Att, Launch_Cmp,
  Launch_Cmp_Perc, Pass_Att, Pass_Launch_Perc, Pass_AvgLen,
  GK_Att, GK_Launch_Perc, GK_Avg_Len, Cross_Att, Cross_Stp,
  Cross_Stp_Perc, OPA, OPA_90, Sweep_AvgDist)

total_comp <- keeper_comp %>%
  group_by(Surname) %>%
  summarise(Mins = sum(Mins), PsxG_90 = mean(PsxG_90),
  PsxG_SoT = mean(PsxG_SoT), GA90 = mean(GA90),
  SoTA = sum(SoTA), SoTA90 = mean(SoTA90), PSxG = sum(PSxG),
  GA = sum(GA), PsxG_PlusMinus = sum(PsxG_PlusMinus),
  Cross_Stp = sum(Cross_Stp), Cross_Stp_Perc = mean(Cross_Stp_Perc)) %>%
  filter(Mins > 1530)
view(total_comp)

# 4. SHOT STOPPING ANALYSIS ----------

# 4.1 Exploratory Analysis ----------

ggplot(total_comp, aes(PsxG_90, GA90)) + geom_point(size = 5, alpha = 0.8) +
  geom_text(aes(label = Surname),
            nudge_y = 0.05,
            check_overlap = T) +
  geom_jitter(alpha = 0.8) + bbc_style() +
  theme(axis.title = element_text(size = 10)) +
  labs(title = glue("Post Shot Expected Goals Per 90 (PSxG/90)
                     vs Goals Conceded Per 90 (GA/90)"),
       subtitle = "Comparing Goalkeeper Performance | Bundesliga 18/19 - 19/20",
       x = "PSxG/90", y = "GA/90")
ggsave("img.png", width = 20, height = 16)

# 4.2 Dumbell Plots ----------

total_comp %>%
  mutate(gap = PSxG - GA) %>%
  arrange(desc(gap)) %>%
  ggplot(aes(x = PSxG, xend = GA, y = reorder(Surname, gap))) +
  geom_dumbbell(colour = "#dddddd",
                size = 6.5,
                colour_x = "#e63946",
                colour_xend = "#457b9d",
                alpha = 0.8,
                dot_guide = TRUE, dot_guide_size = 0.15) +
  bbc_style() +
  labs(title = glue("Goalkeeper Performance
                     Above/Below Expectations |
                     Bundesliga 18/19 - 19/20"),
       subtitle = glue("PSxG - Goals Conceded |
                        Ranking Keepers on Performance
                       from Better to Worse than Expected"),
       x = glue("Goals Total (<b style='color:#457B9D'>Expected</b>
                 vs <b style='color:#E63946'>Conceded</b>)"),
       y = NULL) +
  theme(axis.title.x = element_markdown(size = 18),
        axis.text.x = element_markdown(size = 18),
        panel.grid.major.x = element_line(color = "#cbcbcb"),
        panel.grid.major.y = element_blank())

ggsave("img.png", width = 20, height = 16)
# 4.3 PSxG/SoT ----------

keeper_comp %>%
  mutate(Season = as.factor(Season)) %>%
  ggplot(aes(reorder(Surname, PsxG_SoT), PsxG_SoT, fill = Season)) +
  geom_bar(stat = "identity", alpha = 0.8, position = "dodge") +
  bbc_style() + scale_fill_manual(values = c("#e63946", "#457b9d")) +
  coord_flip()

  ### PSxG +/-

total_comp %>%
  mutate(comp_color = ifelse(PsxG_PlusMinus > 0, "type1", "type2")) %>%
  ggplot(aes(x = reorder(Surname, PsxG_PlusMinus), y = PsxG_PlusMinus)) +
    geom_segment(aes(x = 0, xend = PsxG_PlusMinus,
                     y = reorder(Surname, PsxG_PlusMinus),
                     yend = reorder(Surname, PsxG_PlusMinus),
                      color = comp_color), size = 5, alpha = 0.8) +
    bbc_style() +
    theme(legend.position = "none",
          panel.grid.major.x = element_line(color = "#cbcbcb"),
                               panel.grid.major.y = element_blank()) +
  scale_color_manual(values = c("#e63946", "#457b9d")) +
  labs(title = glue("Comparing Total Post-Shot Expected Goals +/-
                     | Bundesliga 18/19 - 19/20"),
       subtitle = glue("Using PSxG +/- as a Measure of Goalkeeper
                        Performance Above/Below Expectations"))



# 4.4 GK Plots ----------

burkineuer_desc <- glue("Both goalkeepers face relatively few shots,
                         but have conceded more than they should have,
                          according to PSxG")

gulascisommer_desc <- glue("Two of the best goalkeepers
                             in Europe in the last two seasons")

ggplot(total_comp, aes(x = PsxG_90, y = SoTA90)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 4.687, linetype =  "dashed") +
  annotate("text", x = -0.3375, y = 6.75, hjust = 0, size = 6,
           label = "Busy & Underperforming") +
  annotate("text", x = -0.3375, y = 2.25, hjust = 0, size = 6,
           label = "Not Busy & Underperforming") +
  annotate("text", x = 0.1175, y = 2.25, hjust = 0, size = 6,
    label = "Not Busy & Overperforming") +
  annotate("text", x = 0.1175, y = 6.75, hjust = 0, size = 6,
    label = "Busy & Overperforming") +
  geom_point(aes(), shape = 21, size = 5,
             color = "#e63946", alpha = 0.8, stroke = 1.5) +
  geom_text_repel(data = total_comp %>%
                    filter(!Surname %in%
                    c("Bürki", "Gulácsi", "Sommer", "Neuer")),
    aes(label = Surname),
    min.segment.length = 0.3, seed = 15, size = 5,
    point.padding = 0.2, segment.alpha = 0, color = "grey20") +
  geom_mark_hull(
    aes(filter = Surname %in% c("Bürki", "Neuer"),
        label = "Bürki & Neuer",
        description = burkineuer_desc),
    expand = unit(5, "mm"), con.cap = unit(0, "mm"),
    label.width = unit(85, "mm"), label.buffer = unit(0.1, "mm"),
    label.fontsize = c(14, 12), label.colour = "grey20") +
  geom_mark_hull(
    aes(filter = Surname %in% c("Gulácsi", "Sommer"),
        label = "Gulácsi & Sommer",
        description = gulascisommer_desc),
    expand = unit(5, "mm"), con.cap = unit(0, "mm"),
    label.width = unit(80, "mm"), label.buffer = unit(16, "mm"),
    label.fontsize = c(14, 12), label.colour = "grey20") +
  labs(title = glue("Goalkeeper Shot-Stopping Performance & Workload |
                     Bundesliga 18/19 - 19/20"),
       subtitle = glue("Post-Shot Expected Goals +/- (PSxG +/-) &
                         Shots on Target Against (SoTA) | Both Measured Per 90 |
                         Minimum 1530 Minutes Played"),
       x = "PSxG +/-",
       y = "SoTA",
       caption = "Data: FBref | StatsBomb") +
  scale_x_continuous(labels = seq(-0.4, 0.3, 0.1),
                     breaks = seq(-0.4, 0.3, 0.1),
                     limits = c(-0.4, 0.3),
                     expand = c(0.01, 0)) +
    scale_y_continuous(labels = seq(2, 7, 1),
                       breaks = seq(2, 7, 1),
                       limits = c(2, 7),
                       expand = c(0.01, 0)) +
  bbc_style() +
  theme(plot.title = element_markdown(size = 20),
        plot.subtitle = element_markdown(size = 16),
        plot.caption = element_text(size = 14, color = "#a8a8a8"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))


# 5. DISTRIBUTION ANALYSIS ----------

burkineuer_desc <- glue("Both goalkeepers face relatively few shots,
                         but have conceded more than they should have,
                          according to PSxG")

gulascisommer_desc <- glue("Two of the best goalkeepers in
                            Europe in the last two seasons")

ggplot(total_comp, aes(x = PsxG_90, y = SoTA90)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
   geom_hline(yintercept = 4.687, linetype =  "dashed") +
  annotate("text", x = -0.3375, y = 6.75, hjust = 0, size = 6,
           label = "Busy & Underperforming") +
  annotate("text", x = -0.3375, y = 2.25, hjust = 0, size = 6,
           label = "Not Busy & Underperforming") +
  annotate("text", x = 0.1175, y = 2.25, hjust = 0, size = 6,
           label = "Not Busy & Overperforming") +
  annotate("text", x = 0.1175, y = 6.75, hjust = 0, size = 6,
           label = "Busy & Overperforming") +
  geom_point(aes(), shape = 21, size = 5,
             color = "#e63946", alpha = 0.8, stroke = 1.5) +
  geom_text_repel(data = total_comp %>%
                    filter(!Surname %in%
                    c("Bürki", "Gulácsi", "Sommer", "Neuer")),
                  aes(label = Surname),
                  min.segment.length = 0.3, seed = 15, size = 5,
                  point.padding = 0.2, segment.alpha = 0, color = "grey20") +

  labs(title = "Goalkeeper Shot-Stopping & Activity | Bundesliga 18/19 - 19/20",
       subtitle = glue("Post-Shot Expected Goals +/- (PSxG +/-) &
                        Shots on Target Against (SoTA) |
                        Both Measured Per 90 | Minimum 1530 Minutes Played"),
       x = "PSxG +/-",
       y = "SoTA",
       caption = "Data: FBref | StatsBomb") +
  scale_x_continuous(labels = seq(-0.4, 0.3, 0.1),
                     breaks = seq(-0.4, 0.3, 0.1),
                     limits = c(-0.4, 0.3),
                     expand = c(0.01, 0)) +
  scale_y_continuous(labels = seq(2, 7, 1),
                     breaks = seq(2, 7, 1),
                     limits = c(2, 7),
                     expand = c(0.01, 0)) +
  bbc_style() +
  theme(plot.title = element_markdown(size = 20),
        plot.subtitle = element_markdown(size = 16),
        plot.caption = element_text(size = 14, color = "#a8a8a8"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# 6. CLAIMS ANALYSIS ----------


burkineuer_desc <- glue("Both goalkeepers face relatively few shots,
                         but have conceded more than they should have,
                         according to PSxG")

gulascisommer_desc <- glue("Two of the best goalkeepers in
                            Europe in the last two seasons")

ggplot(total_comp, aes(x = Cross_Stp, y = Cross_Stp_Perc)) +
  geom_vline(xintercept = 23.18, linetype = "dashed") +
  geom_hline(yintercept = 7.27, linetype =  "dashed") +
  annotate("text", x = 30, y = 2, hjust = 0, size = 6,
           label = "Many Claims & Low Success Rate") +
  annotate("text", x = 8, y = 2, hjust = 0, size = 6,
           label = "Few Claims & Low Success Rate") +
  annotate("text", x = 8, y = 13, hjust = 0, size = 6,
           label = "Few Claims & High Success Rate") +
  annotate("text", x = 30, y = 13, hjust = 0, size = 6,
           label = "Many Claims & High Success Rate") +
  geom_point(aes(), shape = 21, size = 5,
  color = "#e63946", alpha = 0.8, stroke = 1.5) +
  geom_text_repel(data = total_comp %>%
                    filter(!Surname %in% c()),
                  aes(label = Surname),
                  min.segment.length = 0.3, seed = 15, size = 5,
                  point.padding = 0.2, segment.alpha = 0, color = "grey20") +
  labs (title = "Goalkeeper Cross Claims | Bundesliga 18/19 - 19/20",
       subtitle = glue("Total Crosses Claimed & Percentage of
                        Successful Claims | Minimum 1530 Minutes Played",
       x = "Crosses Claimed",
       y = "Claim %",
       caption = "Data: FBref | StatsBomb") +
  bbc_style() +
  theme(plot.title = element_markdown(size = 20),
        plot.subtitle = element_markdown(size = 16),
        plot.caption = element_text(size = 14, color = "#a8a8a8"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

#! geom_mark_hull(
#!    aes(filter = Surname %in% c("Bürki", "Neuer"),
#!        label = "Bürki & Neuer",
#!        description = burkineuer_desc),
#!    expand = unit(5, "mm"), con.cap = unit(0, "mm"),
#!    label.width = unit(85, 'mm'), label.buffer = unit(0.1, "mm"),
#!    label.fontsize = c(14, 12), label.colour = "grey20") +
#! geom_mark_hull(
#!      aes(filter = Surname %in% c("Gulácsi", "Sommer"),
#!          label = "Gulácsi & Sommer",
#!          description = gulascisommer_desc),
#!      expand = unit(5, "mm"), con.cap = unit(0, "mm"),
#!      label.width = unit(80, 'mm'), label.buffer = unit(16, "mm"),
#!      label.fontsize = c(14, 12), label.colour = "grey20") +

#!  scale_x_continuous(labels = seq(-0.4, 0.3, 0.1),
#!                     breaks = seq(-0.4, 0.3, 0.1),
#!                     limits = c(-0.4, 0.3),
#!                     expand = c(0.01, 0)) +
#!  scale_y_continuous(labels = seq(2, 7, 1),
#!                     breaks = seq(2, 7, 1),
#!                     limits = c(2, 7),
#!                     expand = c(0.01, 0))
