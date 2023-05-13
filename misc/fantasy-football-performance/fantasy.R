pacman::p_load(tidyverse, devtools, ggbeeswarm,
               bbplot, ggsci)

# 1. Data

fantasy_pts <- read_csv("/home/paulj1989/Documents/fantasy.csv")
fantasy19_pts <- read_csv("/home/paulj1989/Documents/fantasy19.csv")


pos_pts <- fantasy_pts %>%
  filter(VBD > 0) %>%
  select(FantPos, TD, PPR, VBD,
         PosRank, OvRank) %>%
  group_by(FantPos) %>%
  summarise(TD = mean(TD), PPR = mean(PPR),
            VBD = mean(VBD), PosRank = mean(PosRank),
            OvRank = mean(OvRank))

pos19_pts <- fantasy19_pts %>%
  filter(VBD > 0) %>%
  select(FantPos, TD, PPR, VBD,
         PosRank, OvRank) %>%
  group_by(FantPos) %>%
  summarise(TD = mean(TD), PPR = mean(PPR),
            VBD = mean(VBD), PosRank = mean(PosRank),
            OvRank = mean(OvRank))

fantasy_pts <- fantasy_pts %>%
  filter(VBD > 0)

fantasy19_pts <- fantasy19_pts %>%
  filter(VBD > 0)

yards <- fantasy_pts %>%
  select(Player, FantPos, Pass_Yds, Rush_Yds,
         Rec_Yds) %>%
  group_by(Player) %>%
  mutate("Passing" = Pass_Yds, "Rushing" = Rush_Yds, "Receiving" = Rec_Yds) %>%
  pivot_longer(Passing:Receiving, names_to = "Type", values_to = "Frequency") %>%
  filter(Frequency > 0)

yards19 <- fantasy19_pts %>%
  select(Player, FantPos, Pass_Yds, Rush_Yds,
         Rec_Yds) %>%
  group_by(Player) %>%
  mutate("Passing" = Pass_Yds, "Rushing" = Rush_Yds, "Receiving" = Rec_Yds) %>%
  pivot_longer(Passing:Receiving, names_to = "Type", values_to = "Frequency") %>%
  filter(Frequency > 0)

# 2. Visualization

## 2020

ggplot(fantasy_pts, aes(x = FantPos, y = VBD)) +
  geom_beeswarm(groupOnX = TRUE, size = 10, alpha = 0.8, shape = 21, stroke = 2, color = "#0E79B2") +
  scale_color_nejm() + bbc_style() +
  labs(title = "Comparing Fantasy Value of NFL Positions | 2021 Season",
       subtitle = "Beeswarm Plot Showing Fantasy Points Above Positional Average (VBD) Per Game")

ggplot(pos_pts, aes(x = FantPos, y = VBD)) +
  geom_col(size = 10, alpha = 0.9,fill = "#0E79B2") +
  scale_color_nejm() + bbc_style() +
  labs(title = "Comparing Fantasy Value of NFL Positions | 2020 Season",
       subtitle = "Fantasy Points Above Positional Average (VBD) Per Game")

ggplot(fantasy_pts, aes(x = FantPos, y = PPR)) +
  geom_beeswarm(groupOnX = TRUE, size = 10, alpha = 0.8, shape = 21, stroke = 2, color = "#0E79B2") +
  scale_color_nejm() + bbc_style() +
  labs(title = "Comparing Fantasy Value of NFL Positions | 2020 Season",
       subtitle = "PPR Fantasy Points Per Game")

##2019

ggplot(fantasy19_pts, aes(x = FantPos, y = VBD)) +
  geom_beeswarm(groupOnX = TRUE, size = 10, alpha = 0.8, shape = 21, stroke = 2, color = "#0E79B2") +
  scale_color_nejm() + bbc_style() +
  theme(plot.caption = element_text(size = 14, color = "#a8a8a8")) +
  labs(title = "Comparing Fantasy Value of NFL Positions | 2019 Season",
       subtitle = "Fantasy Points Above Positional Average (VBD) Per Game",
       caption = "Source: Pro Football Reference") 

ggplot(fantasy19_pts, aes(x = FantPos, y = PPR)) +
  geom_beeswarm(groupOnX = TRUE, size = 10, alpha = 0.8, shape = 21, stroke = 2, color = "#0E79B2") +
  scale_color_nejm() + bbc_style() +
  theme(plot.caption = element_text(size = 14, color = "#a8a8a8")) +
  labs(title = "Comparing Fantasy Points of NFL Positions | 2019 Season",
       subtitle = "PPR Fantasy Points Per Game",
       caption = "Source: Pro Football Reference") 

ggplot(yards19, aes(x = FantPos, y = Frequency, color = Type)) +
  geom_beeswarm(groupOnX = TRUE, size = 10, alpha = 0.8, shape = 21, stroke = 2) +
  scale_color_nejm() + bbc_style() +
  theme(plot.caption = element_text(size = 14, color = "#a8a8a8")) +
  labs(title = "Comparing Player Yardage (Distance and Type) in the NFL | 2019 Season",
       subtitle = "Total Passing, Receiving, and Rushing Yards Per Game by Position",
       caption = "Source: Pro Football Reference") 
