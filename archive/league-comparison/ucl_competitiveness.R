

ucl_compete <- read_csv("asd.csv")
ucl_compete$League <- as.factor(ucl_compete$League)


ols_compete <- lm(Pts ~ ASD, data = ucl_compete)
summary(ols_compete)

ggthemr('greyscale', layout = "scientific", text_size = 15)

ggplot(ucl_compete, aes(Pts, ASD)) +
  geom_point(aes(color = League))+
  geom_smooth(method = lm, aes(Pts, ASD))+
  labs(title = "The Relationship Between Domestic & European Competitiveness",
       subtitle = " Competitiveness = ASD (Actual Standard Deviation of Points Ratios)
      UCL Performance = Total Domestic Club Progress",
       color = NULL, y = "Competitiveness", x = "UCL Performance")+
  scale_color_lancet()+
  theme(plot.title = element_text(size=20),
        legend.position="bottom",
        plot.margin = margin(30, 30, 30, 30))

