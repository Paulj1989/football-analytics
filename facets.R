facet_pts <- ggplot(big3, aes(pts, fill = year))+
  geom_density(alpha=0.5)+
  theme_ipsum(plot_title_face = "plain", 
              plot_title_size = 20, 
              base_size = 18, 
              axis_title_size = 20,
              strip_text_size = 20)+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.box.spacing = unit(0.005, "cm"),
        legend.key.size = unit(0.7, "cm"))+
  xlim(16,100)+
  coord_cartesian(xlim = c(19,99.5))+
  labs(title = NULL, y = NULL, x = "Points")+
  scale_fill_viridis_d(labels = c("14/15", "15/16", "16/17", "17/18", "18/19")) + 
  facet_wrap(~country, nrow = 3)
facet_pts

summary(big3$pts)

facet_value <- ggplot(big3, aes(value, fill = year))+
  geom_density(alpha=0.5)+
  theme_ipsum(plot_title_face = "plain", plot_title_size = 20, base_size = 18, axis_title_size = 20)+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.box.spacing = unit(0.005, "cm"),
        legend.key.size = unit(0.7, "cm"))+
  labs(title = "", y = "", x = "Market Value (€m)")+
  scale_fill_viridis_d(labels = c("14/15", "15/16", "16/17", "17/18", "18/19"))+ 
  facet_wrap(~country, nrow = 3)
facet_value

facet_spend <- ggplot(big3, aes(spend, fill = year))+
  geom_density(alpha=0.5)+
  theme_ipsum(plot_title_face = "plain", plot_title_size = 20, base_size = 18, axis_title_size = 20)+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.box.spacing = unit(0.005, "cm"),
        legend.key.size = unit(0.7, "cm"))+
  labs(title = "", y = "", x = "Transfer Spending (€m)")+
  scale_fill_viridis_d(labels = c("14/15", "15/16", "16/17", "17/18", "18/19"))+ facet_wrap(~country)
facet_spend
