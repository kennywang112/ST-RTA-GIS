youbike_spd <- acc_buf%>%
  st_drop_geometry()%>%
  group_by(youbike_100m_count, spd_group)%>%
  summarize(n = n())%>%
  group_by(youbike_100m_count)%>%
  mutate(normalized = n / sum(n))

youbike_spd%>%
  ggplot()+
  geom_bar(
    aes(x = youbike_100m_count, y = normalized, fill = spd_group),
    stat = 'identity',
    position = 'dodge'
  )+
  geom_text(
    aes(x = youbike_100m_count, y = normalized, label = n, group = spd_group),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    hjust = -0.3,
    size = 3
  ) +
  coord_flip()

# youbike 和自行車分析
tbl <- table(acc_buf$`當事者區分-類別-子類別名稱-車種`, acc_buf$youbike_100m_count)
res <- chisq.test(tbl)$residuals

library(reshape2)

melt(res) %>%
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2() +
  labs(x="Accident Type", y="Near YouBike (0/1)", fill="Residual")+
  coord_flip()
