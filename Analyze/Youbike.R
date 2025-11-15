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
