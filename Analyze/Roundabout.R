# 圓環分析

# 公館圓環事故
roundabouts <- tribble(
  ~name, ~lat, ~lon,
  "台北市公館圓環", 25.011225446326605, 121.53712748254088,
  "台北市建成圓環", 25.053836355512296, 121.51470061353103,
  "台北市仁愛圓環", 25.03785094884258, 121.54884923186027,
  "台北市景福門圓環", 25.039024861088162, 121.51764257316145,
  "台北市六張犁圓環", 25.024360242208918, 121.55254916164026,
  "台南市後甲圓環", 22.98840360799005,  120.23388549774656,
  "台南市湯德章紀念公園圓環", 22.992723473467116, 120.20550269537732,
  "台南市東門圓環（台南）", 22.988786000438793,  120.21131391309147,
  "台南市東門城圓環", 22.987032343973418, 120.21761113102006,
  "台南車站圓環", 22.99731225497853, 120.21198469516527,
  "新竹之心圓環", 24.80421481067459, 120.9704530295758,
  "桃園市平鎮圓環", 24.92561494924363, 121.24416617971389,
  "嘉義市中央噴水圓環", 23.479986129920146,  120.44962028826049
)
roundabouts_sf <- st_as_sf(
  roundabouts,
  coords = c("lon", "lat"),
  crs = 4326
)

pt_3826 <- st_transform(roundabouts_sf, 3826)
acc_buf_3826 <- st_transform(acc_buf, 3826)
acc_cent_3826 <- st_centroid(acc_buf_3826)

pt_buf100 <- st_buffer(pt_3826, dist = 200)

write_sf(
  pt_3826,
  "./CalculatedData/roundabouts_sf.shp",
  layer_options = "ENCODING=UTF-8"
)

write_sf(
  pt_buf100,
  "./CalculatedData/roundabouts_buf200m.shp",
  layer_options = "ENCODING=UTF-8"
)

inter_idx <- st_intersects(acc_cent_3826, pt_buf100, sparse = FALSE)
rows_any <- apply(inter_idx, 1, any)
acc_in_100m_all <- acc_buf_3826[rows_any, ]

# save acc_in_100m_all as csv
write_csv(
  acc_in_100m_all,
  "./CalculatedData/accidents_near_roundabouts_200m.csv"
)

acc_specific <- acc_buf_3826[inter_idx[, 1], ]
acc_specific$COUNTYNAME%>%unique()

acc_in_100m_all <- st_join(
  acc_in_100m_all,
  pt_buf100
)%>%
  filter(!is.na(name))

# 和原始的圓環事故作比較
roundabouts <- acc_buf%>%filter(道路型態子類別名稱 == '圓環') # 舊版

acc_in_100m_all%>%
  st_drop_geometry()%>%
  group_by(spd_group)%>%
  summarize(n = n())%>%
  mutate(prop= n / sum(n))%>%
  ggplot()+
  geom_bar(
    aes(x = spd_group, y = prop, fill = spd_group),
    stat = 'identity'
  )+
  coord_flip()

county_signal <- acc_in_100m_all%>%
  st_drop_geometry()%>%
  group_by(COUNTYNAME, `號誌-號誌種類名稱`)%>%
  summarize(n = n())

county_signal%>%
  ggplot()+
  geom_bar(
    aes(x = COUNTYNAME, y = n, fill = `號誌-號誌種類名稱`),
    stat = 'identity',
    position = 'dodge'
  )+
  coord_flip()

sd_signal <- acc_in_100m_all%>%
  st_drop_geometry()%>%
  group_by(spd_group, `號誌-號誌種類名稱`)%>%
  summarize(n = n())

sd_signal%>%
  ggplot()+
  geom_bar(
    aes(x = spd_group, y = n, fill = `號誌-號誌種類名稱`),
    stat = 'identity',
    position = 'dodge'
  )+
  coord_flip()

# 圓環分析
acc_in_100m_all%>%
  group_by(name, spd_group)%>%
  summarize(n = n())%>%
  ggplot()+
  geom_bar(
    aes(x = name, y = n, fill = spd_group),
    stat = 'identity',
    position = 'dodge'
  )+
  coord_flip()

acc_in_100m_all%>%
  # filter(事故類型及型態子類別名稱 %in% c('追撞', '路口交岔撞', '側撞', '同向擦撞', '對向擦撞', '對撞'))%>%
  group_by(name, 事故類型及型態子類別名稱)%>%
  summarize(n = n())%>%
  group_by(name) %>%
  mutate(normalized = n / sum(n))%>%
  ggplot()+
  geom_bar(
    aes(x = name, y = normalized, fill = 事故類型及型態子類別名稱),
    stat = 'identity',
    position = 'dodge'
  )+
  coord_flip()


lst <- c("當事者區分-類別-大類別名稱-車種", "道路型態子類別名稱", "號誌-號誌種類名稱")

for (i in lst) {
  plt <- acc_in_100m_all%>%
    ggplot()+
    geom_bar(
      aes(x = name, fill = !!sym(i)),
      position = 'dodge'
    )+
    coord_flip()

  print(plt)
}
