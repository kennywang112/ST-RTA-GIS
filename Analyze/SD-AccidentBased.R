install.packages(c("showtext","sysfonts"))
library(showtext)
library(sysfonts)
font_add_google("Noto Sans TC", family = "noto")
showtext_auto()

library(sf)
library(tidyverse)
crs <- 3826
crs_init <- 4326
taiwan <- st_read("Data/村(里)界(TWD97經緯度)/VILLAGE_NLSC_1140825.shp")
taipei <- st_read(dsn="~/Desktop/RTA-GIS/Data/縣市界線(TWD97經緯度)/COUNTY_MOI_1090820.shp", layer="COUNTY_MOI_1090820")
pairs_annot <- st_read(dsn="./CalculatedData/pairs_annot_all_cities.shp", layer="pairs_annot_all_cities")%>%st_transform(crs)

pairs_annot <- st_join(
  pairs_annot,
  taiwan%>%
    select(COUNTYNAME, TOWNNAME, VILLNAME)%>%
    st_transform(crs))


library(tmap)
tmap_mode("view")
tm_shape(pairs_annot) +
  tm_dots(size = 0.5, col = "spd_dlt", palette = "Reds", style = "quantile")

filter_lst <- c("新北市", "基隆市", "桃園市", "新竹市", "新竹縣",
            "苗栗縣", "臺中市", "嘉義市", "嘉義縣",
            "臺南市", "高雄市", "屏東縣", "臺東縣")

specific_pairs_annot <- pairs_annot%>%
  # filter(COUNTYNAME %in% filter_lst)%>%
  st_transform(crs)


final_data <- combined_data_in_taiwan%>%
  # filter(COUNTYNAME %in% filter_lst)%>%
  st_as_sf(coords = c("經度", "緯度"), crs = crs_init)%>%
  st_transform(crs)

final_data$youbike_100m_count <- case_when(
  final_data$youbike_100m_count > 2 ~ 2,
  TRUE ~ final_data$youbike_100m_count
)

buf_dist <- 100
acc_buf <- st_buffer(final_data, dist = buf_dist) %>%
  st_make_valid() %>%
  mutate(buf_id = row_number())

hits <- st_intersects(acc_buf, specific_pairs_annot)
acc_buf$spec_count <- lengths(hits)

# 每個 buffer 的最大 spd_dlt
acc_buf$max_spd_dlt <- sapply(hits, function(ix) {
  if(length(ix) == 0) return(NA_real_)
  vals <- specific_pairs_annot$spd_dlt[ix]
  max(vals, na.rm = TRUE)
})
acc_buf$max_spd_dlt <- ifelse(is.na(acc_buf$max_spd_dlt), 0, acc_buf$max_spd_dlt)

# 確認速差區分
qs <- acc_buf %>%
  filter(max_spd_dlt > 0) %>%
  pull(max_spd_dlt) %>%
  quantile(probs = c(0.33, 0.66), na.rm = TRUE)
qs

# acc_buf$spd_group <- case_when(
#   acc_buf$max_spd_dlt > 50 ~ "High Speed Diff",
#   acc_buf$max_spd_dlt == 50 ~ "Medium Speed Diff",
#   acc_buf$max_spd_dlt > 0 & acc_buf$max_spd_dlt < 50 ~ "Low Speed Diff",
#   TRUE ~ "No Speed Diff"
# )
# acc_buf$spd_group <- factor(
#   acc_buf$spd_group,
#   levels = c("No Speed Diff", "Low Speed Diff", "Medium Speed Diff", "High Speed Diff")
# )

acc_buf$spd_group <- case_when(
  acc_buf$max_spd_dlt > 0 ~ "Speed Diff",
  TRUE ~ "No Speed Diff"
)

# acc_buf$spd_group <- cut(
#   acc_buf$max_spd_dlt,
#   breaks = 10,
#   include.lowest = TRUE
# )
# acc_buf$spd_group <- cut(
#   acc_buf$max_spd_dlt,
#   # breaks = c(-1, 0, 20, 40, 60, 80, 100, 110),
#   breaks = c(-1, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110),
#   include.lowest = TRUE,
# )

acc_buf%>%
  filter(max_spd_dlt > 0)%>%
  ggplot() +
  geom_bar(aes(x = max_spd_dlt), fill = "steelblue", color = "black")

# 有無設施下、有高素差和無高素差有沒有不同型態的道路事故
# 以速限差為主軸去分割
acc_buf%>%colnames()

plot_func <- function(data, main_col, target, type='origin') {
  main_col_q <- enquo(main_col)
  target_q  <- enquo(target)

  if (type == 'origin') {

    ratio_table <- data %>%
      st_drop_geometry() %>%
      count(!!main_col_q, !!target_q) %>%
      group_by(!!main_col_q) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup()

    plt <- ggplot(ratio_table, aes(x = !!target_q, y = prop, fill = !!main_col_q)) +
            geom_col(position = position_dodge(width = 0.9)) +
            labs(x = as_label(target_q), fill = as_label(main_col_q)) +
            # theme_minimal()
            geom_text(aes(label = n), position = position_dodge(width = 0.9), hjust = -0.1) +
            coord_flip()
  }
  else if (type == 'percent') {

    ratio_table <- data %>%
      st_drop_geometry() %>%
      add_count(!!target_q, name = "total_n") %>%
      mutate(new_label = paste0(!!target_q, "(N=", total_n, ")"))%>%
      count(!!target_q, !!main_col_q, total_n, new_label) %>%
      group_by(!!target_q) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup()%>%
      filter(!is.na(!!target_q))

    plt <- ggplot(ratio_table, aes(x = reorder(new_label, ifelse(!!main_col_q == "Speed Diff", prop, 0)), y = prop, fill = !!main_col_q)) +
            geom_col() +
            coord_flip() +
            scale_y_continuous(labels = function(x) scales::percent(abs(x))) +
            scale_fill_manual(values = c("No Speed Diff" = "#4682B4", "Speed Diff" = "#B22222"))+
            labs(x = as_label(target_q), fill = as_label(main_col_q))+
            theme_minimal(base_size = 15)
  }
  return(plt)
}

lst <- c('道路類別-第1當事者-名稱', '道路型態大類別名稱', '道路型態子類別名稱', '事故位置大類別名稱', '事故位置子類別名稱', '號誌-號誌種類名稱',
  '號誌-號誌動作名稱', '車道劃分設施-分向設施大類別名稱', '車道劃分設施-分向設施子類別名稱', '車道劃分設施-分道設施-快車道或一般車道間名稱',
  '車道劃分設施-分道設施-快慢車道間名稱', '車道劃分設施-分道設施-路面邊線名稱', '事故類型及型態大類別名稱', '事故類型及型態子類別名稱', '肇因研判大類別名稱-主要',
  '當事者區分-類別-大類別名稱-車種', '當事者行動狀態大類別名稱', '車輛撞擊部位大類別名稱-最初', '肇因研判大類別名稱-個別', '當事者區分-類別-子類別名稱-車種',
  '道路型態子類別名稱', 'youbike_100m_count')


for (i in lst) {
  col_sym <- sym(i)
  p <- plot_func(acc_buf, spd_group, !!col_sym, type='percent')
  ggsave(paste0("Layouts/speed_diff_", i, ".png"), plot = p, width = 8, height = 6, dpi = 300)
}
plot_func(acc_buf, spd_group, `車道劃分設施-分向設施子類別名稱`, type='percent')

cause_lookup_df <- stack(cause_mapping_list) %>%
  rename(cause_detail = values, cause_category = ind)
df_final <- acc_buf%>%
  st_drop_geometry()%>%
  left_join(cause_lookup_df, by = c(`肇因研判子類別名稱-主要` = "cause_detail"))
plot_func(df_final, spd_group, cause_category, type='percent')
# plot_func(acc_buf, spd_group, `肇因研判子類別名稱-主要`, type='percent')

## Roundabout and Youbike analysis
rt%>%
  filter(道路型態子類別名稱=='圓環')%>%
  ggplot()+
  geom_col(aes(x=spd_group, y=prop, fill=spd_group),
           position = position_dodge(width = 0.9))+
  geom_text(aes(x=spd_group, y=prop, label=n), position = position_dodge(width = 0.9), hjust = -0.1) +
  coord_flip()

rt%>%colnames()
rt$youbike_100m_count <- case_when(
  rt$youbike_100m_count == 0 ~ "No Station",
  rt$youbike_100m_count == 1 ~ "1 Station",
  rt$youbike_100m_count == 2 ~ "2+ Station"
)
rt%>%
  ggplot(aes(x = youbike_100m_count, y = prop, fill = spd_group)) +
  geom_col(position = position_dodge(width = 0.9)) +
  labs(x = "youbike_100m_count", fill = "spd_group") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), hjust = -0.1)+
  coord_flip()

acc_buf$`肇因研判子類別名稱-主要`

# for poster
rear_end <- acc_buf%>%
  st_drop_geometry()%>%
  # filter(事故類型及型態子類別名稱 == '追撞') %>%
  count(spd_group, 事故類型及型態子類別名稱) %>%
  group_by(spd_group) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()%>%
  filter(事故類型及型態子類別名稱 == '追撞')%>%
  rename(type = 事故類型及型態子類別名稱)


intersection <- acc_buf%>%
  st_drop_geometry()%>%
  # filter(事故類型及型態子類別名稱 == '追撞') %>%
  count(spd_group, 道路型態子類別名稱) %>%
  group_by(spd_group) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()%>%
  filter(道路型態子類別名稱 %in% c('多岔路', '四岔路'))%>%
  rename(type = 道路型態子類別名稱)

full_type <- rbind(rear_end, intersection)

full_type <- full_type %>%
  mutate(type = case_when(
    type == "追撞" ~ "Rear-end",
    type == "四岔路" ~ "4-way Intersection",
    type == "多岔路" ~ "Multi-way Intersection",
    TRUE ~ type
  ))

full_type <- full_type %>%
  mutate(type = factor(type, levels = c("Multi-way Intersection", "Rear-end", "4-way Intersection")))

ggplot(full_type, aes(x = spd_group, y = prop, fill = type)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Speed Difference Group",
    y = "Proportion",
    fill = "Accident/Road Type"
  ) +
  theme_minimal(base_size = 18) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 6)
