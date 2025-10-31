install.packages(c("showtext","sysfonts"))
library(showtext)
library(sysfonts)
font_add_google("Noto Sans TC", family = "noto")
showtext_auto()

pairs_annot <- st_read(dsn="./CalculatedData/pairs_annot.shp", layer="pairs_annot")
pairs_annot

specific_pairs_annot <- pairs_annot
specific_pairs_annot_3826  <- st_transform(specific_pairs_annot, 3826)

specific_combined_data <- combined_data_in_taiwan %>% filter(COUNTYNAME == "臺北市")
specific_combined_data_sf <- st_as_sf(specific_combined_data, coords = c("經度", "緯度"), crs = 4326)
specific_combined_data_3826 <- st_transform(specific_combined_data_sf, 3826)
specific_combined_data_3826%>%select(geometry)


buf_dist <- 100

acc_buf <- st_buffer(specific_combined_data_3826, dist = buf_dist) %>%
  st_make_valid() %>%
  mutate(buf_id = row_number())

hits <- st_intersects(acc_buf, specific_pairs_annot_3826)

acc_buf$spec_count <- lengths(hits)
acc_buf$has_spd <- acc_buf$spec_count > 0

# 每個 buffer 的最大 spd_dlt
acc_buf$max_spd_dlt <- sapply(hits, function(ix) {
  if(length(ix) == 0) return(NA_real_)
  vals <- specific_pairs_annot_3826$spd_dlt[ix]
  max(vals, na.rm = TRUE)
})
acc_buf$max_spd_dlt <- ifelse(is.na(acc_buf$max_spd_dlt), 0, acc_buf$max_spd_dlt)
acc_buf$spd_group <- case_when(
  acc_buf$max_spd_dlt >= 50 ~ "High Speed Diff",
  acc_buf$max_spd_dlt > 0 & acc_buf$max_spd_dlt < 50 ~ "Low Speed Diff",
  TRUE ~ "No Speed Diff"
)
acc_buf$spd_group%>%table()

# plot acc_buf$max_spd_dlt
acc_buf%>%
  ggplot() +
  geom_bar(aes(x = max_spd_dlt), fill = "steelblue", color = "black")

# 有無設施下、有高素差和無高素差有沒有不同型態的道路事故
# 以素差為主軸去分割
acc_buf%>%colnames()

plot_func <- function(data, main_col, target) {
  main_col_q <- enquo(main_col)
  target_q   <- enquo(target)

  ratio_table <- data %>%
    count(!!main_col_q, !!target_q) %>%
    group_by(!!main_col_q) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()

  ggplot(ratio_table, aes(x = !!target_q, y = prop, fill = as.factor(!!main_col_q))) +
    geom_col(position = position_dodge(width = 0.9)) +
    labs(x = as_label(target_q), fill = as_label(main_col_q)) +
    theme_minimal()
}

lst <- c('道路類別-第1當事者-名稱', '道路型態大類別名稱', '道路型態子類別名稱', '事故位置大類別名稱', '事故位置子類別名稱', '號誌-號誌種類名稱',
  '號誌-號誌動作名稱', '車道劃分設施-分向設施大類別名稱', '車道劃分設施-分向設施子類別名稱', '車道劃分設施-分道設施-快車道或一般車道間名稱',
  '車道劃分設施-分道設施-快慢車道間名稱', '車道劃分設施-分道設施-路面邊線名稱', '事故類型及型態大類別名稱', '事故類型及型態子類別名稱', '肇因研判大類別名稱-主要',
  '當事者區分-類別-大類別名稱-車種', '當事者行動狀態大類別名稱', '車輛撞擊部位大類別名稱-最初', '肇因研判大類別名稱-個別')

plot_func(acc_buf, spd_group, `號誌-號誌種類名稱`)
plot_func(acc_buf, has_spd, `號誌-號誌種類名稱`)

for (i in lst) {
  col_sym <- sym(i)
  p <- plot_func(acc_buf, spd_group, !!col_sym)
  print(p)
  ggsave(paste0("Layouts/speed_diff_", i, ".png"), plot = p, width = 8, height = 6, dpi = 300)
}





