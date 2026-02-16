library(stringr)
library(purrr)
source("./Analyze/ReadData.R")

# taipei_roads
roadsf <- roads#%>%filter(maxspeed > 0)

roads_lines <- roadsf %>%
  st_collection_extract("LINESTRING") %>%  # 若有 GEOMETRYCOLLECTION，先抽出線
  st_cast("LINESTRING") %>%  # 拆 MULTILINESTRING 成多條 LINESTRING
  filter(!st_is_empty(geometry)) %>%
  mutate(road_id = row_number())

roadsf%>%dim()
roads_lines%>%dim()

# 取端點
roads_lines <- st_transform(roads_lines, 3826)
pts_multi <- st_line_sample(st_geometry(roads_lines), sample = c(0, 1))
pts <- st_cast(pts_multi, "POINT")

# 依每條線實際產生的點數展開屬性索引
# seq_len(nrow(roads_lines)): 產生一個向量：1, 2, 3, …, N, 每個數字代表一條路段的索引
# lengths(pts_multi): 算出每個 MULTIPOINT 物件裡有幾個點（通常是 2，偶爾 1 或 0）。
# rep(..., times = ...), 例如：
# 第 1 條路段有 2 個點 → 重複「1」兩次 → 1, 1
# 第 2 條路段只有 1 個點 → 重複「2」一次 → 2
# 第 3 條路段有 2 個點 → 重複「3」兩次 → 3, 3
# idx <- rep(seq_len(nrow(roads_lines)), times = lengths(pts_multi))
idx <- rep(seq_len(nrow(roads_lines)), each = 2)
# 組合成新的 sf 物件
endpoints <- st_sf(
  road_id = roads_lines$road_id[idx],
  maxspeed_num = roads_lines$maxspeed[idx],
  geometry = pts
)

# 四捨五入1公尺對齊
coords <- st_coordinates(endpoints)
endpoints <- endpoints %>%
  mutate(
    x_round = round(coords[,1], 0),
    y_round = round(coords[,2], 0),
    key = paste0(x_round, ",", y_round)
  )

# 匯總端點 - 兩兩配對 - 計算速限差
by_junction <- endpoints %>%
  group_by(key) %>%
  summarise(
    roadsf = list(road_id),
    speeds = list(maxspeed_num),
    geometry = first(geometry),
    .groups = "drop"
  ) %>%
  mutate(n_roads = map_int(roadsf, length)) %>%
  filter(n_roads >= 2)

speed_diff_min <- 10  # distance threshold
pairs_df <- by_junction %>%
  mutate(combo = map2(roadsf, speeds, ~{
    r <- .x; s <- .y
    if (length(r) < 2) return(tibble())
    idx <- t(combn(seq_along(r), 2))
    tibble(
      road_id_a = r[idx[,1]],
      road_id_b = r[idx[,2]],
      speed_a = s[idx[,1]],
      speed_b = s[idx[,2]]
    )
  }))
pairs_df_filter <- pairs_df%>%
  dplyr::select(geometry, combo) %>%
  unnest(combo) %>%
  filter(!is.na(speed_a), !is.na(speed_b)) %>%
  mutate(speed_delta = abs(speed_a - speed_b)) %>%
  filter(speed_delta >= speed_diff_min) %>%
  st_as_sf()

# 接回屬性（路名、等級）
pairs_annot <- pairs_df_filter %>%
  dplyr::left_join(st_drop_geometry(roads_lines), #%>% dplyr::select(road_id, name_a = name, fclass_a = fclass),
            by = c("road_id_a" = "road_id"))

pairs_annot$tunnel%>%unique()
pairs_annot$layer%>%unique()
pairs_annot$fclass%>%unique()
pairs_annot$oneway%>%unique()

write_sf(
  pairs_annot,
  "./CalculatedData/pairs_annot_all_cities.shp",
  layer_options = "ENCODING=UTF-8"
)

