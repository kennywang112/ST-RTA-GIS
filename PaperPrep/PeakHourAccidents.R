library(tidyverse)
library(sf)
library(lubridate)

roads <- st_read(dsn="./Data/road/gis_osm_roads_free_1.shp", layer="gis_osm_roads_free_1")
final_boundary_data <- read_csv("../ST-RTA/ComputedDataV7/Data/Final_boundary_data.csv")
final_boundary_data

final_boundary_data <- final_boundary_data %>%
  mutate(
    hour = floor(發生時間 / 10000),
    date_parsed = ymd(發生日期),
  )

final_boundary_data

final_boundary_data %>%
  count(hour) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 2.5, color = "darkblue") +
  geom_hline(yintercept = 35000, linetype = "dashed", color = "red", size = 1) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Hourly Distribution of Traffic Accidents",
    x = "Time of Day (Hour)",
    y = "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16)
  )

youbike_sf <- read_csv('../ST-RTA/ComputedData/Youbike/full_youbike.csv')%>%
  st_as_sf(coords = c("PositionLon", "PositionLat"), crs = 4326)%>%
  st_transform(3826)

facility_list <- list("Youbike" = youbike_sf)

library(future.apply)
plan(multisession, workers = 3)
message(paste("Cell:", availableCores() - 1))

combined_sf <- final_boundary_data %>%
  mutate(hour_factor = factor(sprintf("%02d:00", hour), levels = sprintf("%02d:00", 0:23))) %>%
  filter(!is.na(hour_factor)) %>%
  st_as_sf(coords = c("經度", "緯度"), crs = 4326) %>%
  st_transform(3826)

idx_yb <- st_nearest_feature(combined_sf, youbike_sf)
combined_sf$dist_to_nearest_youbike <- as.numeric(st_distance(combined_sf, youbike_sf[idx_yb, ], by_element = TRUE))

set.seed(123)
n_sample_size <- 10000
n_sim <- 39
dist_grid <- seq(0, 200, by = 5)

yb_buffers <- youbike_sf %>%
  st_buffer(200)

roads_filtered <- roads_3826 %>%
  filter(!fclass %in% c("motorway", "motorway_link", "trunk", "trunk_link", "steps", "pedestrian"))

roads_in_buffer <- st_filter(roads_filtered, yb_buffers)

run_simulation_network <- function(i, target_dt, network_sf) {
  rand_pts <- st_sample(network_sf, size = n_sample_size, type = "random") %>%
    st_as_sf() %>%
    st_cast("POINT")
  # 計算路網上的隨機點，到最近 YouBike 站的距離
  idx <- st_nearest_feature(rand_pts, target_dt)
  return(ecdf(as.numeric(st_distance(rand_pts, target_dt[idx, ], by_element = TRUE)))(dist_grid))
}

plan(multisession, workers = 3)

results_list <- future_lapply(1:n_sim, run_simulation_network,
                                      target_dt = youbike_sf,
                                      network_sf = roads_in_buffer,
                                      future.seed = 999)

sim_results_matrix_yb <- do.call(cbind, results_list)
plan(sequential)

df_sim_baseline <- data.frame(
  dist = dist_grid,
  hi = apply(sim_results_matrix_yb, 1, max),
  lo = apply(sim_results_matrix_yb, 1, min),
  mean = apply(sim_results_matrix_yb, 1, mean)
)

df_sim_centered <- df_sim_baseline %>%
  mutate(
    lo_diff = lo - mean,
    hi_diff = hi - mean,
    mean_diff = 0
    # mean_diff = mean
  )

obs_cdf_hourly <- combined_sf %>%
  st_drop_geometry() %>%
  group_by(hour_factor) %>%
  reframe(
    dist = dist_grid,
    obs_cdf = ecdf(dist_to_nearest_youbike)(dist_grid)
  ) %>%
  left_join(df_sim_baseline %>% select(dist, mean), by = "dist") %>%
  mutate(deviation = obs_cdf - mean)

hourly_youbike_deviation <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_ribbon(data = df_sim_centered,
              aes(x = dist, ymin = lo_diff, ymax = hi_diff),
              fill = "grey70", alpha = 0.5) +
  geom_line(data = df_sim_centered,
            aes(x = dist, y = mean_diff, linetype = "Random Baseline (CSR)"),
            color = "red", linewidth = 1) +
  geom_line(data = obs_cdf_hourly,
            aes(x = dist, y = deviation,
                # y = obs_cdf,
                color = hour_factor),
            linewidth = 0.6, alpha = 0.8) +
  scale_x_continuous(limits = c(0, 200)) +
  scale_color_viridis_d(option = "turbo", name = "Hour of Day") +
  scale_linetype_manual(name = "Baseline", values = c("Random Baseline (CSR)" = "dashed")) +
  labs(title = "Spatial Clustering Deviation across 24 Hours",
       subtitle = paste0("Difference from Random Baseline (", n_sim, " Simulations)"),
       x = "Distance to Nearest YouBike (m)",
       y = "Deviation from CSR (Observed CDF - Expected CDF)") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 8))

print(hourly_youbike_deviation)

ggsave("./Layouts/hourly_youbike_deviation.png", hourly_youbike_deviation, width = 7, height = 5)




##########################


combined_sf <- combined_sf %>%
  mutate(
    peak_status = case_when(
      hour %in% c(7, 8,  17) ~ "Peak Hours",
      # hour %in% c(9, 10, 11, 12, 13, 14, 15, 16, 18, 19) ~ "Normal Hours",
      TRUE ~ "Off-Peak Hours"
    ),
    care_type = case_when(
      `當事者區分-類別-大類別名稱-車種` == '人' ~ "Pedestrian",
      `當事者區分-類別-大類別名稱-車種` == '機車' ~ "Motorcycle",
       `當事者區分-類別-大類別名稱-車種` == '慢車' ~ "Bike",
       TRUE ~ "Car"
    ),
  )
combined_sf$peak_status%>%table()

lst <- c("事故類別名稱", "處理單位名稱警局層", "天候名稱", "光線名稱",
         "道路類別-第1當事者-名稱", "速限-第1當事者", "道路型態大類別名稱",
         "道路型態子類別名稱", "事故位置大類別名稱", "事故位置子類別名稱",
         "路面狀況-路面鋪裝名稱", "路面狀況-路面狀態名稱", "路面狀況-路面缺陷名稱",
         "道路障礙-障礙物名稱", "道路障礙-視距品質名稱", "道路障礙-視距名稱",
         "號誌-號誌種類名稱", "號誌-號誌動作名稱", "車道劃分設施-分向設施大類別名稱",
         "車道劃分設施-分向設施子類別名稱", "車道劃分設施-分道設施-快車道或一般車道間名稱",
         "車道劃分設施-分道設施-快慢車道間名稱", "車道劃分設施-分道設施-路面邊線名稱",
         "事故類型及型態大類別名稱", "事故類型及型態子類別名稱", "肇因研判大類別名稱-主要",
         "肇因研判子類別名稱-主要", "死亡受傷人數", "當事者區分-類別-大類別名稱-車種",
         "當事者區分-類別-子類別名稱-車種", "當事者屬-性-別名稱", "當事者事故發生時年齡",
         "保護裝備名稱", "行動電話或電腦或其他相類功能裝置名稱", "當事者行動狀態大類別名稱",
         "當事者行動狀態子類別名稱", "車輛撞擊部位大類別名稱-最初", "車輛撞擊部位子類別名稱-最初",
         "車輛撞擊部位大類別名稱-其他", "車輛撞擊部位子類別名稱-其他", "肇因研判大類別名稱-個別", "肇事逃逸類別名稱-是否肇逃")

for(i in c('peak_status', 'care_type')) {
  print(i)
  group_type = i
  obs_cdf_peak <- combined_sf %>%
    st_drop_geometry() %>%
    group_by(.data[[group_type]]) %>%
    reframe(
      dist = dist_grid,
      obs_cdf = ecdf(dist_to_nearest_youbike)(dist_grid)
    ) %>%
    left_join(df_sim_baseline %>% select(dist, mean), by = "dist") %>%
    mutate(deviation = obs_cdf - mean)

  peak_youbike_deviation <- ggplot() +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_ribbon(data = df_sim_centered,
                aes(x = dist, ymin = lo_diff, ymax = hi_diff),
                fill = "grey70", alpha = 0.5) +
    geom_line(data = df_sim_centered,
              aes(x = dist, y = mean_diff, linetype = "Random Baseline (CSR)"),
              color = "red", linewidth = 1) +
    geom_line(data = obs_cdf_peak,
              aes(x = dist, y = deviation, color = as.factor(.data[[group_type]])),
              linewidth = 1.2, alpha = 0.9) +
    scale_x_continuous(limits = c(0, 200)) +
    scale_color_viridis_d(option = "turbo", name = "Care Type") +
      scale_linetype_manual(name = "Baseline", values = c("Random Baseline (CSR)" = "dashed")) +
    labs(title = "Spatial Clustering Deviation: Peak vs. Off-Peak Hours",
         subtitle = paste0("Difference from Random Baseline (", n_sim, " Simulations)"),
         x = "Distance to Nearest YouBike (m)",
         y = "Deviation from CSR (Observed - Expected CDF)") +
    theme_minimal(base_family = "PingFang TC") +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          plot.title = element_text(face = "bold", size = 14))
  print(peak_youbike_deviation)

ggsave(paste0("./Layouts/testing_", i, ".png"), peak_youbike_deviation, width = 7, height = 6)
}

