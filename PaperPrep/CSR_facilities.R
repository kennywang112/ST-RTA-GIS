library(sf)
library(tidyverse)
library(lubridate)
library(future.apply)
plan(multisession, workers = 3)
message(paste("Cell:", availableCores() - 1))


roads <- st_read(dsn="./Data/road/gis_osm_roads_free_1.shp", layer="gis_osm_roads_free_1")
youbike_sf <- read_csv('../ST-RTA/ComputedData/Youbike/full_youbike.csv')%>%
  st_as_sf(coords = c("PositionLon", "PositionLat"), crs = 4326)%>%
  st_transform(3826)%>%
  slice_sample(n = 1000)
final_boundary_data <- read_csv("../ST-RTA/ComputedDataV7/Data/Final_boundary_data.csv")

# 這是python經過篩選後計算出來的grid gi，雖然用taiwan 篩選的boundary直接處理更好但現在這樣比較方便
final_boundary <- read_csv('../ST-RTA/ComputedDataV7/Grid/grid_data_區級篩選.csv') %>% st_as_sf(wkt = "geometry", crs = 3826)
bus <- st_read(dsn='./Data/road_new.shp/gis_osm_transport_free_1.shp') %>%
  filter(fclass == "bus_stop") %>%
  st_transform(3826)
bus_filtered <- bus %>% st_filter(final_boundary, .predicate = st_intersects) %>%
  slice_sample(n = nrow(youbike_sf))

mrt_sf <- read_csv('../ST-RTA/ComputedData/MRT/full_mrt.csv')%>%
  st_as_sf(coords = c("PositionLon", "PositionLat"), crs = 4326)%>%
  st_transform(3826)
mrt_filtered <- mrt_sf %>% st_filter(final_boundary, .predicate = st_intersects) %>%
  slice_sample(n = nrow(youbike_sf))

differnce_filtered <- st_read("./CalculatedData/pairs_annot_all_cities.shp")%>%
  st_as_sf(geometry = "geometry", crs = 3826) %>% st_filter(final_boundary, .predicate = st_intersects) %>%
  slice_sample(n = nrow(youbike_sf))

parking_lot <- read_csv('../ST-RTA/ComputedData/Parkinglot/full_parkinglot.csv') %>%
  st_as_sf(coords = c("PositionLon", "PositionLat"), crs = 4326) %>%
  st_transform(3826)
parking_lot_filtered <- parking_lot %>% st_filter(final_boundary, .predicate = st_intersects) %>%
  slice_sample(n = nrow(youbike_sf))
########################## Bus Stop, MRT, Difference Filter ended
final_boundary_data <- final_boundary_data %>%
  mutate(
    hour = floor(發生時間 / 10000),
    date_parsed = ymd(發生日期),)

final_boundary_data %>%
  count(hour) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 2.5, color = "darkblue") +
  geom_hline(yintercept = 40000, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = 25000, linetype = "dashed", color = "red", size = 1) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Hourly Distribution of Traffic Accidents",
    x = "Time of Day (Hour)",
    y = "Number of Accidents") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16))

combined_sf <- final_boundary_data %>%
  mutate(hour_factor = factor(sprintf("%02d:00", hour), levels = sprintf("%02d:00", 0:23))) %>%
  filter(!is.na(hour_factor)) %>%
  st_as_sf(coords = c("經度", "緯度"), crs = 4326) %>%
  st_transform(3826)

combined_sf <- combined_sf %>%
  mutate(
    peak_status = case_when(
      # hour %in% c(7, 8, 16, 17, 18) ~ "Peak Hours",
      hour %in% c(7, 8, 17) ~ "Peak Hours",
      hour %in% c(9, 10, 11, 12, 13, 14, 15, 16, 18, 19) ~ "Normal Hours",
      TRUE ~ "Off-Peak Hours"
    ),
    car_type = case_when(
      `當事者區分-類別-大類別名稱-車種` == '人' ~ "Pedestrian",
      `當事者區分-類別-大類別名稱-車種` == '機車' ~ "Motorcycle",
      `當事者區分-類別-大類別名稱-車種` == '慢車' ~ "Bike",
      TRUE ~ "Car"
    ),
    speed_group = case_when(
      `速限-第1當事者` < 50 ~ "Low Speed (<50)",
      `速限-第1當事者` == 50 ~ "Medium Speed (50)",
      `速限-第1當事者` > 50 ~ "High Speed (>50)",
      TRUE ~ "Other"
    )
  )

set.seed(123)
n_sim <- 39
dist_grid <- seq(0, 500, by = 5)

run_simulation_network <- function(i, target_dt, network_sf, size = 10000) {
  rand_pts <- st_sample(network_sf, size = size, type = "random") %>%
    st_as_sf() %>%
    st_cast("POINT")
  # 計算路網上的隨機點，到最近 YouBike 站的距離
  idx <- st_nearest_feature(rand_pts, target_dt)
  dists <- as.numeric(st_distance(rand_pts, target_dt[idx, ], by_element = TRUE))

  valid_dists <- dists[dists <= 500]

  return(ecdf(valid_dists)(dist_grid))
}


chosed <- mrt_filtered # youbike_sf, mrt_filtered, bus_filtered

roads_3826  <- roads%>%st_transform(3826)
roads_filtered <- roads_3826 %>%
  filter(!fclass %in% c("motorway", "motorway_link", "trunk", "trunk_link", "steps", "pedestrian"))


poi_to_analyze <- list(
  "YouBike" = youbike_sf,
  "Bus_Stop" = bus_filtered,
  "MRT" = mrt_filtered,
  "Parking_Lot" = parking_lot_filtered,
  "Difference_Filter" = differnce_filtered
)


all_simulation <- list()
all_obs_distances <- list()
all_local_sf <- list()
for (poi_name in names(poi_to_analyze)) {

  message(poi_name)

  chosed_poi <- poi_to_analyze[[poi_name]]

  idx_all <- st_nearest_feature(combined_sf, chosed_poi)
  message('keep 1')
  dist_all <- as.numeric(st_distance(combined_sf, chosed_poi[idx_all, ], by_element = TRUE))
  message('keep 2')
  local_combined_sf <- combined_sf[dist_all <= 500, ]
  message('keep 3')
  all_local_sf[[poi_name]] <- local_combined_sf
  message('keep 4')
  all_obs_distances[[poi_name]] <- dist_all[dist_all <= 500]
  message('keep 5')
  local_roads <- roads_filtered[lengths(st_is_within_distance(roads_filtered, chosed_poi, dist = 500)) > 0, ]

  message('keep 6')

  plan(multisession, workers = 3)

  results_list <- future_lapply(1:n_sim, run_simulation_network,
                                size = 10000,
                                target_dt = chosed_poi,
                                network_sf = local_roads,
                                future.seed = 999)

  all_simulation[[poi_name]] <- do.call(cbind, results_list)

}

plan(sequential)

lst <- c("事故類別名稱", "天候名稱", "光線名稱",
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
         "肇因研判大類別名稱-個別", "肇事逃逸類別名稱-是否肇逃", 'peak_status')

##########################
for(poi in c("YouBike" , "Bus_Stop", "MRT" , "Difference_Filter", "Parking_Lot")) {
  target_poi <- poi

  current_sim_matrix <- all_simulation[[target_poi]]
  plot_sf <- all_local_sf[[target_poi]]
  plot_sf$current_dist <- all_obs_distances[[target_poi]]
  overall_accident_cdf <- ecdf(plot_sf$current_dist)(dist_grid)


  df_sim_baseline <- data.frame(
    dist = dist_grid,
    mean = apply(current_sim_matrix, 1, mean),
    hi = apply(current_sim_matrix, 1, max),
    lo = apply(current_sim_matrix, 1, min)
  )
  df_sim_centered <- df_sim_baseline %>%
    mutate(lo_diff = lo - mean, hi_diff = hi - mean, mean_diff = 0)

  for(i in lst) {

    print(i)
    group_type = i

    group_totals <- plot_sf %>%
      # filter(!.data[[i]] %in% c("平交道", "其他"))%>%
      st_drop_geometry() %>%
      count(.data[[group_type]], name = "total_n")

    obs_cdf_peak <- plot_sf %>%
      # filter(!.data[[i]] %in% c("平交道", "其他"))%>%
      st_drop_geometry() %>%
      group_by(.data[[group_type]]) %>%
      reframe(
        dist = dist_grid,
        obs_cdf = ecdf(current_dist)(dist_grid)) %>%
      left_join(df_sim_baseline %>% select(dist, mean), by = "dist") %>%
      mutate(deviation = obs_cdf - mean)

    peak_youbike_deviation <- ggplot() +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
      geom_ribbon(data = df_sim_centered,
                  aes(x = dist,
                      ymin = lo_diff,
                      ymax = hi_diff),
                  fill = "grey70", alpha = 0.5) +
      geom_line(data = df_sim_centered,
                aes(x = dist, y = mean_diff, linetype = "Random Baseline (CSR)"),
                color = "red", linewidth = 1) +
      geom_line(data = obs_cdf_peak,
                aes(x = dist, y = deviation, color = as.factor(.data[[group_type]])),
                linewidth = 1.2, alpha = 0.9) +
      scale_x_continuous(limits = c(0, 500)) +
      scale_color_viridis_d(option = "turbo", name = group_type) +
      scale_linetype_manual(name = "Baseline", values = c("Random Baseline (CSR)" = "dashed")) +
      labs(title = paste("Spatial Accident:", group_type),
           subtitle = paste0("Relative Probability Deviation (Observed - Simulated Baseline) | n_sim = ", n_sim),
           x = "Distance to Nearest POI (m)",
           y = "Deviation") +
      theme_minimal(base_family = "PingFang TC") +
      theme(legend.position = "bottom",
            legend.box = "vertical",
            plot.title = element_text(face = "bold", size = 14))
    print(peak_youbike_deviation)

  ggsave(paste0("./Layouts/deviation_testing_", i, "_", target_poi, ".png"), peak_youbike_deviation, width = 7, height = 6)
  }
}
