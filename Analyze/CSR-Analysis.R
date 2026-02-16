library(sf)
library(tidyverse)
# 這個檔案的目的是找出離每個事故最近的youbike設施
youbike <- read_csv('../ST-RTA/ComputedData/Youbike/full_youbike.csv')
combined_data1 <- read_csv("~/Desktop/ST-RTA/ComputedDataV2/Accident/combined_data_in_taiwan.csv")
combined_data <- combined_data1%>%filter(`道路類別-第1當事者-名稱` == '市區道路')

youbike_sf <- st_as_sf(youbike, coords = c("PositionLon", "PositionLat"), crs = 4326) %>%
  st_transform(3826)

combined_sf <- st_as_sf(combined_data, coords = c("經度", "緯度"), crs = 4326) %>%
  st_transform(3826)

nearest_idx <- st_nearest_feature(combined_sf, youbike_sf)
dist_to_youbike <- st_distance(combined_sf, youbike_sf[nearest_idx,], by_element = TRUE)

combined_data$dist_to_nearest_youbike <- as.numeric(dist_to_youbike)
threshold <- quantile(combined_data$dist_to_nearest_youbike, 0.90, na.rm = TRUE)
med <- median(combined_data$dist_to_nearest_youbike, na.rm = TRUE)
mean_dist <- mean(combined_data$dist_to_nearest_youbike, na.rm = TRUE)
# plot distribution
distance_youbike <- combined_data%>%
  filter(dist_to_nearest_youbike <= threshold) %>%
  ggplot(aes(x = dist_to_nearest_youbike)) +
  geom_histogram(binwidth = 10) +
  # medianline
  geom_vline(xintercept = med, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean_dist, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = med, y = Inf, label = paste("Median:", round(med, 2)),
           color = "blue", vjust = 1.5, hjust = 1.1, size = 3.5) +
  annotate("text", x = mean_dist, y = Inf, label = paste("Median:", round(mean_dist, 2)),
           color = "red", vjust = 1.5, hjust = 1.1, size = 3.5) +
  labs(title = "Distribution of Distance to Nearest Youbike Station",
       x = "Distance to Nearest Youbike Station (meters)",
       y = "Frequency") +
  theme_minimal()
ggsave("./Layouts/distance_to_youbike_histogram.png", distance_youbike, width = 8, height = 5)

# Simulation
# https://rpubs.com/Peters64s/548577
library(future.apply)
plan(multisession, workers = 3)
message(paste("Cell:", availableCores() - 1))

n_sample_size <- 10000
n_sim <- 50
n_points <- nrow(combined_data)
dist_grid <- seq(0, 500, by = 5)

urban_mask <- youbike_sf %>%
  st_buffer(500) %>%
  st_union() %>%
  st_as_sf()

set.seed(123)
accidents_sampled <- combined_data %>% sample_n(n_sample_size)
real_dists <- accidents_sampled$dist_to_nearest_youbike
real_cdf <- ecdf(real_dists)(dist_grid)

run_simulation <- function(i) {
  rand_pts <- st_sample(urban_mask, size = n_sample_size) %>%
    st_as_sf() %>%
    st_cast("POINT")

  idx <- st_nearest_feature(rand_pts, youbike_sf)
  dists <- st_distance(rand_pts, youbike_sf[idx,], by_element = TRUE)
  dists_numeric <- as.numeric(dists)

  return(ecdf(dists_numeric)(dist_grid))
}

results_list <- future_lapply(1:n_sim, run_simulation, future.seed = 999)
# col: times of simulation, row: distance grid
sim_results_matrix <- do.call(cbind, results_list)
plan(sequential)

env_hi <- apply(sim_results_matrix, 1, max)
env_lo <- apply(sim_results_matrix, 1, min)
env_mean <- apply(sim_results_matrix, 1, mean)

real_dists <- combined_data$dist_to_nearest_youbike
real_cdf <- ecdf(real_dists)(dist_grid)

plot_data <- data.frame(
  distance = dist_grid,
  obs = real_cdf,
  hi = env_hi,
  lo = env_lo,
  mean = env_mean
)

simulation <- ggplot(plot_data, aes(x = distance)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey70", alpha = 0.5) +
  geom_line(aes(y = mean, color = "Random Baseline (CSR)"), linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = obs, color = "Real Accidents"), linewidth = 1.2) +
  scale_color_manual(values = c("Real Accidents" = "black", "Random Baseline (CSR)" = "red")) +
  labs(title = "Spatial Association Test",
       subtitle = paste0("Observed vs. ", n_sim, " Simulations"),
       x = "Distance to Nearest YouBike (meters)",
       y = "Cumulative Proportion (CDF)",
       color = "Legend") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("./Layouts/simulation.png", simulation, width = 5, height = 5)

# 串連速限以及youbike之間的關聯
med <- median(combined_data$`速限-第1當事者`, na.rm = TRUE)
mean_dist <- mean(combined_data$`速限-第1當事者`, na.rm = TRUE)
speed_distribution <- combined_data%>%
  ggplot() +
  geom_histogram(aes(x = `速限-第1當事者`), binwidth = 10, fill = "lightblue", color = "black") +
   geom_vline(xintercept = med, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean_dist, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = med, y = Inf, label = paste("Median:", round(med, 2)),
           color = "blue", vjust = 1.5, hjust = -0.1, size = 3.5) +
  annotate("text", x = mean_dist, y = Inf, label = paste("Mean:", round(mean_dist, 2)),
           color = "red", vjust = 1.5, hjust = 1.1, size = 3.5) +
  labs(title = "Distribution of Speed Limits for Urban Road Accidents",
       x = "Speed Limit (km/h)",
       y = "Frequency") +
  theme_minimal()
ggsave("./Layouts/speed_limit_distribution.png", speed_distribution, width = 8, height = 5)

plot_data_stratified <- combined_data %>%
  mutate(speed_group = case_when(
    `速限-第1當事者` < 50 ~ "Low Speed (<50)",
    `速限-第1當事者` == 50 ~ "Medium Speed (50)",
    `速限-第1當事者` > 50 ~ "High Speed (>50)",
    TRUE ~ "Other"
  ))

random_points <- st_sample(urban_mask, size = 10000) %>% st_as_sf() %>% st_cast("POINT")
nearest_idx_rand <- st_nearest_feature(random_points, youbike_sf)
dist_rand_matrix <- st_distance(random_points, youbike_sf[nearest_idx_rand, ], by_element = TRUE)
dist_rand <- as.numeric(dist_rand_matrix)
df_random <- data.frame(dist = dist_rand)

speed_limit_youbike <- ggplot() +
  stat_ecdf(data = df_random,
            aes(x = dist, linetype = "Random Baseline (CSR)"),
            geom = "step", linewidth = 1, color = "grey50") +
  stat_ecdf(data = plot_data_stratified,
            aes(x = dist_to_nearest_youbike, color = speed_group),
            geom = "step", linewidth = 0.5, linetype = "solid") +
  scale_x_continuous(limits = c(0, 500)) +
  scale_color_manual(values = c("High Speed (>50)" = "#E41A1C",
                                "Medium Speed (50)" = "#377EB8",
                                "Low Speed (<50)" = "#4DAF4A")) +
  scale_linetype_manual(name = "Data Type", values = c("Random Baseline (CSR)" = "dashed")) +
  labs(title = "YouBike Stations on Accidents across Speed Limits",
       x = "Distance to Nearest YouBike (m)",
       y = "Cumulative Proportion of Accidents (CDF)") +
  theme_minimal()+
  theme(legend.position = "bottom", legend.box = "vertical")

ggsave("./Layouts/speed_limit_youbike_cdf.png", speed_limit_youbike, width = 5, height = 5)

# Speed difference
# boundary <- st_read('../ST-RTA/ComputedDataV2/Taiwan/taiwan.shp')
boundary <- read_csv('../ST-RTA/ComputedDataV2/Taiwan/taiwan_無離島.csv')%>%
  st_as_sf(wkt = "geometry", crs = 3826)

differnce <- st_read("./CalculatedData/pairs_annot_all_cities.shp")%>%
  st_as_sf(geometry = "geometry", crs = 3826)%>%
  # find the points in boundary
  st_join(boundary, join = st_within, left = FALSE)

## speed 不討論速差，只考慮距離到速差點
nearest_idx <- st_nearest_feature(combined_sf, differnce)
dist_to_spd <- st_distance(combined_sf, differnce[nearest_idx,], by_element = TRUE)
combined_data$dist_to_nearest_spd <- as.numeric(dist_to_spd)

n_sample_size <- 10000
n_sim <- 50
n_points <- nrow(combined_data)
dist_grid <- seq(0, 300, by = 5)

urban_mask <- differnce %>%
  st_buffer(300) %>%
  st_union() %>%
  st_as_sf()

set.seed(123)
accidents_sampled <- combined_data %>% sample_n(n_sample_size)
real_dists <- accidents_sampled$dist_to_nearest_spd
real_cdf <- ecdf(real_dists)(dist_grid)

run_simulation <- function(i) {
  rand_pts <- st_sample(urban_mask, size = n_sample_size) %>%
    st_as_sf() %>%
    st_cast("POINT")

  idx <- st_nearest_feature(rand_pts, differnce)
  dists <- st_distance(rand_pts, differnce[idx,], by_element = TRUE)
  dists_numeric <- as.numeric(dists)

  return(ecdf(dists_numeric)(dist_grid))
}

results_list <- future_lapply(1:n_sim, run_simulation, future.seed = 999)
# col: times of simulation, row: distance grid
sim_results_matrix <- do.call(cbind, results_list)
plan(sequential)

env_hi <- apply(sim_results_matrix, 1, max)
env_lo <- apply(sim_results_matrix, 1, min)
env_mean <- apply(sim_results_matrix, 1, mean)

real_dists <- combined_data$dist_to_nearest_spd
real_cdf <- ecdf(real_dists)(dist_grid)

plot_data <- data.frame(
  distance = dist_grid,
  obs = real_cdf,
  hi = env_hi,
  lo = env_lo,
  mean = env_mean
)

simulation <- ggplot(plot_data, aes(x = distance)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey70", alpha = 0.5) +
  geom_line(aes(y = mean, color = "Random Baseline (CSR)"), linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = obs, color = "Real Accidents"), linewidth = 1.2) +
  scale_color_manual(values = c("Real Accidents" = "black", "Random Baseline (CSR)" = "red")) +
  labs(title = "Spatial Association Test",
       subtitle = paste0("Observed vs. ", n_sim, " Simulations"),
       x = "Distance to Nearest Speed Difference (meters)",
       y = "Cumulative Proportion (CDF)",
       color = "Legend") +
  theme_minimal() +
  theme(legend.position = "bottom")
simulation
ggsave("./Layouts/simulation_speed.png", simulation, width = 5, height = 5)

# distribution
combined_data$dist_to_nearest_spd <- as.numeric(dist_to_spd)
threshold <- quantile(combined_data$dist_to_nearest_spd, 0.95, na.rm = TRUE)
med <- median(combined_data$dist_to_nearest_spd, na.rm = TRUE)
mean_dist <- mean(combined_data$dist_to_nearest_spd, na.rm = TRUE)
distance_spd <- combined_data%>%
  filter(dist_to_nearest_spd <= threshold) %>%
  ggplot(aes(x = dist_to_nearest_spd)) +
  geom_histogram(binwidth = 10) +
  geom_vline(xintercept = med, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean_dist, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = med, y = Inf, label = paste("Median:", round(med, 2)),
           color = "blue", vjust = 1.5, hjust = 1.1, size = 3.5) +
  annotate("text", x = mean_dist, y = Inf, label = paste("Median:", round(mean_dist, 2)),
           color = "red", vjust = 1.5, hjust = 1.1, size = 3.5) +
  labs(title = "Distribution of Distance to Nearest Youbike Station",
       x = "Distance to Nearest Youbike Station (meters)",
       y = "Frequency") +
  theme_minimal()
ggsave("./Layouts/distance_to_spd_histogram.png", distance_spd, width = 8, height = 5)
