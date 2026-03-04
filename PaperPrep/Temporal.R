library(tidyverse)

combined_data_in_taiwan <- read_csv("../ST-RTA/ComputedDataV2/Accident/combined_data_in_taiwan.csv")


library(lubridate)

weekly_trends <- final_boundary_data %>%
  mutate(
    date_parsed = ymd(發生日期),
    week_start = floor_date(date_parsed, unit = "day")) %>%
  count(week_start, name = "accident_count")

ggplot(weekly_trends, aes(x = week_start, y = accident_count)) +
  geom_line(color = "#1f77b4", size = 1) +
  geom_point(color = "#1f77b4", size = 2, alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", linetype = "dashed", se = FALSE, span = 0.2) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  labs(
    title = "Daily Trend of Traffic Accidents",
    x = "Date",
    y = "Number of Accidents") +
  theme_minimal(base_family = "PingFang TC") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16))
