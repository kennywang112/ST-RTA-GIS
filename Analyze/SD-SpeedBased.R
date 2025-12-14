## 這裏是以速限差為中心buffer找資料點，但是應該使用youbike為基礎找速限差點點
## 大部分處理資料都在SD-AccidentBased
## This is old version

library(tmap)
tmap_mode("view")
tm_shape(pairs_annot) +
  tm_dots(size = 0.5, col = "spd_dlt", palette = "Reds", style = "quantile")

specific_pairs_annot <- pairs_annot%>%filter(COUNTYNAME=="臺北市")

specific_pairs_annot%>%
  tm_shape() +
  tm_dots(size = 0.5, col = "spd_dlt", palette = "Reds", style = "quantile")

specific_combined_data <- combined_data_in_taiwan %>% filter(COUNTYNAME == "臺北市")

# crs transformation
specific_pairs_annot_3826  <- st_transform(specific_pairs_annot, crs)

specific_combined_data_sf <- st_as_sf(specific_combined_data, coords = c("經度", "緯度"), crs = crs_init)
specific_combined_data_3826 <- st_transform(specific_combined_data_sf, crs)
specific_combined_data_3826%>%select(geometry)

buf_dist <- 100
pairs_buf <- st_buffer(specific_pairs_annot_3826, dist = buf_dist) %>%
  st_make_valid() %>%
  mutate(buf_id = row_number())

pairs_buf %>%
  tm_shape() +
  tm_polygons(col = "spd_dlt", palette = "Reds", style = "quantile",
              alpha = 0.7, border.col = "grey40", lwd = 0.5) +
  tm_layout(legend.outside = TRUE, frame = FALSE)

acc_with_flag <- st_join(
  specific_combined_data_3826,
  pairs_buf,
  join = st_within,
  left = TRUE
) %>%
  mutate(in_buf = if_else(is.na(buf_id), "outside", "inside"))


ratio_table <- acc_with_flag %>%
  count(in_buf, youbike_100m_count) %>%
  group_by(in_buf) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ratio_table%>%
  ggplot()+
  geom_bar(aes(x = youbike_100m_count, y = prop, fill = in_buf), stat = "identity", position = "dodge")

