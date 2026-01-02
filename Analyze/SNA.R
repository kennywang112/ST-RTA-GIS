adj_matrix <- Mapper$adjacency

adj_list <- lapply(1:Mapper$num_vertices, function(i) {
  neighbors <- which(adj_matrix[i, ] > 0)
  return(neighbors[neighbors != i]) # 移除自己
})
# find total unique indices
uniq_idx <- sort(unique(unlist(Mapper$points_in_vertex, use.names = FALSE)))
length(uniq_idx) == dim(all_features)[1]

## Betweenness and Eigenvector Centrality
g <- graph_from_adjacency_matrix(Mapper$adjacency, mode = "undirected")
e_result <- eigen_centrality(g)
e_scores <- e_result$vector
b_scores <- betweenness(g, normalized = TRUE)
# 找橋樑
top_nodes <- sort(b_scores, decreasing = TRUE)
print(head(top_nodes, 10))

b_labels <- rep(0, length(b_scores))
top_10_indices <- head(order(b_scores, decreasing = TRUE), 10)
b_labels[top_10_indices] <- 1
e_labels <- rep(0, length(e_scores))
top_10_eigen_indices <- head(order(e_scores, decreasing = TRUE), 10)
e_labels[top_10_eigen_indices] <- 1

source('./Analyze/SNAplot.R')
MapperPlotterV2(
  Mapper,
  # label = e_scores,
  label = e_labels,
  data = filter_data,
  is_node_attribute = TRUE
)
MapperPlotterV2(
  Mapper,
  label = b_labels,
  # label = b_scores,
  data = filter_data,
  is_node_attribute = TRUE
)
MapperPlotterV2(
  Mapper,
  label = all_features$hotspot,
  data = filter_data,
  avg=TRUE,
  is_node_attribute = FALSE
)

MapperPlotterV2(
  Mapper,
  label = all_features$bn_feature,
  data = filter_data,
  avg=TRUE,
  is_node_attribute = FALSE
)

source('../TDA-R/MapperAlgo/R/MapperCorrelation.R')
MapperCorrelation(Mapper, original_data = filter_data, labels = list(all_features$bn_feature, all_features$hotspot), use_embedding = list(FALSE, FALSE))

length(g)
length(e_scores)

features <- tibble(
  eigen_centrality = e_scores,
  betweenness = b_scores,
  neighbors = adj_list,
  points_in_vertex = Mapper$points_in_vertex
)

# all_features <- read_csv("../ST-RTA/ComputedDataV4/ForModel/all_features.csv")
grid_filter <- read_csv("../ST-RTA/ComputedDataV2/Grid/grid_filter.csv")$accident_indices
combined_data <- read_csv("../ST-RTA/ComputedDataV2/Grid/combined_data.csv")
all_features_grid <- cbind(all_features, grid_filter)

filter_features <- features%>%
  mutate(
    neighbor_points = map(neighbors, function(nbr_ids) {
      # get all points in neighbor vertices
      points_list <- points_in_vertex[nbr_ids]
      all_nbr_points <- unlist(points_list)
      unique(all_nbr_points)
    })
  )%>%
  filter(!map_lgl(neighbor_points, is.null))

cols <- c(
  '車道劃分設施-分道設施-快車道或一般車道間名稱',
  '車道劃分設施-分道設施-快慢車道間名稱',
  '車道劃分設施-分道設施-路面邊線名稱',
  '車道劃分設施-分向設施大類別名稱',
  '事故類型及型態大類別名稱',
  '道路型態大類別名稱',
  '號誌-號誌種類名稱',
  '速限-第1當事者',
  'youbike_100m_count',
  'mrt_100m_count',
  'parkinglot_100m_count',
  '當事者區分-類別-大類別名稱-車種',
  'cause_group',
  'type'
)

source('./utils/model.R')
# Analyze betweenness top 10 nodes
bdt <- get_model_data(filter_features, betweenness, top_k = 10)
betweenness_info <- model_from_node(bdt[[1]], bdt[[2]])
# betweenness_tree <- tree_model_from_node(bdt[[1]], bdt[[2]], cols, cp_value=0.001)
betweenness_info$accuracy
betweenness_info$confusion_matrix

betweenness_info$importance_df%>%
  filter(Importance > 1.96)%>%
  ggplot(aes(x = Importance, y = Variable, color = Direction)) +
  geom_segment(aes(x = 0, xend = Importance, y = Variable, yend = Variable), linewidth = 1) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Positive" = "#E41A1C", "Negative" = "#377EB8")) +
  theme_minimal()

# Analyze eigen_centrality top 10 nodes
edt <- get_model_data(filter_features, eigen_centrality, top_k = 10)
eigen_info <- model_from_node(edt[[1]], edt[[2]], cols)
# eigen_tree <- tree_model_from_node(edt[[1]], edt[[2]], cols, cp_value=0.002)
eigen_info$accuracy

eigen_info$importance_df%>%
  filter(Importance > 1.96)%>%
  ggplot(aes(x = Importance, y = Variable, color = Direction)) +
  geom_segment(aes(x = 0, xend = Importance, y = Variable, yend = Variable), linewidth = 1) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Positive" = "#E41A1C", "Negative" = "#377EB8")) +
  theme_minimal()


betweenness_info$importance_df$type <- 'betwenness'
eigen_info$importance_df$type <- 'eigen_centrality'
fdt <- rbind(betweenness_info$importance_df, eigen_info$importance_df)%>%
  filter(Importance > 1.96)
fdt

# 下一步: 分析節點周圍特徵的種類
# 熱點最集中的特徵以及非熱點最集中的特徵是什麼

ggplot(features, aes(x = eigen_centrality)) +
  geom_histogram(binwidth = 0.03, fill = "#69b3a2", color = "white", alpha = 0.8) +
  theme_minimal()

ggplot(features, aes(x = betweenness)) +
  geom_histogram(binwidth = 0.003, fill = "#69b3a2", color = "white", alpha = 0.8) +
  theme_minimal()
