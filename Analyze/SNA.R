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

source('./Analyze/SNAplot.R')
MapperPlotterV2(
  Mapper,
  label = e_scores,
  data = filter_data,
  is_node_attribute = TRUE
)
MapperPlotterV2(
  Mapper,
  label = b_scores,
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
## 嘗試看看這些指標和熱點有沒有相關性

e_scores
b_scores

length(g)
length(e_scores)

features <- tibble(
  eigen_centrality = e_scores,
  betweenness = b_scores,
  neighbors = adj_list,
  points_in_vertex = Mapper$points_in_vertex
)

all_features <- read_csv("../ST-RTA/ComputedDataV4/ForModel/all_features.csv")
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

top_10_betweenness <- filter_features%>%arrange(desc(betweenness))%>%head(10)

length(unlist(top_10_betweenness$points_in_vertex))
top_betweenness_features <- all_features_grid[unlist(top_10_betweenness$points_in_vertex),]
rest_of_features <- all_features_grid[-unlist(top_10_betweenness$points_in_vertex),]

raw_indices <- unlist(top_10_betweenness$points_in_vertex)
unique_indices <- unique(raw_indices)
length(unlist(top_betweenness_features$grid_filter))
length(unique(unlist(top_betweenness_features$grid_filter)))

top_betweenness_combined <- combined_data[unique_indices, ]
rest_of_combined <- combined_data[-unique_indices, ]

dim(rest_of_combined)[1] + dim(top_betweenness_combined)[1] == dim(combined_data)[1]

