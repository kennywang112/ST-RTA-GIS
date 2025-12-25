################# SNA ###################
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
