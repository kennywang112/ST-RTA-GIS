source('./utils/model.R')

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
b_scores <- betweenness(g, weights = NA, normalized = TRUE)
# 找橋樑
top_nodes <- sort(b_scores, decreasing = TRUE)

b_labels <- rep(0, length(b_scores))
top_10_indices <- head(order(b_scores, decreasing = TRUE), 10)
b_labels[top_10_indices] <- 1
e_labels <- rep(0, length(e_scores))
top_10_eigen_indices <- head(order(e_scores, decreasing = TRUE), 10)
e_labels[top_10_eigen_indices] <- 1

########################this is for edge weighting############################
node_sizes <- pmax(sapply(Mapper$points_in_vertex, length), 1)
g <- graph_from_adjacency_matrix(Mapper$adjacency, mode = "undirected")

calc_edge_weight <- function(u, v) {
  size_u <- node_sizes[u]
  size_v <- node_sizes[v]
  # the weight is inversely proportional to the sum of node sizes
  # because larger nodes should have less influence on betweenness
  return(1 / (size_u + size_v))
}

edge_list <- as_edgelist(g, names = FALSE)
edge_weights <- apply(edge_list, 1, function(x) calc_edge_weight(x[1], x[2]))

b_scores_weighted <- betweenness(g, weights = edge_weights, normalized = TRUE)

MapperPlotter(
  Mapper,
  label = b_scores_weighted,
  original_data = filter_data,
  use_embedding=TRUE
)


calc_eigen_weight <- function(u, v) {
  size_u <- node_sizes[u]
  size_v <- node_sizes[v]
  return(size_u + size_v)
}

eigen_weights <- apply(edge_list, 1, function(x) calc_eigen_weight(x[1], x[2]))
e_result_weighted <- eigen_centrality(g, weights = eigen_weights)
e_scores_weighted <- e_result_weighted$vector

####################################################
source('../TDA-R/MapperAlgo/R/Plotter.R')
MapperPlotter(
  Mapper,
  label = e_scores,
  # label = e_labels,
  # label = b_labels,
  # label = b_scores,
  original_data = filter_data,
  use_embedding=TRUE
)
MapperPlotter(
  Mapper,
  label = all_features$hotspot,
  # label = all_features$lr_feature,
  original_data = filter_data,
  avg=TRUE,
  use_embedding=FALSE
)
####################### save model

library(jsonlite)
export_data <- list(
  adjacency = Mapper$adjacency,
  num_vertices = Mapper$num_vertices,
  level_of_vertex = Mapper$level_of_vertex,
  points_in_vertex = Mapper$points_in_vertex,
  original_data = as.data.frame(all_features),
  # this is the label that already calculated for each node
  cc = tibble(
    eigen_centrality = e_scores,
    betweenness = b_scores,
    weighted_betweenness = b_scores_weighted,
    weighted_eigen_centrality = e_scores_weighted,
    eigen_top10 = e_labels,
    betweenness_top10 = b_labels,
    threshold_hotspot = features$hotspott
  )
)

write(toJSON(export_data, auto_unbox = TRUE), "~/desktop/ST-RTA-GIS/CalculatedData/Mapper/mapper_graph.json")

#######################


source('../TDA-R/MapperAlgo/R/MapperCorrelation.R')
MapperCorrelation(Mapper, original_data = filter_data, labels = list(all_features$bn_feature, all_features$hotspot), use_embedding = list(FALSE, FALSE))

length(g)
length(e_scores)

features <- tibble(
  size = node_sizes <- sapply(Mapper$points_in_vertex, length),
  eigen_centrality = e_scores,
  betweenness = b_scores,
  neighbors = adj_list, # neighbor vertex indices
  points_in_vertex = Mapper$points_in_vertex # original data indices in each vertex
)

# all_features <- read_csv("../ST-RTA/ComputedDataV4/ForModel/all_features.csv")
grid_filter <- read_csv("../ST-RTA/ComputedDataV2/Grid/grid_filter.csv")$accident_indices
combined_data <- read_csv("../ST-RTA/ComputedDataV2/Grid/combined_data.csv")
all_features_grid <- cbind(all_features, grid_filter)

node_hotspot_values <- vapply(Mapper$points_in_vertex, function(idx) {
  mean(all_features$hotspot[idx], na.rm = TRUE)
}, numeric(1))
features$hotspot <- node_hotspot_values
features <- features%>%
  mutate(hotspott = ifelse(hotspot > 0.2, 1, 0))

MapperPlotter(
  Mapper,
  # label = e_scores,
  label = features$hotspott,
  # label = b_labels,
  # label = b_scores,
  original_data = filter_data,
  use_embedding=TRUE
)

filter_features <- features%>%
  mutate(
    neighbor_points = map(neighbors, function(nbr_ids) {
      # get all points in neighbor vertices
      points_list <- points_in_vertex[nbr_ids]
      all_nbr_points <- unlist(points_list)
      unique(all_nbr_points)
    }),
    # count of neighbor points
    neighbor_point_size = sapply(neighbor_points, length),
    # count of neighbor nodes
    neighbor_node_size = sapply(adj_list, length)
  )%>%
  filter(!map_lgl(neighbor_points, is.null))

# correlation plot
library(GGally)
ggpairs(filter_features,
        columns = c("size", "eigen_centrality", "betweenness",
                    "neighbor_point_size", "neighbor_node_size", "hotspot"),
        upper = list(continuous = wrap("cor", method = "spearman")))

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

################################## model
source('./utils/model.R')
# Analyze betweenness top 10 nodes
bdt <- get_model_data(filter_features, betweenness, top_k = 1)
# betweenness_tree <- tree_model_from_node(bdt[[1]], bdt[[2]], cols, cp_value=0.001)
# Analyze eigen_centrality top 10 nodes
edt <- get_model_data(filter_features, eigen_centrality, top_k = 1)
# eigen_tree <- tree_model_from_node(edt[[1]], edt[[2]], cols, cp_value=0.002)

####################### EDA betweenness and eigenvector centrality nodes
cols_eda <- c(
  '發生月份',
  '車道劃分設施-分道設施-快車道或一般車道間名稱',
  '車道劃分設施-分道設施-快慢車道間名稱',
  '車道劃分設施-分道設施-路面邊線名稱',
  '車道劃分設施-分向設施大類別名稱',
  '事故類型及型態大類別名稱',
  '道路型態大類別名稱',
  '號誌-號誌種類名稱',
  '速限-第1當事者',
  "當事者事故發生時年齡",
  "cause_group",
  "mrt_100m_count",
  "youbike_100m_count",
  "parkinglot_100m_count"
)

cols_to_remove <- c(
  "車道劃分設施-分道設施-快車道或一般車道間名稱_車道線(無標記)",
  "車道劃分設施-分道設施-快車道或一般車道間名稱_車道線(附標記)",
  "車道劃分設施-分道設施-快車道或一般車道間名稱_禁止變換車道線(無標記)",
  "車道劃分設施-分道設施-快車道或一般車道間名稱_禁止變換車道線(附標記)",
  "車道劃分設施-分道設施-快慢車道間名稱_寬式快慢車道分隔島(50公分以上)",
  "車道劃分設施-分道設施-快慢車道間名稱_快慢車道分隔線",
  "車道劃分設施-分道設施-快慢車道間名稱_窄式快慢車道分隔島(無柵欄)",
  "車道劃分設施-分道設施-快慢車道間名稱_窄式快慢車道分隔島(附柵欄)",
  "車道劃分設施-分道設施-路面邊線名稱_有",
  "車道劃分設施-分向設施大類別名稱_行車分向線",
  "車道劃分設施-分向設施大類別名稱_雙向禁止超車線",
  "車道劃分設施-分向設施大類別名稱_單向禁止超車線",
  "車道劃分設施-分向設施大類別名稱_中央分向島"
)

bdt <- get_model_grid(filter_features, betweenness, top_k = 10)
edt <- get_model_grid(filter_features, eigen_centrality, top_k = 10)

top_betweenness <- bdt[[1]]
top_eigen <- edt[[1]]
common_nodes <- inner_join(top_betweenness, top_eigen)
betweenness_only <- anti_join(top_betweenness, top_eigen)
eigen_only <- anti_join(top_eigen, top_betweenness)
dim(betweenness_only)[1] + dim(common_nodes)[1] == dim(top_betweenness)[1] &
  dim(eigen_only)[1] + dim(common_nodes)[1] == dim(top_eigen)[1]

source('./utils/model.R')

###########
top_betweenness$type <- 1
top_eigen$type <- 0

n_between <- nrow(betweenness_only)
n_eigen <- nrow(eigen_only)
target_size <- min(n_between, n_eigen)

fdt <- bind_rows(
  top_betweenness %>% sample_n(target_size),
  top_eigen %>% sample_n(target_size)
)%>%select(-bn_feature, -grid_filter, -hotspot, -all_of(cols_to_remove))


result <- glm_report(fdt)

result$importance_df%>%
  filter(Importance > 1.96)%>%
  ggplot(aes(x = Importance, y = Variable, color = Direction)) +
  geom_segment(aes(x = 0, xend = Importance, y = Variable, yend = Variable), linewidth = 1) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Betweenness" = "#E41A1C", "Eigen" = "#377EB8")) +
  theme_minimal(base_family = "PingFang TC")+
  labs(x="Variable Importance", y="Variables")
# odds ratio
result$importance_df%>%
  filter(Importance > 1.96)%>%
  ggplot(aes(x = OddsRatio, y = Variable, color = Direction)) +
  geom_segment(aes(x = 1, xend = OddsRatio, y = Variable, yend = Variable), linewidth = 1) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Betweenness" = "#E41A1C", "Eigen" = "#377EB8")) +
  theme_minimal(base_family = "PingFang TC")+
  labs(x="Odds Ratio", y="Variables")


