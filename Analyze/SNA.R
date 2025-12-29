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

# Analyze betweenness top 10 nodes
top_10_betweenness <- filter_features%>%arrange(desc(betweenness))%>%head(30)

## Get the unique indices from grid filter
unique_indices <- unique(unlist(top_10_betweenness$points_in_vertex))

length(unlist(top_10_betweenness$points_in_vertex))
##.get original data indices from grid filter
top_betweenness_features <- all_features_grid[unique_indices,]
rest_of_features <- all_features_grid[-unique_indices,]
dim(top_betweenness_features)[1] + dim(rest_of_features)[1] == dim(all_features_grid)[1]

# get original data
library(jsonlite)
parsed_list <- lapply(top_betweenness_features$grid_filter, fromJSON)
top_combined_data_indices <- unique(unlist(parsed_list))

top_betweenness_combined <- combined_data[top_combined_data_indices, ]
rest_of_combined <- combined_data[-top_combined_data_indices, ]
dim(rest_of_combined)[1] + dim(top_betweenness_combined)[1] == dim(combined_data)[1]

features <- c(
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

top_betweenness_combined$type <- 1
rest_of_combined$type <- 0
top_betweenness_combined%>%colnames()
test_betweenness_data <- rbind(top_betweenness_combined, rest_of_combined)%>%
  select(all_of(features))

model <- glm(type ~ ., data = test_betweenness_data, family = binomial)
summary(model)
# This is to get odds ratio and confidence interval
exp(cbind(OR = coef(model), confint(model)))

library(caret)
imp <- varImp(model)

# library(dplyr)
# library(stringr)

imp_df <- data.frame(
  Variable = rownames(imp),
  Importance = imp$Overall
)
imp_df <- imp_df[order(imp_df$Importance, decreasing = FALSE), ]
imp_df$Variable <- factor(imp_df$Variable, levels = imp_df$Variable)

ggplot(imp_df, aes(x = Importance, y = Variable)) +
  geom_point(size = 3, color = "steelblue") +
  geom_segment(aes(x = 0, xend = Importance, y = Variable, yend = Variable), color = "steelblue") +
  theme_minimal() +
  labs(title = "Type var imprortance",
       x = "Importance",
       y = "var name")

# Analyze eigen_centrality top 10 nodes
top_10_eigen <- filter_features%>%arrange(desc(eigen_centrality))%>%head(30)

## Get the unique indices from grid filter
unique_indices <- unique(unlist(top_10_eigen$points_in_vertex))

length(unlist(top_10_eigen$points_in_vertex))
##.get original data indices from grid filter
top_eigen_features <- all_features_grid[unique_indices,]
rest_of_features <- all_features_grid[-unique_indices,]
dim(top_eigen_features)[1] + dim(rest_of_features)[1] == dim(all_features_grid)[1]

# get original data
library(jsonlite)
parsed_list <- lapply(top_eigen_features$grid_filter, fromJSON)
top_combined_data_indices <- unique(unlist(parsed_list))

top_eigen_combined <- combined_data[top_combined_data_indices, ]
rest_of_combined <- combined_data[-top_combined_data_indices, ]
dim(rest_of_combined)[1] + dim(top_eigen_combined)[1] == dim(combined_data)[1]

top_eigen_combined$type <- 1
rest_of_combined$type <- 0
top_eigen_combined%>%colnames()
test_eigen_data <- rbind(top_eigen_combined, rest_of_combined)%>%
  select(all_of(features))

model <- glm(type ~ ., data = test_eigen_data, family = binomial)
summary(model)
# This is to get odds ratio and confidence interval
exp(cbind(OR = coef(model), confint(model)))

library(caret)
# overall = beta/std error
imp <- varImp(model)

imp_df <- data.frame(Variable = rownames(imp), Importance = imp$Overall)
imp_df <- imp_df[order(imp_df$Importance, decreasing = FALSE), ]
imp_df$Variable <- factor(imp_df$Variable, levels = imp_df$Variable)

ggplot(imp_df, aes(x = Importance, y = Variable)) +
  geom_point(size = 3, color = "steelblue") +
  geom_segment(aes(x = 0, xend = Importance, y = Variable, yend = Variable), color = "steelblue") +
  theme_minimal() +
  labs(title = "Type var imprortance",
       x = "Importance",
       y = "var name")
