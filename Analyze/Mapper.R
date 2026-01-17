library(tidyverse)
library(igraph)
library(networkD3)
library(ggraph)
library(tidygraph)
library(parallel)
library(doParallel)

source('../TDA-R/MapperAlgo/R/EdgeVertices.R')
source('../TDA-R/MapperAlgo/R/ConvertLevelsets.R')
source('../TDA-R/MapperAlgo/R/Cover.R')
source('../TDA-R/MapperAlgo/R/Cluster.R')
source('../TDA-R/MapperAlgo/R/SimplicialComplex.R')
source('../TDA-R/MapperAlgo/R/MapperAlgo.R')
source('../TDA-R/MapperAlgo/R/Plotter.R')

filter_data <- read_csv("../ST-RTA/ComputedDataV4/ForModel/filtered_dataV1.csv")
filter_data%>%summary()

# all_features <- read_csv("../ST-RTA/ComputedDataV4/ForModel/all_features_gdf.csv")
all_features <- read_csv("../ST-RTA/ComputedDataV4/ForModel/all_features.csv")
all_features$hotspot <- case_when(
  all_features$hotspot == 'Not Significant' ~ 0,
  TRUE ~ 1)

all_features <- all_features%>%
  mutate(
    bn_feature = ifelse(
      (
          (
            (`車道劃分設施-分道設施-路面邊線名稱_無` > 0)
          ) &
          (
            (`當事者區分-類別-大類別名稱-車種_小客車(含客、貨兩用)` > 0)
          ) &
          (
            (`cause-group_Decision` > 0)
          )
      ),
      1,
      0
    )
  )

all_features <- all_features%>%
  mutate(
    no_lane = ifelse(
      (
        (
          (`車道劃分設施-分道設施-路面邊線名稱_無` > 0)
        ) &
          (
            (`車道劃分設施-分向設施大類別名稱_無` > 0)
          ) &
          (
            (`車道劃分設施-分道設施-快車道或一般車道間名稱_未繪設車道線` > 0)
          ) &
          (
            (`車道劃分設施-分道設施-快慢車道間名稱_未繪設快慢車道分隔線` > 0)
          ) &
          (
            (`號誌-號誌種類名稱_無號誌` > 0)
          )
      ),
      1,
      0
    )
  )

all_features <- all_features%>%
  mutate(
    no_lane = ifelse(
      (
        (
          (`車道劃分設施-分道設施-路面邊線名稱_無` > 0)
        ) &
          (
            (`車道劃分設施-分向設施大類別名稱_無` > 0)
          ) &
          (
            (`車道劃分設施-分道設施-快車道或一般車道間名稱_未繪設車道線` > 0)
          ) &
          (
            (`車道劃分設施-分道設施-快慢車道間名稱_未繪設快慢車道分隔線` > 0)
          ) &
          (
            (`號誌-號誌種類名稱_無號誌` > 0)
          )
      ),
      1,
      0
    )
  )

all_features <- all_features%>%
  mutate(
    lr_feature = ifelse(
      (
        (
          (`事故類型及型態大類別名稱_車與車` > 0)
        ) &
          (
            (`當事者區分-類別-大類別名稱-車種_人` > 0)
          ) &
          (
            (`cause-group_Unidentified` > 0)
          )
      ),
      1,
      0
    )
  )

all_features$bn_feature%>%table()
# normalize
n_data <- as.data.frame(scale(filter_data))
n_data%>%summary()

plot_data <- n_data %>%
  mutate(is_hotspot = factor(all_features$hotspot)) %>%
  pivot_longer(cols = -is_hotspot, names_to = "variable", values_to = "value")

plot_data %>%
  ggplot(aes(x = value, fill = is_hotspot)) +
  geom_histogram(bins = 50, color = "white", alpha = 0.8, position = "stack") +
  facet_wrap(~variable, scales = "free") +
  # Manual color scale with English labels
  scale_fill_manual(values = c("0" = "#999999", "1" = "#E63946"),
                    name = "Hotspot Status",  labels = c("No", "Yes")) +
  theme_minimal() +
  labs(title = "Distribution of Variables by Hotspot Status",
       subtitle = "Red indicates hotspot occurrences within the value range",
       x = "Normalized Value",
       y = "Frequency") +
  theme(legend.position = "top")


data <- all_features%>%select(-hotspot)

time_taken <- system.time({
  Mapper <- MapperAlgo(
    original_data = data,
    filter_values = n_data[,1:3],
    percent_overlap = 0.5,
    # methods = "dbscan",
    # method_params = list(eps = 0.3, minPts = 3),
    # methods = "hierarchical",
    # method_params = list(num_bins_when_clustering = 5, method = 'ward.D2'),
    methods = "kmeans",
    method_params = list(max_kmeans_clusters = 3),
    # methods = "pam",
    # method_params = list(num_clusters = 2),
    cover_type = 'stride',
    # intervals = 4,
    interval_width = 0.7,
    num_cores = 12
  )
})

length(Mapper$points_in_level_set)
unique_indexes <- unique(unlist(Mapper$points_in_vertex))
unique_indexes%>%length()
unique_levelset <- unique(unlist(Mapper$points_in_level_set))
unique_levelset%>%length()

# `車道劃分設施-分道設施-路面邊線名稱_無 x 當事者區分-類別-大類別名稱-車種_小客車(含客、貨兩用) x cause-group_Decision`
MapperPlotter(Mapper,
              label=all_features$hotspot,
              original_data=data,
              avg=TRUE,
              use_embedding=FALSE)

time_taken


# uniq_idx <- sort(unique(unlist(Mapper$points_in_vertex, use.names = FALSE)))
# length(uniq_idx)
source('../TDA-R/MapperAlgo/R/CPEmbedding.R')
# This is to get the most feature in the node
fc <- c()
feature <- 'cause-group'
for (col in colnames(all_features)) {
  if (startsWith(col, feature)) {
    fc <- c(fc, col)
  }
}
all_features <- all_features %>%
  mutate(
    max_feature_name = fc[max.col(select(., all_of(fc)), ties.method = "first")],

    chr_feature2 = str_remove(max_feature_name, paste(feature, "_", sep=""))
  )
# This is to concat multiple features

urban <- CPEmbedding(Mapper, all_features,
                     columns = list("hotspot", "bn_feature"),
                     a_level = 1, b_level = 1)
# plot
MapperPlotter(Mapper,
              # label=all_features$merged_feature,
              # label=urban,
              # label=all_features$hotspot,
              label=all_features$bn_feature,
              original_data=filter_data,
              avg=TRUE,
              use_embedding=FALSE)


source('../TDA-R/MapperAlgo/R/MapperCorrelation.R')
MapperCorrelation(Mapper, original_data = filter_data, labels = list(all_features$bn_feature, all_features$hotspot), use_embedding = list(FALSE, FALSE))
MapperCorrelation(Mapper, original_data = filter_data, labels = list(all_features$bn_feature, urban),  use_embedding = list(FALSE, TRUE))

piv <- Mapper$points_in_vertex
adj_indices <- which(Mapper$adjacency == 1, arr.ind = TRUE)
adj_indices <- adj_indices[adj_indices[, 1] < adj_indices[, 2], , drop = FALSE]
edge_weights <- apply(adj_indices, 1, function(idx) {
  length(intersect(piv[[idx[1]]], piv[[idx[2]]]))
})
