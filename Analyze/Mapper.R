library(tidyverse)
library(MapperAlgo)
library(igraph)
library(networkD3)
library(ggraph)
library(tidygraph)

filter_data <- read_csv("../ST-RTA/ComputedDataV4/ForModel/filtered_dataV1.csv")
filter_data%>%summary()

source('../TDA-R/MapperAlgo/R/Plotter.R')

all_features <- read_csv("../ST-RTA/ComputedDataV4/ForModel/all_features.csv")
all_features$hotspot <- case_when(
  all_features$hotspot == 'Not Significant' ~ 0,
  TRUE ~ 1
)

all_features$`號誌-號誌種類名稱_行車管制號誌` <- ifelse(all_features$`號誌-號誌種類名稱_行車管制號誌` > 0, 1, 0)
table(all_features[, c("hotspot", "號誌-號誌種類名稱_行車管制號誌")])


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

all_features$bn_feature%>%table()

# normalize
n_data <- as.data.frame(scale(filter_data))

time_taken <- system.time({
  Mapper <- MapperAlgo(
    filter_values = n_data[,1:5],
    percent_overlap = 0.3,
    # methods = "dbscan",
    # method_params = list(eps = 0.3, minPts = 3),
    # methods = "hierarchical",
    # method_params = list(num_bins_when_clustering = 10, method = 'ward.D2'),
    methods = "kmeans",
    method_params = list(max_kmeans_clusters = 3),
    # methods = "pam",
    # method_params = list(num_clusters = 2),
    cover_type = 'stride',
    # intervals = 4,
    interval_width = 1.5,
    num_cores = 12
  )
})

MapperPlotter(Mapper,
              label=all_features$hotspot,
              data=filter_data,
              type="forceNetwork",
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
all_features <- all_features %>%
  mutate(
    merged_feature = paste(chr_feature, chr_feature2, sep = "_")
  )

urban <- CPEmbedding(Mapper, all_features,
                     columns = list("hotspot", "bn_feature"),
                     a_level = 1, b_level = 1)
# plot
MapperPlotter(Mapper,
              # label=all_features$merged_feature,
              label=urban,
              # label=all_features$hotspot,
              # label=all_features$bn_feature,
              data=filter_data,
              type="forceNetwork",
              avg=TRUE,
              use_embedding=FALSE)


source('../TDA-R/MapperAlgo/R/MapperCorrelation.R')
MapperCorrelation(Mapper, data = filter_data, labels = list(all_features$bn_feature, all_features$hotspot),  use_embedding = list(FALSE, FALSE))
MapperCorrelation(Mapper, data = filter_data, labels = list(all_features$bn_feature, urban),  use_embedding = list(FALSE, TRUE))


library(jsonlite)
export_data <- list(
  adjacency = Mapper$adjacency,
  num_vertices = Mapper$num_vertices,
  level_of_vertex = Mapper$level_of_vertex,
  points_in_vertex = Mapper$points_in_vertex,
  original_data = as.data.frame(all_features)
)

write(toJSON(export_data, auto_unbox = TRUE), "~/desktop/my_mapper_graph.json")






