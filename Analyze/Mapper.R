library(tidyverse)
library(MapperAlgo)
library(igraph)
library(networkD3)
library(ggraph)
library(tidygraph)

filter_data <- read_csv("../ST-RTA/ComputedDataV2/ForModel/filtered_dataV1.csv")
filter_data%>%summary()

source('../TDA-R/MapperAlgo/R/Plotter.R')

all_features <- read_csv("../ST-RTA/ComputedDataV2/ForModel/all_featuresV1.csv")
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
          (道路型態大類別名稱_單路部分 > 0) |
            (道路型態大類別名稱_其他 > 0) |
            (道路型態大類別名稱_圓環廣場 > 0) |
            (道路型態大類別名稱_平交道 > 0) |
            (道路型態大類別名稱_交岔路 > 0)
        ) &
          (
            (`號誌-號誌種類名稱_行車管制號誌(附設行人專用號誌)` > 0) |
              (`號誌-號誌種類名稱_行車管制號誌` > 0) |
              (`號誌-號誌種類名稱_閃光號誌` > 0) |
              (`號誌-號誌種類名稱_無號誌` > 0)
          ) &
          (
            (`道路類別-第1當事者-名稱_市區道路` > 0)
          ) &
          (
            (original_speed < 60)
          ) &
          (
            (youbike_100m_count_mean > 0)
          )
      ),
      1,
      0
    )
  )

all_features$bn_feature%>%table()


time_taken <- system.time({
  Mapper <- MapperAlgo(
    filter_values = filter_data[,1:5],
    percent_overlap = 0.5,
    # methods = "dbscan",
    # method_params = list(eps = 0.3, minPts = 3),
    # methods = "hierarchical",
    # method_params = list(num_bins_when_clustering = 10, method = 'ward.D2'),
    methods = "kmeans",
    method_params = list(max_kmeans_clusters = 5),
    # methods = "pam",
    # method_params = list(num_clusters = 2),
    cover_type = 'stride',
    # intervals = 4,
    interval_width = 0.7,
    num_cores = 12
  )
})
time_taken

# uniq_idx <- sort(unique(unlist(Mapper$points_in_vertex, use.names = FALSE)))
# length(uniq_idx)

source('../TDA-R/MapperAlgo/R/CPEmbedding.R')
urban <- CPEmbedding(Mapper, all_features,
                     columns = list("hotspot", "號誌-號誌種類名稱_行車管制號誌"),
                     a_level = 1, b_level = 1)

MapperPlotter(Mapper,
              label=urban,
              data=filter_data,
              type="forceNetwork",
              avg=TRUE,
              use_embedding=TRUE)

MapperPlotter(Mapper,
              label=all_features$bn_feature,
              data=filter_data,
              type="forceNetwork",
              avg=TRUE,
              use_embedding=FALSE)

MapperPlotter(Mapper,
              label=all_features$hotspot,
              data=filter_data,
              type="ggraph",
              avg=TRUE,
              use_embedding=FALSE)

source('../TDA-R/MapperAlgo/R/MapperCorrelation.R')
MapperCorrelation(Mapper, data = filter_data, labels = list(all_features$bn_feature, all_features$hotspot),  use_embedding = list(FALSE, FALSE))
MapperCorrelation(Mapper, data = filter_data, labels = list(all_features$bn_feature, urban),  use_embedding = list(FALSE, TRUE))




