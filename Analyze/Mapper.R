library(tidyverse)
library(MapperAlgo)
library(igraph)
library(networkD3)
library(ggraph)
library(tidygraph)

all_features <- read_csv("../ST-RTA/ComputedDataV2/ForModel/all_featuresV1.csv")
filter_data <- read_csv("../ST-RTA/ComputedDataV2/ForModel/filtered_dataV1.csv")
filter_data%>%summary()

time_taken <- system.time({
  Mapper <- MapperAlgo(
    filter_values = filter_data[,1:5],
    percent_overlap = 30,
    # methods = "dbscan",
    # method_params = list(eps = 0.3, minPts = 3),
    # methods = "hierarchical",
    # method_params = list(num_bins_when_clustering = 3, method = 'ward.D2'),
    methods = "kmeans",
    method_params = list(max_kmeans_clusters = 5),
    # methods = "pam",
    # method_params = list(num_clusters = 2),
    cover_type = 'stride',
    # intervals = 4,
    interval_width = 0.5,
    num_cores = 10
  )
})
time_taken

source('../TDA-R/MapperAlgo/R/Plotter.R')
source('../TDA-R/MapperAlgo/R/ColorEmbedding.R')

all_features$hotspot <- case_when(
  all_features$hotspot == 'Not Significant' ~ 0,
  TRUE ~ 1
)
embedded <- ColorEmbedding(Mapper, all_features, 'hotspot', type='mean')
MapperPlotter(Mapper,
              label=embedded,
              data=filter_data,
              type="forceNetwork",
              avg=TRUE,
              use_embedding=TRUE)

all_features$hotspot

