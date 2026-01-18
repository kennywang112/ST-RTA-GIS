# This file is only use to source by SNA

library(tidyverse)
library(igraph)
library(networkD3)
library(ggraph)
library(tidygraph)
library(parallel)
library(doParallel)

source('../../TDA-R/MapperAlgo/R/EdgeVertices.R')
source('../../TDA-R/MapperAlgo/R/ConvertLevelsets.R')
source('../../TDA-R/MapperAlgo/R/Cover.R')
source('../../TDA-R/MapperAlgo/R/Cluster.R')
source('../../TDA-R/MapperAlgo/R/SimplicialComplex.R')
source('../../TDA-R/MapperAlgo/R/MapperAlgo.R')
source('../../TDA-R/MapperAlgo/R/Plotter.R')

filter_data <- read_csv("../../ST-RTA/ComputedDataV4/ForModel/filtered_dataV1.csv")
combined_data <- read_csv("../../ST-RTA/ComputedDataV2/Grid/combined_data.csv")

all_features <- read_csv("../../ST-RTA/ComputedDataV4/ForModel/all_features.csv")
grid_filter <- read_csv("../../ST-RTA/ComputedDataV2/Grid/grid_filter.csv")$accident_indices
all_features_grid <- cbind(all_features, grid_filter)

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


n_data <- as.data.frame(scale(filter_data))
data <- all_features%>%select(-hotspot)

# time_taken <- system.time({
#   Mapper <- MapperAlgo(
#     original_data = data,
#     filter_values = n_data[,1:3],
#     percent_overlap = 0.5,
#     # methods = "dbscan",
#     # method_params = list(eps = 0.3, minPts = 3),
#     # methods = "hierarchical",
#     # method_params = list(num_bins_when_clustering = 5, method = 'ward.D2'),
#     methods = "kmeans",
#     method_params = list(max_kmeans_clusters = 3),
#     # methods = "pam",
#     # method_params = list(num_clusters = 2),
#     cover_type = 'stride',
#     # intervals = 4,
#     interval_width = 0.7,
#     num_cores = 12
#   )
# })

# read json
library(jsonlite)
Mapper <- fromJSON("~/desktop/ST-RTA-GIS/CalculatedData/Mapper/mapper_graph.json")
