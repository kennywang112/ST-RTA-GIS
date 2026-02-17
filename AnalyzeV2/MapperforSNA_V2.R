# 這個版本對應到ST-RTA的V6版本，重新建構特徵工程
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
combined_data <- read_csv("../ST-RTA/ComputedDataV2/Grid/combined_data.csv")
all_features <- read_csv('../ST-RTA/ComputedDataV6/ForModel/final_grid_features_cleaned.csv')

grid_filter <- read_csv("../ST-RTA/ComputedDataV2/Grid/grid_filter.csv")$accident_indices
all_features_grid <- cbind(all_features, grid_filter)

all_features$hotspot <- case_when(
  all_features$hotspot == 'Not Significant' ~ 0,
  TRUE ~ 1)

# read json
library(jsonlite)
Mapper <- fromJSON("~/desktop/ST-RTA-GIS/CalculatedData/Mapper/mapper_graph.json")
