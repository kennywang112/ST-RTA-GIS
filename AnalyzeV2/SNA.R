library(tidyverse)

# read json file
Mapper <- jsonlite::fromJSON("~/desktop/my_mapper_graph.json")
filter_data <- read_csv("../../ST-RTA/ComputedDataV4/ForModel/filtered_dataV1.csv")

MapperPlotter(
  Mapper,
  label = Mapper$cc$betweenness,
  original_data = filter_data,
  use_embedding=TRUE
)


b_labels <- rep(0, length(b_scores))
top_10_indices <- head(order(b_scores, decreasing = TRUE), 10)
b_labels[top_10_indices] <- 1

cc <- Mapper$cc
Q3_x <- quantile(cc$betweenness, 0.75, na.rm = TRUE)
IQR_x <- IQR(cc$betweenness, na.rm = TRUE)
upper_bound_x <- Q3_x + 1.5 * IQR_x

ggplot(cc, aes(x = betweenness, y = threshold_hotspot)) +
  geom_point(color = "grey", alpha = 0.7) +
  geom_point(data = cc %>% filter(betweenness > upper_bound_x, threshold_hotspot > 0.5),
             color = "red", size = 3) +
  geom_vline(xintercept = upper_bound_x, linetype = "dashed")

hotspot_and_high_between <- cc%>%filter(betweenness > upper_bound_x, threshold_hotspot > 0.5)


rpart.plot(tree_model, family = "PingFang")
