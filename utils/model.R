# This is for SNA analysis

library(caret)
# library(dplyr)
# library(stringr)

model_from_node <- function(features, sort_metric, target_cols, top_k = 30) {

  top_features <- features%>%arrange(desc({{sort_metric}}))%>%head(top_k)

  ## Get the unique indices from grid filter
  unique_indices <- unique(unlist(top_features$points_in_vertex))

  length(unlist(top_features$points_in_vertex))
  ##.get original data indices from grid filter
  top_grid_features <- all_features_grid[unique_indices,]
  rest_of_features <- all_features_grid[-unique_indices,]
  print(dim(top_grid_features)[1] + dim(rest_of_features)[1] == dim(all_features_grid)[1])

  # get original data
  library(jsonlite)
  parsed_list <- lapply(top_grid_features$grid_filter, fromJSON)
  top_combined_data_indices <- unique(unlist(parsed_list))

  top_combined <- combined_data[top_combined_data_indices, ]
  rest_of_combined <- combined_data[-top_combined_data_indices, ]
  print(dim(rest_of_combined)[1] + dim(top_combined)[1] == dim(combined_data)[1])

  top_combined$type <- 1
  rest_of_combined$type <- 0
  test_data <- rbind(top_combined, rest_of_combined)%>%select(all_of(target_cols))
  # model
  set.seed(123)
  model <- glm(type ~ ., data = test_data, family = binomial)
  model_summary <- summary(model)

  # This is to get odds ratio and confidence interval
  # exp(cbind(OR = coef(model), confint(model)))

  imp <- varImp(model)
  imp_df <- data.frame(
    Variable = rownames(imp),
    Importance = imp$Overall
  )

  coefs <- coef(model)

  imp_df$Coefficient <- coefs[as.character(imp_df$Variable)]
  imp_df$Direction <- ifelse(imp_df$Coefficient > 0, "Positive", "Negative")

  imp_df <- imp_df[order(imp_df$Importance, decreasing = FALSE), ]
  imp_df$Variable <- factor(imp_df$Variable, levels = imp_df$Variable)

  odds_ratios <- exp(coef(model))
  imp_df$OddsRatio <- odds_ratios[as.character(imp_df$Variable)]

  return(list(
    importance_df = imp_df,
    top_group = top_combined,
    rest_group = rest_of_combined
  ))
}
