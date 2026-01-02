# This is for SNA analysis

library(caret)
library(rpart)
library(rpart.plot)
# library(dplyr)
# library(stringr)

get_model_data <- function(features, sort_metric, top_k = 30) {

  top_features <- features%>%arrange(desc({{sort_metric}}))%>%head(top_k)

  ## Get the unique indices from grid filter
  unique_indices <- unique(unlist(top_features$points_in_vertex))

  length(unlist(top_features$points_in_vertex))
  ##.get original data indices from grid filter
  top_grid_features <- all_features_grid[unique_indices,]
  rest_of_features <- all_features_grid[-unique_indices,]
  print(dim(top_grid_features)[1] + dim(rest_of_features)[1] == dim(all_features_grid)[1])

  top_combined <- top_grid_features
  rest_of_combined <- rest_of_features
  # # get original data
  # library(jsonlite)
  # parsed_list <- lapply(top_grid_features$grid_filter, fromJSON)
  # ## all of the overlapped data will classified into top group
  # top_combined_data_indices <- unique(unlist(parsed_list))
  #
  # top_combined <- combined_data[top_combined_data_indices, ]
  # rest_of_combined <- combined_data[-top_combined_data_indices, ]
  # print(dim(rest_of_combined)[1] + dim(top_combined)[1] == dim(combined_data)[1])

  return(list(top_combined, rest_of_combined))
}

model_from_node <- function(
    top_combined, rest_of_combined, target_cols = NULL
    ) {

  top_combined$type <- 1
  rest_of_combined$type <- 0

  if (is.null(target_cols)) {
    top_data <- top_combined
    rest_data <- rest_of_combined
  } else {
    top_data <- top_combined %>% select(all_of(target_cols), type)
    rest_data <- rest_of_combined %>% select(all_of(target_cols), type)
  }

  n_top <- nrow(top_data)
  set.seed(123)
  rest_data_balanced <- rest_data %>% sample_n(size = n_top)

  train_data <- na.omit(rbind(top_data, rest_data_balanced))

  model <- glm(type ~ ., data = train_data, family = binomial)
  model_summary <- summary(model)

  probabilities <- predict(model, newdata = train_data, type = "response")
  predicted_classes <- ifelse(probabilities > 0.5, 1, 0)

  cm <- confusionMatrix(factor(predicted_classes, levels = c(0, 1)),
                        factor(train_data$type, levels = c(0, 1)))
  acc_value <- cm$overall['Accuracy']

  # This is to get odds ratio and confidence interval
  # exp(cbind(OR = coef(model), confint(model)))

  imp <- varImp(model)
  imp_df <- data.frame(
    Variable = rownames(imp),
    Importance = imp$Overall
  )

  coefs <- coef(model)
  match_idx <- match(rownames(imp_df), names(coefs))
  imp_df$Coefficient <- coefs[match_idx]
  imp_df$Direction <- ifelse(imp_df$Coefficient > 0, "Positive", "Negative")
  imp_df <- imp_df[order(imp_df$Importance, decreasing = FALSE), ]
  imp_df$Variable <- factor(imp_df$Variable, levels = imp_df$Variable)

  odds_ratios <- exp(coef(model))
  imp_df$OddsRatio <- odds_ratios[as.character(imp_df$Variable)]

  return(list(
    importance_df = imp_df,
    accuracy = acc_value,
    confusion_matrix = cm,
    top_group = top_combined,
    rest_group = rest_of_combined
  ))
}

tree_model_from_node <- function(
    top_combined, rest_of_combined, target_cols, cp_value = 0.005
    ) {

  top_combined$type <- "TopGroup"
  rest_of_combined$type <- "Rest"

  tree_data <- rbind(top_combined, rest_of_combined) %>%
    select(all_of(c(target_cols, "type"))) %>%
    mutate(type = factor(type, levels = c("Rest", "TopGroup")))

  # CART Algorithm)
  set.seed(123)
  tree_model <- rpart(type ~ .,
                      data = tree_data,
                      method = "class",
                      parms = list(prior = c(0.5, 0.5)),
                      control = rpart.control(cp = cp_value, minbucket = 2))

  imp_vector <- tree_model$variable.importance

  if (length(imp_vector) > 0) {
    imp_df <- data.frame(Variable = names(imp_vector),
                         Importance = as.numeric(imp_vector))%>%
      arrange(desc(Importance))
  } else {
    imp_df <- data.frame(Variable = character(), Importance = numeric())
    warning("The model did not select any variables for splitting (the tree has only the root node). Please try lowering the cp value.")
  }

  rpart.plot(tree_model, type = 4, under = TRUE,
             extra = 104, # show probability and percentage
             main = "Decision Tree Rules for Top Group", box.palette = "GnBu")

  return(list(model = tree_model, importance_df = imp_df,  data = tree_data))
}
