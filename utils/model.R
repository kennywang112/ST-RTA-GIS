# This is for SNA analysis

library(caret)
library(rpart)
library(rpart.plot)
# library(dplyr)
# library(stringr)

# this is the combined data from all nodes
get_model_data <- function(
    features, sort_metric, top_k = 30
    ) {

  top_features <- features%>%arrange(desc({{sort_metric}}))%>%head(top_k)

  ## Get the unique indices from grid filter
  unique_indices <- unique(unlist(top_features$points_in_vertex))

  length(unlist(top_features$points_in_vertex))
  ##.get original data indices from grid filter
  top_grid_features <- all_features_grid[unique_indices,]
  rest_of_features <- all_features_grid[-unique_indices,]
  print(dim(top_grid_features)[1] + dim(rest_of_features)[1] == dim(all_features_grid)[1])

  # top_combined <- top_grid_features
  # rest_of_combined <- rest_of_features
  # get original data
  parsed_list <- lapply(top_grid_features$grid_filter, fromJSON)
  ## all of the overlapped data will classified into top group
  top_combined_data_indices <- unique(unlist(parsed_list))

  top_combined <- combined_data[top_combined_data_indices, ]
  rest_of_combined <- combined_data[-top_combined_data_indices, ]
  print(dim(rest_of_combined)[1] + dim(top_combined)[1] == dim(combined_data)[1])

  return(list(top_combined, rest_of_combined))
}

get_hotspot_data <- function(
    features
    ) {

  hotspot_features <- features %>% filter(hotspot > 0.2)
  unique_indices <- unique(unlist(hotspot_features$points_in_vertex))
  hotspot_grid_data <- all_features_grid[unique_indices, ]
  non_hotspot_grid_data <- all_features_grid[-unique_indices, ]

  return(list(hotspot_grid_data, non_hotspot_grid_data))
}

# this is to get the grid data from all nodes
get_model_grid <- function(
    features, sort_metric, top_k = 30
    ) {

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

  return(list(top_combined, rest_of_combined))
}

glm_report <- function(fdt) {
  model <- glm(type ~ ., data = fdt, family = binomial)
  summary(model)

  probabilities <- predict(model, newdata = fdt, type = "response")
  predicted_classes <- ifelse(probabilities > 0.5, 1, 0)

  cm <- confusionMatrix(factor(predicted_classes, levels = c(0, 1)),
                        factor(fdt$type, levels = c(0, 1)))
  acc_value <- cm$overall['Accuracy']
  imp <- varImp(model)
  imp_df <- data.frame(
    Variable = rownames(imp),
    Importance = imp$Overall
  )

  coefs <- coef(model)
  match_idx <- match(imp_df$Variable, names(coefs))
  imp_df$Coefficient <- coefs[match_idx]
  imp_df$Direction <- ifelse(imp_df$Coefficient > 0, "Betweenness", "Eigen")
  imp_df <- imp_df[order(imp_df$Importance, decreasing = FALSE), ]
  imp_df$Variable <- factor(imp_df$Variable, levels = imp_df$Variable)

  odds_ratios <- exp(coef(model))
  imp_df$OddsRatio <- odds_ratios[as.character(imp_df$Variable)]

  return(list(
    importance_df = imp_df,
    accuracy = acc_value,
    confusion_matrix = cm
  ))
}

model_from_node <- function(
    betweenness_only, eigen_only, target_cols = NULL
    ) {

  set.seed(123)

  betweenness_only$type <- 1
  eigen_only$type <- 0

  n_between <- nrow(betweenness_only)
  n_eigen <- nrow(eigen_only)
  target_size <- min(n_between, n_eigen)

  balanced_data <- bind_rows(
    betweenness_only %>% sample_n(target_size),
    eigen_only %>% sample_n(target_size)
  )

  fdt_filled <- balanced_data %>%
    mutate(across(where(is.character), ~replace_na(., "空值")),
           across(
             .cols = where(is.numeric),
             .fns  = ~ replace_na(., mean(., na.rm = TRUE))
           ))
  # write.csv(fdt_filled,"./fdt_filled.csv", row.names = FALSE)

  train_data <- fdt_filled %>% select(all_of(target_cols), type)

  names(train_data) <- make.names(names(train_data))
  train_data <- train_data %>%
    mutate(across(where(is.character), ~replace_na(., "空值")),
           across(
             .cols = where(is.numeric),
             .fns  = ~ replace_na(., mean(., na.rm = TRUE))
           ))

  result <- glm_report(train_data)

  return(result)
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



# This is used in SNA.qmd to get the grid data
get_comparison_data <- function(labeled_features, raw_grid_data) {

  green_nodes <- labeled_features %>%
    filter(final_category == "Hot_with_Safe_Neighbors_highbetweenness")
  green_indices <- unique(unlist(green_nodes$points_in_vertex))

  orange_nodes <- labeled_features %>%
    filter(final_category == "Cold_with_Hot_Neighbors_highbetweenness")
  orange_indices <- unique(unlist(orange_nodes$points_in_vertex))

  common_indices <- intersect(green_indices, orange_indices)
  if (length(common_indices) > 0) {
    message(sprintf("偵測到 %d 筆重疊資料點同時屬於 Broken 和 Resilient", length(common_indices)))
  } else {
    message("組資料完全獨立無重疊")
  }

  green_indices_clean <- setdiff(green_indices, common_indices)
  orange_indices_clean <- setdiff(orange_indices, common_indices)

  green_grid_data <- raw_grid_data[green_indices_clean, ] %>%
    mutate(group_label = "Broken_Bridge")

  orange_grid_data <- raw_grid_data[orange_indices_clean, ] %>%
    mutate(group_label = "Resilient_Bridge")

  comparison_data <- bind_rows(green_grid_data, orange_grid_data)

  return(comparison_data)
}


# This is used in SNA.qmd to get the original combined data but now it's not in use
get_comparison_combined <- function(labeled_features, all_features_grid, combined_data) {

  extract_raw_group <- function(target_category, label_name) {

    target_nodes <- labeled_features %>%
      filter(final_category == target_category)
    grid_indices <- unique(unlist(target_nodes$points_in_vertex))

    if (length(grid_indices) == 0) return(NULL)

    target_grid_features <- all_features_grid[grid_indices, ]
    parsed_list <- lapply(target_grid_features$grid_filter, fromJSON)
    raw_data_indices <- unique(unlist(parsed_list))
    raw_data <- combined_data[raw_data_indices, ] %>%
      mutate(group_label = label_name)

    return(raw_data)
  }

  green_raw_combined <- extract_raw_group(
    "Hot_with_Safe_Neighbors_highbetweenness",
    "Broken_Bridge"
  )
  orange_raw_combined <- extract_raw_group(
    "Cold_with_Hot_Neighbors_highbetweenness",
    "Resilient_Bridge"
  )

  comparison_data <- bind_rows(green_raw_combined, orange_raw_combined)

  return(comparison_data)
}

# final_raw_data <- get_comparison_combined(df, all_features_grid, combined_data)
