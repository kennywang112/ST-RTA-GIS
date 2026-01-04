get_column_entropy <- function(column_data) {
  valid_data <- na.omit(column_data)
  if (length(valid_data) == 0) return(0)
  probs <- prop.table(table(valid_data))
  # Shannon Entropy: -sum(p * log(p))
  entropy <- -sum(probs * log(probs))

  return(entropy)
}

cols <- c(
  '車道劃分設施-分道設施-快車道或一般車道間名稱',
  '車道劃分設施-分道設施-快慢車道間名稱',
  '車道劃分設施-分道設施-路面邊線名稱',
  '車道劃分設施-分向設施大類別名稱',
  '事故類型及型態大類別名稱',
  '道路型態大類別名稱',
  '號誌-號誌種類名稱',
  '速限-第1當事者',
  '當事者區分-類別-大類別名稱-車種',
  'cause_group'
)

get_column_entropy <- function(column_data) {
  valid_data <- na.omit(column_data)
  if (length(valid_data) == 0) return(0)
  probs <- prop.table(table(valid_data))
  return(-sum(probs * log(probs)))
}

analyze_entropy_trend <- function(
    features_data, sort_metric, target_cols, k_values = c(10, 20, 30, 40, 50)
    ) {
  results_list <- list()

  for (k in k_values) {
    message(paste("Analyzing Top K =", k, "..."))

    bdt <- get_model_data(features_data, {{sort_metric}}, top_k = k)

    top_group <- bdt[[1]]
    entropy_values <- sapply(top_group[, target_cols], get_column_entropy)
    k_result <- data.frame(
      K_Value = k,
      Feature = names(entropy_values),
      Entropy = as.numeric(entropy_values)
    )

    results_list[[length(results_list) + 1]] <- k_result
  }

  final_trend_df <- bind_rows(results_list)
  return(final_trend_df)
}

k_steps <- c(10, 20, 30, 40, 50)
trend_data <- analyze_entropy_trend(filter_features, betweenness, cols, k_values = k_steps)
for (i in trend_data$Feature%>%unique()) {
  plt <- trend_data%>%
    filter(Feature == i)%>%
    ggplot(aes(x = K_Value, y = Entropy, color = Feature, group = Feature)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.box = "vertical")
  print(plt)
}
