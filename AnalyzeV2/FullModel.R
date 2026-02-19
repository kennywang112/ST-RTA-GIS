us_data_for_tree$Group_Label%>%unique()

exclude_cols <- c("hotspot", "group_label", "grid_id", "bn_feature", "lr_feature", "no_lane")

us_data_cleaned <- us_data_for_tree %>%
  filter(Group_Label %in% c("Broken_Neighbors", "Resilient_Neighbors", "Broken", "Resilient")) %>%
  ungroup() %>%
  mutate(Group_Label = case_when(
    Group_Label == "Broken_Neighbors" ~ "Resilient",
    Group_Label == "Resilient_Neighbors" ~ "Broken",
    TRUE ~ Group_Label
  ))%>%
  distinct()

categorize_feature <- function(x) {
  case_when(
    x >= 0.8 ~ "Dominant",
    x <= 0.2 ~ "Minor",
    TRUE ~ "Mixed"
  )
}

us_data_cleaned <- us_data_cleaned %>%
  mutate(across(
    .cols = where(is.numeric) & !any_of(exclude_cols),
    .fns = function(x) {
      if (max(x, na.rm = TRUE) <= 1 && min(x, na.rm = TRUE) >= 0) {
        return(categorize_feature(x))
      } else {
        return(x)
      }
    }
  )) %>%
  mutate(across(where(is.character), as.factor))%>%
  select(-c('lr_feature', 'hotspot', 'bn_feature', 'no_lane'))

us_data_cleaned$Group_Label%>%table()

tree_bad_model <- rpart(Group_Label ~ . ,
                        data = us_data_cleaned,
                        method = "class",
                        maxdepth = 5,
                        control = rpart.control(cp = 0.005, minbucket = 50),
                        parms = list(prior = c(0.5, 0.5))
)
pred_class <- predict(tree_bad_model, newdata = us_data_cleaned, type = "class")
conf_matrix <- confusionMatrix(data = pred_class, reference = as.factor(us_data_cleaned$Group_Label))

print(conf_matrix)

png("./Layouts/tree_bad_model_full.png", width = 1200, height = 800)
# rpart.plot(tree_bad_model, family = "PingFang", tweak = 1.5)
rpart.plot(tree_bad_model,
           type = 4,
           extra = 106,
           under = TRUE,
           fallen.leaves = FALSE,
           box.palette = "BuGn",
           # shadow.col = "gray",
           nn = TRUE,
           tweak = 1,
           # varlen = 0,
           # faclen = 0,
)
dev.off()
