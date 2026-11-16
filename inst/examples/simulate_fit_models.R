# Simulate panel and nested multilevel data, then fit memlkit models.
# Install the optional engine packages before running this script.

suppressPackageStartupMessages(library(memlkit))

panel_split <- simulate_panel_data(train_prop = 2 / 3)
panel_data <- rbind(panel_split$train, panel_split$test)
multilevel_data <- simulate_multilevel_data(
  random_slope = seq(-0.3, 0.3, length.out = 8L)
)
write.csv(panel_data, "panel_simulated.csv", row.names = FALSE)
write.csv(multilevel_data, "multilevel_nested_simulated.csv", row.names = FALSE)

# Hold out later waves while retaining the same subjects for panel prediction.
panel_train <- panel_split$train
panel_test <- panel_split$test

set.seed(101)
panel_gbm <- fit_panel_model(
  y ~ time + x1 + x2,
  data = panel_train,
  id = "id",
  time = "time",
  algorithm = "gbm",
  task = "regression",
  random_slope = "time",
  nrounds = 100L,
  verbose = FALSE
)

set.seed(102)
panel_rf <- fit_panel_model(
  y ~ time + x1 + x2,
  data = panel_train,
  id = "id",
  time = "time",
  algorithm = "random_forest",
  task = "regression",
  random_slope = "time",
  nrounds = 100L,
  mtry = 2L
)

set.seed(103)
panel_rf_class <- fit_panel_model(
  status ~ time + x1 + x2,
  data = panel_train,
  id = "id",
  time = "time",
  algorithm = "random_forest",
  task = "classification",
  random_slope = "time",
  nrounds = 100L
)

panel_predictions <- data.frame(
  id = panel_test$id,
  time = panel_test$time,
  observed = panel_test$y,
  megb = predict_panel_model(panel_gbm, panel_test),
  merf = predict_panel_model(panel_rf, panel_test),
  observed_class = panel_test$status,
  rf_class = predict_panel_model(panel_rf_class, panel_test, type = "class"),
  event_probability = predict_panel_model(panel_rf_class, panel_test, type = "response")
)
print(head(panel_predictions))
print(random_effects(panel_gbm))
print(variable_importance(panel_gbm))
print(variable_importance(panel_rf))
print(variable_importance(panel_rf_class))

# Split within each district so every nested group is represented in both sets.
group_vars <- c("region", "district_id")
multilevel_split <- split_multilevel_data(
  multilevel_data,
  train_prop = 0.8,
  seed = 104,
  group = group_vars,
  strategy = "within_group"
)
multilevel_train <- multilevel_split$train
multilevel_test <- multilevel_split$test

set.seed(201)
multilevel_rf <- fit_multilevel_model(
  y ~ x1 + x2,
  data = multilevel_train,
  group = group_vars,
  algorithm = "random_forest",
  task = "regression",
  random_slope = "x1",
  nrounds = 100L,
  MaxIterations = 8L
)

set.seed(202)
multilevel_gbm <- fit_multilevel_model(
  y ~ x1 + x2,
  data = multilevel_train,
  group = group_vars,
  algorithm = "gbm",
  task = "regression",
  random_slope = "x1",
  nrounds = 100L,
  max_iter = 4L,
  tol = 1e-3
)

set.seed(203)
multilevel_rf_class <- fit_multilevel_model(
  status ~ x1 + x2,
  data = multilevel_train,
  group = group_vars,
  algorithm = "random_forest",
  task = "classification",
  nrounds = 100L
)

set.seed(204)
multilevel_gbm_class <- fit_multilevel_model(
  status ~ x1 + x2,
  data = multilevel_train,
  group = group_vars,
  algorithm = "gbm",
  task = "classification",
  nrounds = 100L
)

multilevel_predictions <- data.frame(
  observed = multilevel_test$y,
  merfranger = predict_panel_model(multilevel_rf, multilevel_test),
  lightgbm_mixed = predict_panel_model(multilevel_gbm, multilevel_test),
  observed_class = multilevel_test$status,
  rf_class = predict_panel_model(multilevel_rf_class, multilevel_test, type = "class"),
  lgb_class = predict_panel_model(multilevel_gbm_class, multilevel_test, type = "class"),
  rf_event_probability = predict_panel_model(multilevel_rf_class, multilevel_test, type = "response"),
  lgb_event_probability = predict_panel_model(multilevel_gbm_class, multilevel_test, type = "response")
)
print(head(multilevel_predictions))
print(random_effects(multilevel_rf))
print(random_effects(multilevel_gbm))
print(variable_importance(multilevel_rf))
print(variable_importance(multilevel_gbm))
print(variable_importance(multilevel_rf_class))
print(variable_importance(multilevel_gbm_class))

panel_regression_comparison <- compare_model_fit(
  list(MEGB = panel_gbm, MERF = panel_rf),
  newdata = panel_test
)
panel_classification_comparison <- compare_model_fit(
  list(RandomForest = panel_rf_class),
  newdata = panel_test
)
print(panel_regression_comparison)
print(panel_classification_comparison)
print(panel_classification_comparison$confusion_matrices)

multilevel_regression_comparison <- compare_model_fit(
  list(MERFranger = multilevel_rf, LightGBM = multilevel_gbm),
  newdata = multilevel_test
)
multilevel_classification_comparison <- compare_model_fit(
  list(RandomForest = multilevel_rf_class, LightGBM = multilevel_gbm_class),
  newdata = multilevel_test
)
print(multilevel_regression_comparison)
print(multilevel_classification_comparison)
print(multilevel_classification_comparison$confusion_matrices)

# PDP examples with LOESS smoothing and 95% confidence intervals.
panel_pdp <- plot_pdp(panel_gbm, "x1", show_smooth = TRUE, show_rug = TRUE)
multilevel_pdp <- plot_pdp(multilevel_gbm, "x1", show_smooth = TRUE, show_rug = TRUE)
print(panel_pdp)
print(multilevel_pdp)
