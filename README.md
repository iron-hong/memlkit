# memlkit

memlkit provides separate interfaces for panel data and cross-sectional multilevel data. It standardizes fitting, prediction, random-effect extraction, variable importance, and partial dependence plots across mixed-effects tree models.

## Supported model combinations

| Data and task | Random forest | GBM |
|---|---|---|
| Panel regression | LongituRF `MERF` (or legacy `REEMforest` / `MERFranger`) | MEGB |
| Panel classification | ranger probability forest with a grouped logistic mixed-model calibration | Not supported by MEGB |
| Cross-sectional multilevel regression | SAEforest `MERFranger` | LightGBM iteratively alternated with a grouped random-effects model |
| Cross-sectional multilevel classification | ranger one-vs-rest probability forests with grouped logistic mixed-model calibration | LightGBM one-vs-rest models with grouped logistic mixed-model calibration |

MEGB is a longitudinal regression method. The classification workflows are two-stage approximations: a forest or LightGBM model estimates the initial probability, then a grouped logistic mixed model calibrates it. They are not joint generalized MERF estimators. The LightGBM regression extension alternates LightGBM fixed-effect fits with an `lmer` random-effects update, and should be viewed as a model extension rather than the original SAEforest estimator.

## Installation

Install the model engines you plan to use, then install `memlkit` from CRAN:

```r
install.packages(c("MEGB", "LongituRF", "SAEforest", "lightgbm", "ranger", "lme4", "pdp", "ggplot2"))
install.packages("memlkit")
```

## Panel data

Panel data must be long format, with one row per subject-time observation. The default random slope is the measurement time.

```r
library(memlkit)

panel_gbm <- fit_panel_model(
  Y ~ time + Var1 + Var2 + Var3 + Var4,
  data = panel_data,
  id = "id",
  time = "time",
  algorithm = "gbm",
  task = "regression",
  nrounds = 500
)

panel_rf <- fit_panel_model(
  Y ~ time + Var1 + Var2 + Var3 + Var4,
  data = panel_data,
  id = "id",
  time = "time",
  algorithm = "random_forest",
  task = "regression",
  nrounds = 500
)

panel_rf_intercept_only <- fit_panel_model(
  Y ~ time + Var1 + Var2 + Var3 + Var4,
  data = panel_data,
  id = "id",
  time = "time",
  algorithm = "random_forest",
  task = "regression",
  include_time_slope = FALSE,
  nrounds = 500
)

pred <- predict_panel_model(panel_gbm, panel_test)
compare_model_fit(
  list(MEGB = panel_gbm, MERF = panel_rf),
  newdata = panel_test
)
random_effects(panel_gbm)
variable_importance(panel_gbm)
plot_pdp(panel_gbm, "Var1", show_smooth = TRUE, show_rug = TRUE)
```

Set `task = "classification"` with `algorithm = "random_forest"` for the two-stage ranger/GLMM classifier. `MEGB` accepts regression outcomes; a classification request with `algorithm = "gbm"` returns an informative error.

The earlier `fit_panel_forest()` API remains available for `MEGB`, `MERF`, `REEMforest`, and `MERFranger` regression fits.

## Cross-sectional multilevel data

Use one or more grouping columns to add group-level random intercepts. Multiple columns are treated as separate (crossed) grouping terms; for nested levels, create unique nested IDs first. A requested random slope is assigned to the first grouping level.

```r
survey_data$district_nested <- interaction(
  survey_data$region, survey_data$district, drop = TRUE
)

multi_rf <- fit_multilevel_model(
  outcome ~ age + education + income,
  data = survey_data,
  group = c("region", "district_nested"),
  algorithm = "random_forest",
  task = "regression",
  random_slope = "age",
  correlated_random_effects = TRUE
)

multi_gbm <- fit_multilevel_model(
  outcome ~ age + education + income,
  data = survey_data,
  group = c("region", "district_nested"),
  algorithm = "gbm",
  task = "regression",
  random_slope = "age",
  nrounds = 300
)

predict_panel_model(multi_gbm, survey_test)
compare_model_fit(
  list(MERFranger = multi_rf, LightGBM = multi_gbm),
  newdata = survey_test
)
random_effects(multi_gbm)
variable_importance(multi_gbm)
plot_pdp(multi_gbm, "age", show_smooth = TRUE, show_rug = TRUE)
```

Classification accepts factor, character, logical, or numeric outcomes with at most 20 classes. For binary models, `type = "response"` returns the probability of the second outcome class; use `type = "prob"` for all class probabilities. Prediction data must include all grouping columns and all random-slope variables.

`compare_model_fit()` evaluates one memlkit fit or compares a named list of fits on validation data. Regression results include MAE, RMSE, R-squared, and mean error. Classification results include accuracy, precision, recall, F1, and one confusion matrix per model. Binary precision, recall, and F1 use the last modeled class as the default positive class; set `positive_class` to choose another. Multiclass precision, recall, and F1 are macro-averaged.

For multilevel models, `correlated_random_effects = FALSE` (the default) uses `||` for independent first-level random intercepts and slopes; set it to `TRUE` to estimate their correlations. For panel models, `include_time_slope = TRUE` (the default) keeps the time random slope; set it to `FALSE` for a random-intercept-only model.

## Interpretation helpers

```r
variable_importance(fit)
random_effects(fit)
predict_panel_model(fit, newdata, type = "response")
predict_panel_model(fit, newdata, type = "class")
predict_panel_model(fit, newdata, type = "prob")
plot_pdp(fit, "x1", show_raw = FALSE, show_smooth = TRUE,
         smooth_span = 0.8, show_rug = TRUE)
plot_pdp(fit, "x1", show_raw = TRUE)
plot_pdp(fit, "x1", show_raw = FALSE, class = "target")
```

## Simulation functions

Generate sample data directly from memlkit. The nested data contain a `region` factor and a unique `district_id` for districts nested within regions.

```r
panel_data <- simulate_panel_data(n_subjects = 40, n_times = 6, seed = 1)
panel_split <- simulate_panel_data(
  n_subjects = 40, n_times = 6, seed = 1, train_prop = 0.8
)
panel_train <- panel_split$train
panel_test <- panel_split$test

nested_data <- simulate_multilevel_data(
  n_regions = 8,
  n_districts = 4,
  n_units_per_district = 10,
  random_slope = seq(-0.3, 0.3, length.out = 8),
  seed = 2
)
multilevel_split <- simulate_multilevel_data(
  n_regions = 8, n_districts = 4, n_units_per_district = 10,
  seed = 2, train_prop = 0.8
)
multilevel_train <- multilevel_split$train
multilevel_test <- multilevel_split$test

# Split an existing panel data set at the latest waves.
split_panel_data(panel_data, train_prop = 0.8, strategy = "time")

# Keep every region/district combination represented in both sets.
split_multilevel_data(nested_data, train_prop = 0.8,
                      group = c("region", "district_id"),
                      strategy = "within_group")

fit <- fit_multilevel_model(
  y ~ x1 + x2,
  data = nested_data,
  group = c("region", "district_id"),
  algorithm = "gbm",
  task = "regression",
  random_slope = "x1"
)
```

`simulate_nested_data()` is an alias for `simulate_multilevel_data()`.

When `train_prop` is supplied to either simulation function, it returns a
`memlkit_data_split` object with `$train`, `$test`, row indices, and requested and
realized training proportions. Panel data defaults to a time-ordered split
that holds out later waves. Multilevel data defaults to a within-group split.
Use `split_strategy = "subject"` for panel simulations or
`split_strategy = "group"` for multilevel simulations to hold out whole
subjects or group combinations, respectively. Set `split_strategy = "random"`
for row-level random splitting. The standalone split helpers use the argument
name `strategy`. Whole-wave and group-preserving splits may differ from the
requested proportion because their units cannot be divided.

## End-to-end simulated example

The package source includes a script that generates balanced panel data and region/district nested data, fits the supported regression and classification models, produces predictions and test metrics, extracts random effects and importance, and plots partial dependence curves:

```r
source(system.file("examples", "simulate_fit_models.R", package = "memlkit"))
```

## Method references

Method and software references used by memlkit are listed in
[`inst/REFERENCES.bib`](inst/REFERENCES.bib). Use `citation("memlkit")` to
cite the package itself.
