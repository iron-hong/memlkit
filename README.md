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

```r
install.packages("remotes")
remotes::install_github("iron-hong/memlkit")
```

Install the optional model engines required by the workflows you plan to run. To run all examples below, install:

```r
install.packages(c(
  "MEGB", "LongituRF", "SAEforest", "randomForest", "ranger",
  "pdp", "ggplot2", "lme4", "lightgbm", "gbm"
))
```

```r
library(memlkit)
library(lightgbm)
```

## Cross-sectional nested multilevel data

The simulated data have three levels: individuals within districts within regions. `district_id` uniquely identifies districts nested within regions. The random slopes for `x1` and `x2` are assigned to the first grouping level (`region`). Setting `correlated_random_effects = FALSE` fits independent random intercepts and slopes.

```r
nested_split <- simulate_multilevel_data(
  train_prop = 0.8,
  split_strategy = "random",
  n_regions = 30,
  n_districts = 4,
  n_units_per_district = 10,
  seed = 2
)

nested_train <- nested_split$train
nested_test <- nested_split$test

multilevel_gbm_fit <- fit_multilevel_model(
  y ~ x1 + x2,
  data = nested_train,
  group = c("region", "district_id"),
  algorithm = "gbm",
  task = "regression",
  correlated_random_effects = FALSE,
  random_slope = c("x1", "x2")
)

multilevel_rf_fit <- fit_multilevel_model(
  y ~ x1 + x2,
  data = nested_train,
  group = c("region", "district_id"),
  algorithm = "random_forest",
  task = "regression",
  correlated_random_effects = FALSE,
  random_slope = c("x1", "x2")
)

multilevel_comparison <- compare_model_fit(
  list(MERF = multilevel_rf_fit, MEGBM = multilevel_gbm_fit),
  newdata = nested_test
)
multilevel_comparison$metrics

variable_importance(multilevel_rf_fit)
plot_pdp(multilevel_rf_fit, "x1", show_smooth = TRUE, show_rug = TRUE)
```

## Panel data

The panel simulator returns long-format data with a subject identifier (`id`), measurement time (`time`), predictors (`x1`, `x2`), and a continuous outcome (`y`). This example uses a random row split and fits random-intercept-only models by setting `include_time_slope = FALSE`.

```r
panel_split <- simulate_panel_data(
  train_prop = 0.8,
  split_strategy = "random",
  n_subjects = 40,
  n_times = 6,
  seed = 1
)

panel_train <- panel_split$train
panel_test <- panel_split$test

panel_rf_fit <- fit_panel_model(
  y ~ x1 + x2,
  data = panel_train,
  id = "id",
  time = "time",
  algorithm = "random_forest",
  include_time_slope = FALSE,
  task = "regression"
)

panel_gbm_fit <- fit_panel_model(
  y ~ x1 + x2,
  data = panel_train,
  id = "id",
  time = "time",
  algorithm = "gbm",
  include_time_slope = FALSE,
  task = "regression"
)

panel_comparison <- compare_model_fit(
  list(MERF = panel_rf_fit, MEGBM = panel_gbm_fit),
  newdata = panel_test
)
panel_comparison$metrics

variable_importance(panel_rf_fit)
plot_pdp(panel_rf_fit, "x1", show_smooth = TRUE, show_rug = TRUE)
```

For panel regression, `algorithm = "random_forest"` uses the LongituRF mixed-effects random forest workflow, while `algorithm = "gbm"` uses MEGB. Panel classification is supported with `algorithm = "random_forest"`; MEGB supports regression only.

## Common helpers

```r
random_effects(panel_rf_fit)
variable_importance(panel_rf_fit)
predict_panel_model(panel_rf_fit, newdata = panel_test)
plot_pdp(panel_rf_fit, "x1", show_smooth = TRUE, show_rug = TRUE)
```

`compare_model_fit()` reports MAE, RMSE, R-squared, and mean error for regression. For classification, it reports classification metrics and confusion matrices. `plot_pdp()` displays a smoothed trend with a 95% confidence interval by default; use `show_raw = TRUE` to add the unsmoothed partial-dependence curve and `show_rug = TRUE` to show the predictor distribution.

For multilevel models, `random_slope` applies to the first grouping level. Set `correlated_random_effects = TRUE` to estimate correlations between its random intercept and slopes. For panel models, `include_time_slope = TRUE` (the default) includes a subject-specific time slope; set it to `FALSE` for a random-intercept-only model.

## Method references

Method and software references used by memlkit are listed in
[`inst/REFERENCES.bib`](inst/REFERENCES.bib). Use `citation("memlkit")` to
cite the package itself.
