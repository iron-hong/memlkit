\name{fit_multilevel_model}
\alias{fit_multilevel_model}
\title{Fit Mixed-Effects Forest or LightGBM Models to Multilevel Data}
\usage{
fit_multilevel_model(formula, data, group,
  algorithm = c("random_forest", "gbm"),
  task = c("regression", "classification"), random_slope = NULL,
  nrounds = 500L, params = list(), max_iter = 10L, tol = 1e-4,
  correlated_random_effects = FALSE, ...)
}
\arguments{
\item{formula}{Two-sided formula for the outcome and fixed-effect predictors.}
\item{data}{Cross-sectional data, one row per unit.}
\item{group}{One or more grouping-column names for nested or crossed levels.}
\item{algorithm}{\code{"random_forest"} or \code{"gbm"}.}
\item{task}{\code{"regression"} or \code{"classification"}.}
\item{random_slope}{Optional names of numeric predictor columns with random slopes at the first grouping level.}
\item{correlated_random_effects}{If \code{TRUE}, estimate correlations among first-level random intercepts and slopes. If \code{FALSE}, constrain those correlations to zero.}
\item{nrounds}{Number of trees or boosting rounds.}
\item{params}{Additional LightGBM parameters.}
\item{max_iter}{Maximum number of alternating LightGBM and random-effect updates for regression.}
\item{tol}{Convergence tolerance for changes in estimated random effects.}
\item{...}{Additional arguments passed to SAEforest, ranger, or LightGBM.}
}
\value{A fitted model for use with prediction, random-effect, importance, and PDP helpers.}
\description{Uses SAEforest MERFranger for random-forest regression. The LightGBM regression extension alternates LightGBM fits with group random-effect updates. Classification uses one-vs-rest tree models with grouped logistic mixed-model calibration.}
\seealso{\code{fit_panel_model}, \code{predict_panel_model}}
