\name{fit_panel_model}
\alias{fit_panel_model}
\title{Fit a Mixed-Effects Random Forest or GBM to Panel Data}
\usage{
fit_panel_model(formula, data, id, time,
  algorithm = c("random_forest", "gbm"),
  task = c("regression", "classification"),
  random_slope = time, nrounds = 500L, engine = NULL,
  include_time_slope = TRUE, ...)
}
\arguments{
\item{formula}{Two-sided formula for the outcome and fixed-effect predictors.}
\item{data}{Long-format data with one row per subject-time observation.}
\item{id}{Subject identifier column name.}
\item{time}{Measurement-time column name.}
\item{algorithm}{\code{"random_forest"} or \code{"gbm"}.}
\item{task}{\code{"regression"} or \code{"classification"}. MEGB supports regression only.}
\item{random_slope}{Numeric subject-specific random-slope variables; defaults to the time column.}
\item{include_time_slope}{If \code{FALSE}, use random intercepts only. If \code{TRUE}, use \code{random_slope} (time by default).}
\item{nrounds}{Number of trees passed to the selected engine.}
\item{engine}{Optional engine override: MEGB, MERF, REEMforest, or MERFranger.}
\item{...}{Additional arguments passed to the selected engine.}
}
\value{A fitted model for use with prediction, random-effect, importance, and PDP helpers.}
\description{Uses MEGB for panel gradient-boosting regression and LongituRF MERF for panel random-forest regression. Random-forest classification uses a two-stage ranger and grouped logistic mixed-model workflow.}
\seealso{\code{fit_panel_forest}, \code{fit_multilevel_model}, \code{predict_panel_model}}
