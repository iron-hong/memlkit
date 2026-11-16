\name{predict_panel_model}
\alias{predict_panel_model}
\title{Predict from a memlkit Model}
\usage{
predict_panel_model(object, newdata, type = c("response", "class", "prob"), ...)
}
\arguments{
\item{object}{A fitted model from \code{fit_panel_model()} or \code{fit_multilevel_model()}.}
\item{newdata}{Data frame with predictors, grouping variables, and random-slope variables.}
\item{type}{Return response predictions, predicted classes, or class probabilities.}
\item{...}{Reserved for future use.}
}
\value{Numeric predictions, a factor of predicted classes, or a probability matrix.}
\description{Provides a common prediction interface across the supported panel and cross-sectional mixed-effects tree models.}
