\name{compare_model_fit}
\alias{compare_model_fit}
\title{Compare Fitted memlkit Models}
\usage{
compare_model_fit(models, newdata, response = NULL, task = NULL,
  positive_class = NULL)
}
\arguments{
\item{models}{A fitted memlkit model or a named list of fitted memlkit models.}
\item{newdata}{Validation data containing the observed outcome and predictors.}
\item{response}{Optional outcome-column name. By default, the response stored in each fitted model is used.}
\item{task}{Optional task override: \code{"regression"} or \code{"classification"}.}
\item{positive_class}{Positive class for binary classification. Defaults to the last modeled class. Multiclass metrics are macro-averaged.}
}
\value{A \code{memlkit_model_comparison} list with a \code{metrics} data frame and, for classification, a named list of confusion matrices. Regression metrics are MAE, RMSE, R-squared, and mean error. Classification metrics are accuracy, precision, recall, and F1.}
\description{Evaluates one memlkit model or compares multiple fitted models using validation data.}
\examples{
\dontrun{
comparison <- compare_model_fit(
  list(random_forest = rf_fit, gbm = gbm_fit),
  newdata = validation_data
)
comparison$metrics
comparison$confusion_matrices
}
}
