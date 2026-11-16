\name{fit_panel_forest}
\alias{fit_panel_forest}
\title{Fit a Mixed-Effects Forest Model}
\usage{
fit_panel_forest(formula, data, id, time = NULL,
  model = c("MEGB", "MERF", "REEMforest", "MERFranger"),
  random_slope = NULL, random = NULL,
  importance = "permutation", ...)
}
\arguments{
\item{formula}{A response and fixed-effect formula, for example \code{y ~ x1 + x2}.}
\item{data}{Long-format data frame.}
\item{id}{Name of the subject or cluster identifier column.}
\item{time}{Optional measurement-time column; required for MEGB, MERF, and REEMforest.}
\item{model}{Model engine: MEGB, MERF, REEMforest, or MERFranger.}
\item{random_slope}{Optional numeric variables used as random slopes.}
\item{random}{Optional lme4-style random-effects formula for MERFranger.}
\item{importance}{Variable-importance mode for MERFranger.}
\item{...}{Additional arguments passed to the selected engine.}
}
\value{An object of class \code{memlkit_model}.}
\description{Fits a mixed-effects boosting or random forest model to panel or clustered data.}
\seealso{\code{random_effects}, \code{variable_importance}, \code{plot_pdp}, \code{plot_convergence}}
