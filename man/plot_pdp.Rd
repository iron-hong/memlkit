\name{plot_pdp}
\alias{plot_pdp}
\title{Plot Partial Dependence with LOESS Smoothing}
\usage{plot_pdp(object, variable, class = NULL,
  rug = NULL, smooth = NULL, show_original = NULL,
  show_raw = FALSE, raw_alpha = 0.35, raw_color = "#2166AC",
  show_smooth = TRUE, smooth_span = 0.8, smooth_color = "#B2182B",
  smooth_linewidth = 1.1, show_rug = TRUE, ...)}
\arguments{
\item{object}{A fitted memlkit model.}
\item{variable}{Predictor name to display.}
\item{class}{Outcome class to plot for classification models.}
\item{rug}{Legacy alias for \code{show_rug}.}
\item{smooth}{Legacy alias for \code{show_smooth}.}
\item{show_original}{Legacy alias for \code{show_raw}.}
\item{show_raw}{Whether to show the unsmoothed PDP line or categorical bars.}
\item{raw_alpha}{Transparency of the raw PDP line or bars, from 0 to 1.}
\item{raw_color}{Color of the raw PDP line or bars.}
\item{show_smooth}{Whether to add a LOESS-smoothed curve and 95\% confidence interval for numeric predictors.}
\item{smooth_span}{LOESS span; larger values produce smoother curves.}
\item{smooth_color}{Color of the smoothed curve.}
\item{smooth_linewidth}{Width of the smoothed curve.}
\item{show_rug}{Whether to add a rug showing the training-data distribution for numeric predictors.}
\item{...}{Additional arguments passed to \code{pdp::partial} for panel models.}
}
\value{A ggplot object.}
\description{Calculates and plots partial dependence curves using the common prediction interface. Numeric predictors show a LOESS-smoothed curve with a 95\% confidence interval and a rug by default. Use \code{show_raw = TRUE} to add the unsmoothed PDP line.}
