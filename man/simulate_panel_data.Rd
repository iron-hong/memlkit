\name{simulate_panel_data}
\alias{simulate_panel_data}
\title{Simulate Panel Data for memlkit}
\usage{
simulate_panel_data(n_subjects = 40L, n_times = 6L,
  sd_intercept = 0.8, sd_slope = 0.25, sd_error = 0.7,
  seed = 20261117L, train_prop = NULL,
  split_strategy = c("time", "subject", "random"), split_seed = NULL)
}
\arguments{
\item{n_subjects}{Number of subjects; at least 2.}
\item{n_times}{Repeated measurements per subject; at least 2.}
\item{sd_intercept}{Standard deviation of subject random intercepts.}
\item{sd_slope}{Standard deviation of subject random time slopes.}
\item{sd_error}{Standard deviation of continuous-outcome errors.}
\item{seed}{Optional integer seed.}
\item{train_prop}{Optional training-data proportion. If supplied, return a train/test split instead of the complete data frame.}
\item{split_strategy}{Panel split strategy passed to \code{split_panel_data()}.}
\item{split_seed}{Optional seed for splitting. Defaults to the generator's continuing random-number stream.}
}
\value{A long-format data frame with identifiers, time, predictors, a continuous outcome, and a binary factor outcome; if \code{train_prop} is supplied, a \code{memlkit_data_split} list is returned.}
\description{Simulates balanced panel data with subject-specific random intercepts and time slopes.}
\seealso{\code{fit_panel_model}, \code{simulate_multilevel_data}}
