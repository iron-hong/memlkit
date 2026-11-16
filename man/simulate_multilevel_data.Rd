\name{simulate_multilevel_data}
\alias{simulate_multilevel_data}
\alias{simulate_nested_data}
\title{Simulate Nested Multilevel Data for memlkit}
\usage{
simulate_multilevel_data(n_regions = 8L, n_districts = 4L,
  n_units_per_district = 10L, sd_region = 0.65,
  sd_district = 0.45, sd_error = 0.8, seed = 20261118L,
  random_slope = NULL, train_prop = NULL,
  split_strategy = c("within_group", "group", "random"),
  split_group = c("region", "district_id"), split_seed = NULL)

simulate_nested_data(n_regions = 8L, n_districts = 4L,
  n_units_per_district = 10L, sd_region = 0.65,
  sd_district = 0.45, sd_error = 0.8, seed = 20261118L,
  random_slope = NULL, train_prop = NULL,
  split_strategy = c("within_group", "group", "random"),
  split_group = c("region", "district_id"), split_seed = NULL)
}
\arguments{
\item{n_regions}{Number of top-level regions; at least 2.}
\item{n_districts}{Number of districts per region; at least 2.}
\item{n_units_per_district}{Observations per district; at least 2.}
\item{sd_region}{Standard deviation of region random intercepts.}
\item{sd_district}{Standard deviation of district random intercepts.}
\item{random_slope}{Optional numeric vector of slopes for \code{x1} at the first grouping level (region). Provide one finite value per region; \code{NULL} omits region-specific slopes.}
\item{sd_error}{Standard deviation of continuous-outcome errors.}
\item{seed}{Optional integer seed.}
\item{train_prop}{Optional training-data proportion. If supplied, return a train/test split instead of the complete data frame.}
\item{split_strategy}{Multilevel split strategy passed to \code{split_multilevel_data()}.}
\item{split_group}{Grouping columns used when splitting the simulated data.}
\item{split_seed}{Optional seed for splitting. Defaults to the generator's continuing random-number stream.}
}
\value{A cross-sectional data frame with region, district, unique nested district ID, predictors, a continuous outcome, and a binary factor outcome; if \code{train_prop} is supplied, a \code{memlkit_data_split} list is returned.}
\description{Simulates data with district units nested within regions and random intercepts at both levels. An optional region-specific slope for \code{x1} can also be supplied. \code{simulate_nested_data()} is an alias.}
\seealso{\code{fit_multilevel_model}, \code{simulate_panel_data}}
