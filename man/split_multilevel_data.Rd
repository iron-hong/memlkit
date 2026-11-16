\name{split_multilevel_data}
\alias{split_multilevel_data}
\title{Split Multilevel Data into Training and Test Sets}
\usage{
split_multilevel_data(data, train_prop = 0.8, seed = NULL,
  group = c("region", "district_id"),
  strategy = c("within_group", "group", "random"))
}
\arguments{
\item{data}{Cross-sectional multilevel data.}
\item{train_prop}{Proportion assigned to training data, strictly between 0 and 1.}
\item{seed}{Optional integer seed for random split strategies.}
\item{group}{Grouping-column names. For \code{"within_group"}, rows are sampled within combinations of these groups. For \code{"group"}, entire combinations are held out.}
\item{strategy}{\code{"within_group"}, \code{"group"}, or \code{"random"}.}
}
\value{A \code{memlkit_data_split} list containing \code{train}, \code{test}, row indices, requested and actual training proportions, and the selected strategy.}
\description{Splits multilevel data while preserving cluster representation, holding out entire groups, or randomly sampling rows.}
\seealso{\code{simulate_multilevel_data}, \code{split_panel_data}}
