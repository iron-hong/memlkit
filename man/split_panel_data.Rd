\name{split_panel_data}
\alias{split_panel_data}
\title{Split Panel Data into Training and Test Sets}
\usage{
split_panel_data(data, train_prop = 0.8, seed = NULL, id = "id",
  time = "time", strategy = c("time", "subject", "random"))
}
\arguments{
\item{data}{Long-format panel data.}
\item{train_prop}{Proportion assigned to training data, strictly between 0 and 1.}
\item{seed}{Optional integer seed for random split strategies.}
\item{id}{Subject identifier column.}
\item{time}{Measurement-time column.}
\item{strategy}{\code{"time"} holds out later waves, \code{"subject"} holds out subjects, and \code{"random"} samples rows.}
}
\value{A \code{memlkit_data_split} list containing \code{train}, \code{test}, row indices, requested and actual training proportions, and the selected strategy.}
\description{Splits panel data using a time-ordered, subject-level, or row-level strategy. The time strategy selects a wave boundary closest to the requested proportion.}
\seealso{\code{simulate_panel_data}, \code{split_multilevel_data}}
