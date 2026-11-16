\name{plot_convergence}
\alias{plot_convergence}
\title{Plot Model Convergence}
\usage{plot_convergence(object, measure = c("OOB", "LL"))}
\arguments{
\item{object}{A fitted \code{memlkit_model} object.}
\item{measure}{Out-of-bag error or log-likelihood.}
}
\value{A base R plot.}
\description{Plots an available convergence diagnostic.}
