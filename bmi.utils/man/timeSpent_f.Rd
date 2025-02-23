% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{timeSpent_f}
\alias{timeSpent_f}
\title{Compute time spent obese in sample restricted to \code{seqn_a}}
\usage{
timeSpent_f(seqn_a, timeSpentObese_a)
}
\arguments{
\item{seqn_a}{ids of individuals for which proportion should be computed.}

\item{timeSpentObese_a}{matrix of times spent obese (returned by \code{\link{tso_f}}).}
}
\value{
Vector of times spent obese in each posterior sample.
}
\description{
Compute time spent obese in sample restricted to \code{seqn_a}
}
