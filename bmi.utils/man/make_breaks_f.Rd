% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{make_breaks_f}
\alias{make_breaks_f}
\title{Create sensible cohort breaks}
\usage{
make_breaks_f(age_a, age_span_a, data_a)
}
\arguments{
\item{age_a}{the age for which projection is required.}

\item{age_span_a}{the desired size of birth cohorts returned.}

\item{data_a}{data.table of individuals studied.}
}
\value{
A list of 4 elements: 
\code{loc} (last observed cohort); 
\code{fno_pos} position of the first non observed cohort (in \code{beg} and \code{end});
\code{beg} : vector of  cohorts' starting years of birth;
\code{end} : vector of  cohorts' ending years of birth.
}
\description{
Avoid mixing cohorts already observed at \code{age_a} and cohorts for 
which the estimate of obesity at \code{age_a} given is a true prediction
}
