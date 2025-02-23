% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preparation_weightHistory.R
\name{add_bias25_f}
\alias{add_bias25_f}
\title{Create deb2 bmis (bmis with cohort-level corrections) for bmi at 25}
\usage{
add_bias25_f(d_a, by_sex_a = T, G25list_a)
}
\arguments{
\item{d_a}{data.table}

\item{by_sex_a}{whether the bias is sex-specific (\code{TRUE}) or not (\code{FALSE})}

\item{G25list_a}{list of models to use to compute bias}
}
\value{
0 for correct execution
}
\description{
Create deb2 bmis (bmis with cohort-level corrections) for bmi at 25
}
