% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor_q_score.R
\name{ctq}
\alias{ctq}
\title{Calculate Factor Quantile}
\usage{
ctq(values, group, quantiles = 3, .desc = TRUE)
}
\arguments{
\item{values}{a numeric vector of factor values.}

\item{group}{a character vector of groups.}

\item{quantiles}{integer representing the number of quantiles
to split data into.}

\item{desc}{Should values be ranked in secending order?}
}
\value{
company quantile based on factor value
}
\description{
Calculate Factor Quantile
}
