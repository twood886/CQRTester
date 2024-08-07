% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor_z_score.R
\name{ctz}
\alias{ctz}
\title{Calculate Factor Z-Score with Winsorization}
\usage{
ctz(values, weights, group, win_prob = c(0, 1))
}
\arguments{
\item{values}{a numeric vector of factor values}

\item{weights}{a numeric vector of weights.}

\item{group}{a character vector of grouping.}

\item{win.prob}{numeric vector of probabilities with values in [0,1]
as used in quantile.}
}
\value{
ord
}
\description{
Function to calculate normalized value with windsorization.
}
\details{
Used in Alpha Testing Functions
}
