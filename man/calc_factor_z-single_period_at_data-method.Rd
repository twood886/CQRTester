% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method-calc_factor_z.R
\name{calc_factor_z,single_period_at_data-method}
\alias{calc_factor_z,single_period_at_data-method}
\title{Calculate Z-Score-Based Factor Values}
\usage{
\S4method{calc_factor_z}{single_period_at_data}(at_data, win_prob = c(0, 1), .desc = TRUE)
}
\arguments{
\item{at_data}{A `single_period_at_data` object containing factor data.}

\item{win_prob}{A numeric vector specifying percentile cutoffs for
winsorization.}

\item{.desc}{A logical value indicating whether values should be ranked in
descending order.}
}
\value{
An `orderedList` object containing `factor_z_score` objects.
}
\description{
Computes Z-score-based factor values, applying winsorization if specified.
}
