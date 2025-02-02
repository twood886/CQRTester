% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-single_period_at_data.R
\docType{class}
\name{single_period_at_data-class}
\alias{single_period_at_data-class}
\title{Single Period Alpha Test Data (S4 Object)}
\description{
Represents alpha test data for a single period, encapsulating results and
configurations for individual factor and return evaluations.
}
\section{Slots}{

\describe{
\item{\code{factor}}{A character string representing the name of the factor.}

\item{\code{date}}{A `Date` object representing the specific period date.}

\item{\code{ids}}{A character vector containing company IDs for the period.}

\item{\code{fvals}}{An `orderedList` containing factor values for the period.}

\item{\code{returns}}{An `orderedList` containing returns data for the period.}

\item{\code{group}}{A named character vector indicating company groupings.}

\item{\code{weights}}{A named numeric vector of weights derived from factor values.}
}}

