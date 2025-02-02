% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-at_settings.R
\docType{class}
\name{at_settings-class}
\alias{at_settings-class}
\title{Alpha Testing Settings (S4 Object)}
\description{
An S4 object that encapsulates settings for alpha testing.
}
\section{Slots}{

\describe{
\item{\code{start_date}}{A `Date` object representing the earliest date for alpha
testing.}

\item{\code{end_date}}{A `Date` object representing the latest date for alpha
testing.}

\item{\code{testing_scheme}}{A character string representing the alpha testing
scheme.}

\item{\code{weighting_scheme}}{A character string representing the weighting scheme.}

\item{\code{benchmark_weighting_scheme}}{A character string representing the
benchmark weighting scheme.}

\item{\code{.desc}}{A logical value indicating whether the factor scores should
be ranked in descending order.}
}}

