% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method-check_at_data_params.R
\name{check_at_data_params,at_data_params,data.frame-method}
\alias{check_at_data_params,at_data_params,data.frame-method}
\title{Validate Alpha Test Data Parameters}
\usage{
\S4method{check_at_data_params}{at_data_params,data.frame}(params, data)
}
\arguments{
\item{params}{An `at_data_params` object specifying parameter metadata.}

\item{data}{A `data.frame` containing the dataset to validate.}
}
\value{
A logical value indicating whether the parameters are valid.
}
\description{
Checks the validity of an `at_data_params` object against a given dataset.
}
