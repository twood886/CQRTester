% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method-check_factor_data_params.R
\name{check_factor_data_params,factor_data_params,data.frame-method}
\alias{check_factor_data_params,factor_data_params,data.frame-method}
\title{Validate Factor Data Parameters}
\usage{
\S4method{check_factor_data_params}{factor_data_params,data.frame}(params, data)
}
\arguments{
\item{params}{A `factor_data_params` object specifying parameter metadata.}

\item{data}{A `data.frame` containing the dataset to validate.}
}
\value{
A logical value indicating whether the parameters are valid.
}
\description{
Ensures that a `factor_data_params` object is consistent with the provided
dataset.
}
