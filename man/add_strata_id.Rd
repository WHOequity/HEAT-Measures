% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{add_strata_id}
\alias{add_strata_id}
\title{Add a new column called \code{strata_id} that will be unique for each strata. The default fields
used to determine unique strata are created in the data-raw/strata-variables.R file and are a
variable called HEAT_strata_variables}
\usage{
add_strata_id(.data, strata_vars = heatmeasures::HEAT_strata_variables)
}
\arguments{
\item{.data}{}

\item{strata_vars}{as string, a vector of field names to use to determine unique strata.}
}
\description{
Add a new column called \code{strata_id} that will be unique for each strata. The default fields
used to determine unique strata are created in the data-raw/strata-variables.R file and are a
variable called HEAT_strata_variables
}
