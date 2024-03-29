% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{table_core_data}
\alias{table_core_data}
\title{Create core data table}
\usage{
table_core_data(data, what = NULL)
}
\arguments{
\item{data}{the complete dataset created with \href{fetch_data}{\code{fetch_data}()}.}

\item{what}{an object produced by \code{\link[=run_LMM_workflow]{run_LMM_workflow()}} or \code{\link[=run_RF_workflow]{run_RF_workflow()}}}
}
\value{
a tibble
}
\description{
This function creates the table with the core information about surveyed countries/territories.
If the argument \code{what} is not null, it will also include estimates and densities for all PAs.
}
\examples{
table_core_data(data_rangers)

}
