% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{table_correlates}
\alias{table_correlates}
\title{Create table with correlations between variables}
\usage{
table_correlates(data, method = "pearson")
}
\arguments{
\item{data}{a dataset with info on continents if breakdown by continent is required (optional)}

\item{method}{a character string indicating which correlation coefficient (or covariance) is to
be computed. One of "pearson" (default), "kendall", or "spearman": can be abbreviated.}
}
\value{
a correlation matrix
}
\description{
Create table with correlations between variables
}
\examples{
# see see ?rangeRinPA

}
