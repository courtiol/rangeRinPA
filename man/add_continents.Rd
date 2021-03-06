% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{add_continents}
\alias{add_continents}
\title{Add continent to a table}
\usage{
add_continents(tbl, data, levels = NULL)
}
\arguments{
\item{tbl}{The table for which continents must be added}

\item{data}{The dataset containing the info on continents}

\item{levels}{a list of levels if the variable continent must be turned into a factor}
}
\description{
Add continent to a table
}
\examples{
add_continents(data.frame(countryname_eng = c("Turkey", "France")), data_rangers)

}
