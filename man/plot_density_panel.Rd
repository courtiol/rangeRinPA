% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures.R
\name{plot_density_panel}
\alias{plot_density_panel}
\title{Plot densities of rangers and all personnel}
\usage{
plot_density_panel(
  what,
  data,
  ymax = 6000,
  breaks = c(10^(0:3), 2 * 10^(0:3), 5 * 10^(0:3))
)
}
\arguments{
\item{what}{an object produced by \code{\link[=run_LMM_workflow]{run_LMM_workflow()}} or \code{\link[=run_RF_workflow]{run_RF_workflow()}}}

\item{data}{a dataset with info on continents if breakdown by continent is required (optional)}

\item{ymax}{the maximal value for the y-axis}

\item{breaks}{a vector of number defining the horizontal lines in the plot (i.e. breaks in ggplot jargon)}
}
\description{
Plot densities of rangers and all personnel
}
\examples{
# see ?rangeRinPA

}
