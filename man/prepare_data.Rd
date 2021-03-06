% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{prepare_data}
\alias{prepare_data}
\title{Create train and test datasets given a formula and data}
\usage{
prepare_data(
  formula,
  data,
  test_prop = 0,
  keep.var = NULL,
  drop_na = TRUE,
  spatial = FALSE,
  seed = NULL
)
}
\arguments{
\item{formula}{the formula for the LMM or RF}

\item{data}{the complete dataset}

\item{test_prop}{the amount of rows to keep in the test dataset (default = 0)}

\item{keep.var}{a vector of character strings indicating which variables to keep in the dataset on top of those defined by the formula}

\item{drop_na}{whether to drop NAs or not (default = TRUE)}

\item{spatial}{whether or not keeping predictor for fitting spatial effects (default = FALSE)}

\item{seed}{an optional seed for the RNG}
}
\description{
Create train and test datasets given a formula and data
}
\examples{
prepare_data(log(staff_rangers + 1) ~ log(GDP_2019) + Matern(1|long + lat),
             data = data_rangers, test_prop = 0.1)

}
