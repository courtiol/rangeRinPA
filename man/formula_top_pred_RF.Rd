% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_selection_RF.R
\name{formula_top_pred_RF}
\alias{formula_top_pred_RF}
\title{Build formula from top predictors}
\usage{
formula_top_pred_RF(fit, resp, k = NULL)
}
\arguments{
\item{fit}{a model fitted with \href{ranger::ranger}{\code{ranger()}}}

\item{resp}{the name of the response variable}

\item{k}{the maximum number of predictors to return (default = NULL for no sub-selection)}
}
\value{
a formula
}
\description{
Build formula from top predictors
}
