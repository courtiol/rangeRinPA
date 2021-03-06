% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_selection_RF.R
\name{rank_predictors_RF}
\alias{rank_predictors_RF}
\title{Extract top predictors}
\usage{
rank_predictors_RF(fit, k = NULL)
}
\arguments{
\item{fit}{a model fitted with \href{ranger::ranger}{\code{ranger()}}}

\item{k}{the maximum number of predictors to return (default = NULL for no sub-selection)}
}
\value{
predictors with their importance
}
\description{
This function extract the top (fixed-effects) predictors from a RF based on their importance.
}
