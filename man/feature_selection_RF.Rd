% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_selection_RF.R
\name{feature_selection_RF}
\alias{feature_selection_RF}
\alias{feature_selection_RF,}
\alias{feature_selection_RF_internal}
\title{Perform feature selection on RFs}
\usage{
feature_selection_RF(
  full_fit,
  data,
  rerank = TRUE,
  metric = "RMSE",
  minimise = TRUE,
  rep = 10,
  Ncpu = 1,
  target = "staff_rangers_log",
  seed = 123,
  ...
)

feature_selection_RF_internal(
  full_fit,
  data,
  rerank = TRUE,
  rep = 10,
  Ncpu = 1,
  target = "staff_rangers_log",
  spatial = TRUE,
  seed = 123,
  ...
)
}
\arguments{
\item{full_fit}{a full fitted model}

\item{data}{the full dataset}

\item{rerank}{whether or not to recompute variable importance recursively during selection (default = TRUE)}

\item{metric}{the metric used for computing prediction accuracy (see \code{\link[=compute_metrics]{compute_metrics()}})}

\item{minimise}{whether the metric should be minimise (TRUE, default) or maximise (FALSE)}

\item{rep}{the number of cross validation replicates (default = 10)}

\item{Ncpu}{the number of CPU cores to be used (default = 1)}

\item{target}{the name of the response variable}

\item{seed}{the seed used to control the reproducibility of the cross validation}

\item{...}{additional parameters to be passed to \code{\link[spaMM:fitme]{spaMM::fitme()}}}

\item{spatial}{either FALSE (default) or TRUE}
}
\description{
Perform feature selection on RFs
}
\section{Functions}{
\itemize{
\item \code{feature_selection_RF}: wrapper function performing the feature selection on RFs with and without the spatial terms

\item \code{feature_selection_RF_internal}: internal function performing the feature selection on RFs
}}

