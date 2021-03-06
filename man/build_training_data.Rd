% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{build_training_data}
\alias{build_training_data}
\alias{build_initial_training_data,}
\alias{build_final_training_data}
\alias{build_initial_training_data}
\alias{build_final_pred_data}
\alias{build_data}
\alias{handle_PA_area}
\alias{handle_outliers}
\alias{handle_transform}
\alias{handle_order}
\alias{handle_na}
\title{Build the training datasets}
\usage{
build_initial_training_data(data, formula, survey, spatial = FALSE)

build_final_training_data(data, formula, survey, spatial = FALSE)

build_final_pred_data(data, formula, survey, spatial = FALSE, outliers = NULL)

build_data(data, formula, type, spatial = FALSE)

handle_PA_area(data, survey, formula = NULL, keep_details = FALSE)

handle_outliers(data, outliers = NULL)

handle_transform(data)

handle_order(data)

handle_na(data, response, NA_in_resp = NULL, NA_in_preds = NULL)
}
\arguments{
\item{data}{the complete dataset}

\item{formula}{the formula for the LMM or RF}

\item{survey}{the criterion used to select rows depending on whether the focal number of personnel is:
- completely unknown ("complete_unknown")
- completely or partially unknown ("partial_unknown")
- completely or partially known ("partial_known")
- completely known ("complete")
according to the choice, the variable PA_area is also adjusted.}

\item{spatial}{whether or not keeping predictor for fitting spatial effects (default = FALSE)}

\item{outliers}{a vector with the name of the countries/territories to discard}

\item{type}{either "prediction" or "training"}

\item{keep_details}{whether or not to keep variables used for construction (default = FALSE)}

\item{response}{the unquoted name of the response variable}

\item{NA_in_resp}{whether or not to keep only NA (TRUE) or discard them all (FALSE) in response variable (default = NULL -> do nothing)}

\item{NA_in_preds}{whether or not to keep only NA (TRUE) or discard them all (FALSE) in predictor variables (default = NULL -> do nothing)}
}
\value{
a tibble
}
\description{
These functions create a subset of the dataset \code{\link{data_rangers}} and log-transform (+1) some of its columns.
}
\section{Functions}{
\itemize{
\item \code{build_initial_training_data}: build the initial training datasets

\item \code{build_final_training_data}: build the final training datasets

\item \code{build_final_pred_data}: build the final prediction datasets

\item \code{build_data}: internal function to build the training and prediction datasets

\item \code{handle_PA_area}: internal function to handle PA_area while building the datasets

\item \code{handle_outliers}: internal function to handle outliers while building the datasets

\item \code{handle_transform}: internal function to handle variable transformation while building the datasets

\item \code{handle_order}: internal function to handle order of variables while building the datasets

\item \code{handle_na}: internal function to handle missing data while building the datasets
}}

\examples{
\dontrun{
## Here is how we created the data stored in this package:
data_test <- build_initial_training_data(data_rangers,
                                         formula = staff_rangers ~ pop_density_log +
                                                   lat + long + country_UN_subcontinent +
                                                   PA_area_log + area_country_log +
                                                   area_forest_pct + GDP_2019_log +
                                                   GDP_capita_log + GDP_growth +
                                                   unemployment_log + EVI + SPI + EPI_2020 +
                                                   IUCN_1_4_prop + IUCN_1_2_prop,
                                         survey = "partial_known")
data_test <- data_test[!is.na(data_test$staff_rangers_log), ]
if (require(usethis)) {
  usethis::use_data(data_test, overwrite = TRUE)
}
}

}
