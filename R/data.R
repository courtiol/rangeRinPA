#' Fetch the ranger data from the Google sheet
#'
#' This function retrieves the data from a Google spreadsheet and format them
#'
#' @return a tibble with the raw data
#' @export
#'
#' @examples
#' \dontrun{
#' ## Here is how we created the data stored in this package:
#' data_rangers <- fetch_data_rangers()
#' if (require(usethis)) {
#'   usethis::use_data(data_rangers, overwrite = TRUE)
#' }
#' }
#'
fetch_data_rangers <- function() {

  ### Read the data online:
  ### TODO: replace by csv when data are final!
  googlesheets4::gs4_auth() # google authentification

  d <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1x3uo3xojtPAcmB3BDMpBVDuOaYRkYB-mfa9icBZcThc",
                                 sheet = "Data for Stats",
                                 na = c("NA", ""),
                                 col_types = "??c??????????????????????????",
                                 range = "A1:AC251")

  ### We rename the columns:
  d %>%
    dplyr::rename(countryname_iso = "ISO 3 Letter Code",
                  countryname_eng = "SURVEYED COUNTRY",
                  data_year_info = "YEAR OF DATA",
                  staff_rangers_others_known = "R_BOTH \nWHERE BOTH RANGER AND NON RANGER NUMBERS ARE LISTED\nNUMBERS WHO ARE SPECIFICALLY IDENTIFIED AS RANGERS",
                  staff_others_rangers_known = "NR_BOTH\n  NUMBERS WHERE BOTH RANGER AND NON RANGER NUMBERS ARE LISTED\nNUMBERS WHO ARE SPECIFICALLY IDENTIFIED AS NON RANGERS",
                  staff_rangers_others_unknown = "R_ONLY\n NUMBERS WHERE ONLY RANGERS ARE LISTED (NON RANGER NUMBERS NOT PROVIDED)\nNUMBERS WHO ARE SPECIFICALLY IDENTIFIED AS RANGERS",
                  staff_total_details_unknown = "RNR_UND\nNUMBERS PROVIDED AS AN UNDIFFERENTIATED TOTAL (COMBINED RANGERS AND NON RANGERS)",
                  staff_total = "RNRT_1\nTOTAL SURVEYED STAFF  (SUM D,E,F,G)",
                  area_PA_surveyed = "AREA OF PAs REPORTED  SURVEYED IN STAFF SURVEY SQ KM",
                  area_PA_total = "AREA OF TERRESTRIAL PAS IN THE COUNTRY USED FOR CALCULATIONS (Higlighted IF DIFFERENT FROM WDPA)",
                  area_PA_WDPA = "TOTAL AREA OF TERRESTRIAL PAS IN THE COUNTRY in SQ KM (WDPA )",
                  area_country = "TOTAL TERRESTRIAL AREA OF  COUNTRY (WDPA)",
                  reliability = "RELIABILITY SCORE /25",
                  country_UN_continent = "UN CONT",
                  country_UN_subcontinent = "UN SUBCONT",
                  area_forest_pct = "Forest_area_percent_total",
                  EVI = "Ecosystem Vitality Index (EVI New)",
                  SPI = "Species Protection Index (SPI.new )",
                  EPI_2020 = "EPI.new_2020",
                  GDP_capita = "GDP_Per Capita",
                  GDP_2019 = "GDP_2019",
                  GDP_growth = "GDP_growth",
                  unemployment = "Unemployment_total",
                  rural_pct = "Rural_population_percentage",
                  income = "Adjusted net national income per capita",
                  pop_density = "Population Density",
                  IUCN_1_4_prop = "PROPORTION IUCN CAT (I-IV)/(I-VI)",
                  IUCN_1_2_prop = "PROPORTION IUCN CAT (I-II)/(I-VI)",
                  notes = "Notes") -> d

  ### Some 0 mean NA:
  d %>%
    dplyr::mutate(staff_rangers_others_unknown = dplyr::if_else(.data$staff_rangers_others_unknown == 0,
                                                                NA_real_,
                                                                .data$staff_rangers_others_unknown)) -> d

  ### Compute ranger variable:
  d %>%
    dplyr::mutate(staff_rangers = dplyr::if_else(!is.na(.data$staff_rangers_others_known),
                                                 .data$staff_rangers_others_known,
                                                 .data$staff_rangers_others_unknown),
                  .after = .data$staff_total) %>%
    dplyr::mutate(staff_others = .data$staff_total - .data$staff_rangers,
                  .after = .data$staff_rangers) %>%
    dplyr::mutate(staff_others = dplyr::if_else(!is.na(.data$staff_rangers_others_unknown), # remove 0s in others if others unknown
                                                NA_real_,
                                                .data$staff_others),
                  staff_total = dplyr::if_else(!is.na(.data$staff_rangers_others_unknown), # remove staff_total if others unknown
                                                NA_real_,
                                                .data$staff_total)) -> d

  ### Deal with continents:
  d %>%
    dplyr::mutate(country_UN_continent = as.character(.data$country_UN_continent),
                  country_UN_continent = dplyr::case_when(
                    .data$country_UN_continent == "142" ~ "Asia",
                    .data$country_UN_continent == "150" ~ "Europe",
                    .data$country_UN_continent == "2" ~ "Africa",
                    .data$country_UN_continent == "21" ~ "Northern America",
                    .data$country_UN_continent == "419" ~ "Latin America & Caribbean",
                    .data$country_UN_continent == "9" ~ "Oceania"),
                  country_UN_subcontinent = as.character(.data$country_UN_subcontinent)) -> d

  ### Deal with subcontinents:
  d %>%
    dplyr::mutate(country_UN_subcontinent = as.character(.data$country_UN_subcontinent),
                  country_UN_subcontinent = dplyr::case_when(
                    .data$country_UN_subcontinent == "15" ~ "Northern Africa",
                    .data$country_UN_subcontinent == "14" ~ "Eastern Africa",
                    .data$country_UN_subcontinent == "17" ~ "Middle Africa",
                    .data$country_UN_subcontinent == "18" ~ "Southern Africa",
                    .data$country_UN_subcontinent == "11" ~ "Western Africa",
                    .data$country_UN_subcontinent == "29" ~ "Caribbean",
                    .data$country_UN_subcontinent == "13" ~ "Central America",
                    .data$country_UN_subcontinent == "5" ~ "South America",
                    .data$country_UN_subcontinent == "21" ~ "Northern America",
                    .data$country_UN_subcontinent == "143" ~ "Central Asia",
                    .data$country_UN_subcontinent == "30" ~ "Eastern Asia",
                    .data$country_UN_subcontinent == "35" ~ "South-Eastern Asia",
                    .data$country_UN_subcontinent == "34" ~ "Southern Asia",
                    .data$country_UN_subcontinent == "145" ~ "Western Asia",
                    .data$country_UN_subcontinent == "151" ~ "Eastern Europe",
                    .data$country_UN_subcontinent == "154" ~ "Northern Europe",
                    .data$country_UN_subcontinent == "39" ~ "Southern Europe",
                    .data$country_UN_subcontinent == "155" ~ "Western Europe",
                    .data$country_UN_subcontinent == "53" ~ "Australia and New Zealand",
                    .data$country_UN_subcontinent == "54" ~ "Melanesia",
                    .data$country_UN_subcontinent == "57" ~ "Micronesia",
                    .data$country_UN_subcontinent == "61" ~ "Polynesia"),
                  country_UN_subcontinent = as.character(.data$country_UN_subcontinent)) -> d

  ### Manual fixes for continents and subcontinents:
  d$country_UN_continent[d$countryname_iso == "ALA"] <- d$country_UN_continent[d$countryname_iso == "FIN"]       # Aland Islands as Finland
  d$country_UN_subcontinent[d$countryname_iso == "ALA"] <- d$country_UN_subcontinent[d$countryname_iso == "FIN"] # Aland Islands as Finland

  d$country_UN_continent[d$countryname_iso == "ATA"] <- "Antarctica"    # Antarctica
  d$country_UN_subcontinent[d$countryname_iso == "ATA"] <- "Antarctica" # Antarctica

  d$country_UN_continent[d$countryname_iso == "MAC"] <- d$country_UN_continent[d$countryname_iso == "CHN"]       # Macau as China
  d$country_UN_subcontinent[d$countryname_iso == "MAC"] <- d$country_UN_subcontinent[d$countryname_iso == "CHN"] # Macau as China

  d$country_UN_continent[d$countryname_iso == "VAT"] <- d$country_UN_continent[d$countryname_iso == "ITA"] # Vatican as Italy
  d$country_UN_subcontinent[d$countryname_iso == "VAT"] <- d$country_UN_subcontinent[d$countryname_iso == "ITA"] # Vatican as Italy

  ### Temporary patch for private analysis only, TODO: remove before release
  d$countryname_iso[d$countryname_eng == "W African Country"] <- "SEN"
  d$countryname_eng[d$countryname_eng == "W African Country"] <- "Senegal"

  ### Dealing with PA variables:
  ## Make clean columns for PA areas:
  d %>%
    dplyr::mutate(PA_area_unsurveyed = .data$area_PA_total - .data$area_PA_surveyed, .after = .data$area_PA_total) %>%
    dplyr::rename(PA_area_surveyed = .data$area_PA_surveyed) -> d

  ## Dealing with missing data on PA areas:
  d %>%
    dplyr::mutate(area_PA_total = dplyr::if_else(is.na(.data$area_PA_total), .data$area_PA_WDPA, .data$area_PA_total),
                  PA_area_surveyed = dplyr::if_else(is.na(.data$PA_area_surveyed), 0, .data$PA_area_surveyed),
                  PA_area_unsurveyed = dplyr::if_else(is.na(.data$PA_area_unsurveyed), .data$area_PA_total, .data$PA_area_unsurveyed)) -> d

  ## No area_PA_total means no ranger or staff:
  d %>%
    dplyr::mutate(staff_rangers_others_known = dplyr::if_else(.data$area_PA_total == 0, 0, .data$staff_rangers_others_known),
                  staff_others_rangers_known = dplyr::if_else(.data$area_PA_total == 0, 0, .data$staff_others_rangers_known),
                  staff_total = dplyr::if_else(.data$area_PA_total == 0, 0, .data$staff_total),
                  staff_rangers = dplyr::if_else(.data$area_PA_total == 0, 0, .data$staff_rangers),
                  staff_others = dplyr::if_else(.data$area_PA_total == 0, 0, .data$staff_others)) -> d

  ## Computing PA sampling coverage:
  d %>%
    dplyr::mutate(sampled_coverage = 100*.data$PA_area_surveyed / (.data$PA_area_surveyed + .data$PA_area_unsurveyed)) -> d

  if (any(d$sampled_coverage[!is.na(d$sampled_coverage)] > 100)) {
    cat("\nSome countries have sampled coverage > 100%, there must be an issue. The problematic countries are:\n")
    print(d$countryname_eng[d$sampled_coverage > 100 & !is.na(d$sampled_coverage)])
    d$sampled_coverage[d$sampled_coverage > 100 & !is.na(d$sampled_coverage)] <- 100
  }

  if (any(is.na(d$sampled_coverage))) {
    cat("\nSome countries have no PAs in our dataset. These countries are:\n")
    print(d$countryname_eng[is.na(d$sampled_coverage)])
  }

  ### Adding latitude and longitude automatically:
  world_sf <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
  world_sf$iso_a3_eh[world_sf$admin == "Norway"] <- "NOR" ## manual fix
  #world_sf$admin[unlist(sapply(world_sf$admin, function(x) grepl("Gu.*", x)))] ## to search for typos

  ## Add prefix to variable others than geometry:
  i <- which(names(world_sf) != "geometry")
  names(world_sf)[i] <- paste0("rne_", names(world_sf)[i])

  ### Manual fix for some iso codes discrepancies:
  world_sf$rne_iso_a3_eh[world_sf$rne_name == "Kosovo"] <- "KOS"
  world_sf$rne_iso_a3_eh[world_sf$rne_name == "Indian Ocean Ter."] <- "IOA"
  world_sf$rne_iso_a3_eh[world_sf$rne_name == "Siachen Glacier"] <- "KAS"

  ### Manual fix for countries/territories referred to differently in the data:
  ## Ashmore is part of Australia in our data:
  world_sf$geometry[world_sf$rne_name == "Australia"] <- sf::st_combine(c(world_sf$geometry[world_sf$rne_name == "Australia"], world_sf$geometry[world_sf$rne_name == "Ashmore and Cartier Is."]))
  world_sf %>%
    dplyr::filter(world_sf$rne_name != "Ashmore and Cartier Is.") -> world_sf

  ## No difference between N and S Cyprus in our data:
  world_sf$geometry[world_sf$rne_name == "Cyprus"] <- sf::st_combine(c(world_sf$geometry[world_sf$rne_name == "Cyprus"], world_sf$geometry[world_sf$rne_name == "N. Cyprus"]))
  world_sf %>%
    dplyr::filter(world_sf$rne_name != "N. Cyprus") -> world_sf


  ## No difference between Somaliland and Somalia in our data:
  world_sf$geometry[world_sf$rne_name == "Somalia"] <- sf::st_combine(c(world_sf$geometry[world_sf$rne_name == "Somalia"], world_sf$geometry[world_sf$rne_name == "Somaliland"]))
  world_sf %>%
    dplyr::filter(world_sf$rne_name != "Somaliland") -> world_sf

  ### Manual fix for polygons combined with others which we need to appear as distinct rows:
  GLP_geom <- extract_polygon(data = world_sf, countryname_eng = "France", lon = c(-62, -61), lat = c(16.5, 15.7)) ## Guadeloupe
  world_sf$geometry[world_sf$rne_name == "France"] <- clipout_polygon(data = world_sf, countryname_eng = "France", lon = c(-62, -61), lat = c(16.5, 15.7))

  GUF_geom <- extract_polygon(data = world_sf, countryname_eng = "France", lon = c(-55, -50), lat = c(6, 2)) ## French Guiana
  world_sf$geometry[world_sf$rne_name == "France"] <- clipout_polygon(data = world_sf, countryname_eng = "France", lon = c(-55, -50), lat = c(6, 2))

  REU_geom <- extract_polygon(data = world_sf, countryname_eng = "France", lon = c(55, 56), lat = c(-20.7, -21.5)) ## Reunion
  world_sf$geometry[world_sf$rne_name == "France"] <- clipout_polygon(data = world_sf, countryname_eng = "France", lon = c(55, 56), lat = c(-20.7, -21.5))

  BES_geom <- extract_polygon(data = world_sf, countryname_eng = "Netherlands", lon = c(-69, -58), lat = c(19, 10)) ## Bonaire, Sint Eustatius and Saba
  world_sf$geometry[world_sf$rne_name == "Netherlands"] <- clipout_polygon(data = world_sf, countryname_eng = "Netherlands", lon = c(-69, -58), lat = c(19, 10))

  MTQ_geom <- extract_polygon(data = world_sf, countryname_eng = "France", lon = c(-62, -60), lat = c(15, 14)) ## Martinique
  world_sf$geometry[world_sf$rne_name == "France"] <- clipout_polygon(data = world_sf, countryname_eng = "France", lon = c(-62, -60), lat = c(15, 14))

  MYT_geom <- extract_polygon(data = world_sf, countryname_eng = "France", lon = c(44, 46), lat = c(-12, -13)) ## Mayotte
  world_sf$geometry[world_sf$rne_name == "France"] <- clipout_polygon(data = world_sf, countryname_eng = "France", lon = c(44, 46), lat = c(-12, -13))

  SJM1_geom <- extract_polygon(data = world_sf, countryname_eng = "Norway", lon = c(5, 40), lat = c(75.7, 82)) ## Svalbard...
  world_sf$geometry[world_sf$rne_name == "Norway"] <- clipout_polygon(data = world_sf, countryname_eng = "Norway", lon = c(5, 40), lat = c(75.7, 82))

  SJM2_geom <- extract_polygon(data = world_sf, countryname_eng = "Norway", lon = c(-10, -7.2), lat = c(70.7, 71.4)) ## and Jan Mayen
  world_sf$geometry[world_sf$rne_name == "Norway"] <- clipout_polygon(data = world_sf, countryname_eng = "Norway", lon = c(-10, -7.2), lat = c(70.7, 71.4))

  SJM_geom <- sf::st_combine(c(SJM1_geom, SJM2_geom))

  TKL_geom <- extract_polygon(data = world_sf, countryname_eng = "New Zealand", lon = c(-174, -169),lat =  c(-6, -11)) ## Tuvalu
  world_sf$geometry[world_sf$rne_name == "New Zealand"] <- clipout_polygon(data = world_sf, countryname_eng = "New Zealand", lon = c(-174, -169),lat =  c(-6, -11))

  CCK_geom <- extract_polygon(data = world_sf, countryname_eng = "Indian Ocean Ter.", lon = c(96.6, 97.1), lat =  c(-11.7, -12.3)) ## Cocos (Keeling) Islands
  world_sf$geometry[world_sf$rne_name == "Indian Ocean Ter."] <- clipout_polygon(data = world_sf, countryname_eng = "Indian Ocean Ter.", lon = c(96.6, 97.1), lat =  c(-11.7, -12.3))

  CXR_geom <- extract_polygon(data = world_sf, countryname_eng = "Indian Ocean Ter.", lon = c(105.48, 105.8), lat =  c(-10.37, -10.62)) ## Christmas Island
  world_sf$geometry[world_sf$rne_name == "Indian Ocean Ter."] <- clipout_polygon(data = world_sf, countryname_eng = "Indian Ocean Ter.", lon = c(105.48, 105.8), lat =  c(-10.37, -10.62))


  ## Bouvet Island -> no polygon but tiny
  ## Cocos (Keeling) Islands -> no polygon but tiny
  ## Christmas Island -> no polygon but tiny
  ## Gibraltar -> no polygon but tiny
  ## United States Minor Outlying Islands -> no polygon but tiny

  polygons_data_to_add <- tibble::tibble(rne_iso_a3_eh = c("GLP", "GUF", "REU", "BES", "MTQ", "MYT", "SJM", "TKL", "CCK", "CXR"),
                                         rne_name = c("Guadeloupe", "French Guiana", "Reunion", "Bonaire, Sint Eustatius and Saba", "Martinique", "Mayotte", "Svalbard and Jan Mayen", "Tuvalu", "Cocos (Keeling) Islands", "Christmas Island"),
                                         geometry = c(GLP_geom, GUF_geom, REU_geom, BES_geom, MTQ_geom, MYT_geom, SJM_geom, TKL_geom, CCK_geom, CXR_geom))

  world_sf |>
    dplyr::bind_rows(polygons_data_to_add) -> world_sf

  ### Check locations not found in map (depend on scale defined above) or not found in data:
  d |>
    dplyr::anti_join(world_sf, by = c(countryname_iso = "rne_iso_a3_eh")) |>
    dplyr::pull(.data$countryname_eng) -> missing1

  world_sf |>
    dplyr::anti_join(d, by = c(rne_iso_a3_eh = "countryname_iso")) |>
    dplyr::pull(.data$rne_name) -> missing2

  missing <- c(missing1, missing2)

  if (length(missing1) > 0) {
    cat("\nHere are the countries/territories for which polygons are not found:\n")
    print(missing1)
  }

  if (length(missing1) > 0) {
    cat("\nHere are the countries/territories for which data are not found in our dataset:\n")
    print(missing2)
    #world_sf |>
    #  dplyr::filter(!.data$rne_name %in% missing2) -> world_sf ## to remove but then no plotted
  }

  ### Extract coordinate of the center of each country/territory:
  world_sf$center <- sf::st_centroid(world_sf$geometry, of_largest_polygon = TRUE)

  ### Add geographical info to the data:
  d %>%
    dplyr::left_join(world_sf %>% dplyr::select(.data$center, .data$rne_iso_a3_eh), by = c("countryname_iso" = "rne_iso_a3_eh")) -> d

  d %>%
    tidyr::unnest_wider(col = .data$center, names_sep = "") %>%
    dplyr::rename(long = .data$center1, lat = .data$center2) -> d

  ### Add coordinates manually for countries/territories with no polygons:
  ## Bouvet Island:
  d$lat[d$countryname_iso == "BVT"]  <- -54.4204601
  d$long[d$countryname_iso == "BVT"] <- 3.3245614

  ### Adding flags
  d %>%
    dplyr::mutate(flag = countrycode::countrycode(sourcevar = .data$countryname_iso, "iso3c", "unicode.symbol", custom_match = c("KOS" = NA))) -> d
  d$flag[d$countryname_iso == "KOS"] <- "\U1f1fd\U1f1f0"
  d %>%
    dplyr::mutate(country = paste(.data$countryname_eng, .data$flag)) -> d

  ### Adding row names
  #rownames(d) <- d$countryname_iso ## support in tibbles should stop soon
  d
}

globalVariables(".data")


#' Ranger data
#'
#' This object contain the data about rangers and other staffs members working in protected areas.
#'
#' @seealso [`fetch_data_rangers()`] for the function used to create such a dataset
#'
#' @examples
#' data_rangers
#'
"data_rangers"


#' Impute missing staff numbers proportionally to the density in surveyed areas
#'
#' This function modifies the numbers of staffs for countries/territories where it is partially known.
#' For those country, it assumes that the unsurveyed area have a density of staff proportional to the density in the surveyed areas.
#' The proportionality is set by the argument `coef`:
#'     - e.g. if `coef = 1`, then unsurveyed areas are populated with the same density as the surveyed areas.
#'     - e.g. if `coef = 0.5`, then unsurveyed areas are populated with half the density as the surveyed areas.
#'
#' @param data a data.frame or tibble produced by [`fetch_data_rangers()`]
#' @param coef the coefficient used for the propagation
#'
#' @return a dataframe or tibble
#' @export
#'
#' @examples
#' data_rangers_50 <- fill_PA_area(data_rangers, coef = 0.5)
#' surveyed_fraction <- with(data_rangers, PA_area_surveyed /
#'      (PA_area_surveyed + PA_area_unsurveyed))
#' plot(surveyed_fraction,
#'      surveyed_fraction * data_rangers_50$staff_rangers / data_rangers$staff_rangers,
#'      ylim = c(0, 1), xlim = c(0, 1))
#' polygon(x = c(0, 1, 0, 0), y = c(0, 1, 0.5, 0))
#'
fill_PA_area <- function(data, coef) {

  if (coef < 0 || coef > 1) {
    stop("coef must be between 0 and 1")
  }

  data %>%
    dplyr::mutate(PA_area_surveyed_notfilled = .data$PA_area_surveyed,
                  PA_area_unsurveyed_notfilled = .data$PA_area_unsurveyed) %>%
    dplyr::mutate(dplyr::across(tidyselect::contains("staff"), \(staff) staff + coef * staff/.data$PA_area_surveyed * .data$PA_area_unsurveyed)) %>%
    dplyr::mutate(PA_area_surveyed = .data$PA_area_surveyed + .data$PA_area_unsurveyed,
                  PA_area_unsurveyed = 0,
                  prop_surveyed = .data$PA_area_surveyed_notfilled / (.data$PA_area_surveyed_notfilled + .data$PA_area_unsurveyed_notfilled))

}


#' Build the training datasets
#'
#' These functions create a subset of the dataset [`data_rangers`] and log-transform (+1) some of its columns.
#'
#' @inheritParams prepare_data
#' @param response the unquoted name of the response variable
#' @param survey the criterion used to select rows depending on whether the focal number of staff is:
#'     - completely unknown ("complete_unknown")
#'     - completely or partially unknown ("partial_unknown")
#'     - completely or partially known ("partial_known")
#'     - completely known ("complete")
#' according to the choice, the variable PA_area is also adjusted.
#' @param NA_in_resp whether or not to keep only NA (TRUE) or discard them all (FALSE) in response variable (default = NULL -> do nothing)
#' @param NA_in_preds whether or not to keep only NA (TRUE) or discard them all (FALSE) in predictor variables (default = NULL -> do nothin)
#' @param keep_details whether or not to keep variables used for construction (default = FALSE)
#' @param type either "prediction" or "training"
#'
#' @return a tibble
#' @name build_training_data
#' @aliases build_initial_training_data, build_final_training_data
#'
#' @examples
#' \dontrun{
#' ## Here is how we created the data stored in this package:
#' data_test <- build_initial_training_data(data_rangers,
#'                                          formula = staff_rangers ~ pop_density_log +
#'                                                    lat + long + country_UN_subcontinent +
#'                                                    PA_area_log + area_country_log +
#'                                                    area_forest_pct + GDP_2019_log +
#'                                                    GDP_capita_log + GDP_growth +
#'                                                    unemployment_log + EVI + SPI + EPI_2020 +
#'                                                    IUCN_1_4_prop + IUCN_1_2_prop,
#'                                          survey = "partial_known")
#' data_test <- data_test[!is.na(data_test$staff_rangers_log), ]
#' if (require(usethis)) {
#'   usethis::use_data(data_test, overwrite = TRUE)
#' }
#' }
#'
NULL


#' @describeIn build_training_data build the initial training datasets
#' @export
#'
build_initial_training_data <- function(data, formula, survey, spatial = FALSE) {
  data <- build_data(data = data, formula = formula, type = "training", spatial = spatial)
  data <- handle_outliers(data = data)
  data <- handle_PA_area(data = data, survey = survey, formula = formula, keep_details = TRUE)
  data <- handle_transform(data = data)
  data <- handle_na(data = data, response = paste0(as.character(formula)[[2]], "_log"))
  data <- handle_order(data)
  data
}


#' @describeIn build_training_data build the final training datasets
#' @export
#'
build_final_training_data <- function(data, formula, survey, spatial = FALSE) {
  data <- build_data(data = data, formula = formula, type = "training", spatial = spatial)
  data <- handle_PA_area(data = data, survey = survey, formula = formula, keep_details = TRUE)
  data <- handle_outliers(data = data)
  data <- handle_transform(data = data)
  data <- handle_order(data)
  data
}

#' @describeIn build_training_data build the final prediction datasets
#' @export
#'
build_final_pred_data <- function(data, formula, survey, spatial = FALSE) {
  data_list <- build_data(data = data, formula = formula, type = "prediction", spatial = spatial)
  for (data in names(data_list)) {
    if (data == "data_known") {
      data_list[[data]] <- handle_PA_area(data = data_list[[data]], survey = survey, formula = formula, keep_details = TRUE)
    } else {
      if (survey == "partial_known") {
        data_list[[data]] <- handle_PA_area(data = data_list[[data]], survey = "any_unknown", formula = formula, keep_details = TRUE)
      } else if (survey == "complete_known") {
        data_list[[data]] <- handle_PA_area(data = data_list[[data]], survey = "any_known", formula = formula, keep_details = TRUE)
      } else {
        stop("wrong survey argument")
      }
      data_list[[data]] <- handle_outliers(data = data_list[[data]])
    }
    data_list[[data]] <- handle_transform(data = data_list[[data]])
    data_list[[data]] <- handle_order(data_list[[data]])
  }
  data_list
}

#' @describeIn build_training_data internal function to build the training and prediction datasets
#' @export
#'
build_data <- function(data, formula, type, spatial = FALSE) {

  formula <- drop_logs(formula)

  if (type == "training") {
    data <- prepare_data(formula = formula, data = data, test_prop = 0, drop_na = TRUE, spatial = spatial,
                         keep.var = c("countryname_eng", "PA_area_surveyed", "PA_area_unsurveyed"))$data_train
    data <- handle_na(data = data, response = as.character(formula)[[2]], NA_in_resp = FALSE, NA_in_preds = FALSE)
  } else if (type == "prediction") {
    data <- prepare_data(formula = formula, data = data, test_prop = 0, drop_na = FALSE, spatial = spatial,
                         keep.var = c("countryname_eng", "PA_area_surveyed", "PA_area_unsurveyed"))$data_train
    data_known   <- handle_na(data = data, response = as.character(formula)[[2]], NA_in_resp = FALSE, NA_in_preds = NULL)
    data_only_na <- handle_na(data = data, response = as.character(formula)[[2]], NA_in_resp = NULL, NA_in_preds = TRUE)
    data_no_na   <- handle_na(data = data, response = as.character(formula)[[2]], NA_in_resp = NULL, NA_in_preds = FALSE)

    data_only_na   <- data_only_na[is.na(data_only_na[[as.character(formula)[[2]]]]) | !(!is.na(data_only_na[[as.character(formula)[[2]]]]) & data_only_na$PA_area_unsurveyed == 0), ]
    data_no_na   <- data_no_na[is.na(data_no_na[[as.character(formula)[[2]]]]) | !(!is.na(data_no_na[[as.character(formula)[[2]]]]) & data_no_na$PA_area_unsurveyed == 0), ]

    data <- list(data_not_predictable = data_only_na, data_predictable = data_no_na, data_known = data_known)
  } else {
    stop("type unknown")
  }
  data
}


#' @describeIn build_training_data internal function to handle PA_area while building the datasets
#' @export
#'
handle_PA_area <- function(data, survey, formula = NULL, keep_details = FALSE) {
  if (survey == "complete_unknown") {
    data %>%
      dplyr::filter(.data$PA_area_surveyed < 0.1) %>%
      dplyr::mutate(PA_area = .data$PA_area_unsurveyed) -> data
  } else if (survey == "partial_unknown") {
    data %>%
      dplyr::filter(.data$PA_area_unsurveyed > 0) %>%
      dplyr::mutate(PA_area = .data$PA_area_unsurveyed) -> data
  } else if (survey == "any_unknown") {
    data %>%
      dplyr::mutate(PA_area = .data$PA_area_unsurveyed) -> data
  } else if (survey == "complete_known") {
    data %>%
      dplyr::filter(.data$PA_area_unsurveyed < 0.1) %>%
      dplyr::mutate(PA_area = .data$PA_area_surveyed) -> data
  } else if (survey == "partial_known") {
    data %>%
      dplyr::filter(.data$PA_area_surveyed > 0) %>%
      dplyr::mutate(PA_area = .data$PA_area_surveyed) -> data
  } else if (survey == "any_known") {
    data %>%
      dplyr::mutate(PA_area = .data$PA_area_surveyed) -> data
  } else stop("survey argument invalid")

  if (!is.null(formula) && !any(grepl(pattern = "PA_area", as.character(formula)[[3]]))) {
    data %>%
      dplyr::select(-.data$PA_area) -> data
  }

  # if (!keep_known) {
  #   data %>%
  #     dplyr::filter(.data$PA_area_unsurveyed == 0 && data$PA_area_surveyed > 0) -> data
  # }

  if (!keep_details) {
  data %>%
    dplyr::select(-.data$PA_area_surveyed, -.data$PA_area_unsurveyed) -> data
  }

  data
}


#' @describeIn build_training_data internal function to handle outliers while building the datasets
#' @export
#'
handle_outliers <- function(data) {
  data %>%
    dplyr::filter(.data$countryname_eng != "Greenland")
}


#' @describeIn build_training_data internal function to handle variable transformation while building the datasets
#' @export
#'
handle_transform <- function(data) {
  for (var in colnames(data)) {
    if (var %in% c("staff_rangers", "staff_others", "staff_total", "PA_area", "area_country", "pop_density", "GDP_2019", "GDP_capita", "unemployment")) {
      data[[paste0(var, "_log")]] <- log(data[[var]] + 1)
      data[[var]] <- NULL
    }
  }
  data
}


#' @describeIn build_training_data internal function to handle order of variables while building the datasets
#' @export
#'
handle_order <- function(data) {

  inorder <- c("countryname_eng",
               "staff_rangers", "staff_rangers_log",
               "staff_others", "staff_others_log",
               "staff_total", "staff_total_log",
               "PA_area_surveyed", "PA_area_unsurveyed", "PA_area",  "PA_area_log",
               "lat", "long",
               "area_country", "area_country_log", "area_forest_pct",
               "pop_density", "pop_density_log",
               "GDP_2019", "GDP_2019_log", "GDP_capita", "GDP_capita_log", "GDP_growth",
               "unemployment", "unemployment_log",
               "EVI", "SPI", "EPI_2020", "IUCN_1_4_prop", "IUCN_1_2_prop")

  data_ordered <- data[, stats::na.omit(match(inorder, colnames(data)))]

  data_ordered
}

#' @describeIn build_training_data internal function to handle missing data while building the datasets
#' @export
#'
handle_na <- function(data, response, NA_in_resp = NULL, NA_in_preds = NULL) {

  bool_response <- grepl(pattern = response, x = colnames(data))

  if (!is.null(NA_in_resp)) {
    if (!NA_in_resp) {
      data <- data[!is.na(data[, bool_response]), ]
    } else if (NA_in_resp) {
      data <- data[is.na(data[, bool_response]), ]
    }
  }

  if (!is.null(NA_in_preds)) {
    if (!NA_in_preds) {
      data <- data[!apply(is.na(data[, !bool_response, drop = FALSE]), 1, any), ]
    } else if (NA_in_preds) {
      data <- data[apply(is.na(data[, !bool_response, drop = FALSE]), 1, any), ]
    }
  }

  data
}

#' Test data
#'
#' This object contain a subset with the data about rangers and other staffs members working in protected areas.
#'
#' @seealso [`build_initial_training_data()`] for the function used to create such a dataset
#'
#' @examples
#' data_test
#'
"data_test"
