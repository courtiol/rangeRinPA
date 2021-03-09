#' Fetch the ranger data from the Google sheet
#'
#' This function retrieves the data from a Google spreadsheet and format them
#'
#' @return a tibble with the raw data
#' @export
#'
#' @examples
#' \dontrun{
#' fetch_data_rangers()
#' }
#'
fetch_data_rangers <- function() {
  googlesheets4::gs4_auth()

  d <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1x3uo3xojtPAcmB3BDMpBVDuOaYRkYB-mfa9icBZcThc",
                                 sheet = "Data Summary",
                                 range = "A1:AM163")

  ## We rename the columns:
  d %>%
    dplyr::rename(country_name_iso_1 = "...1",
                  country_name_eng = "SURVEYED COUNTRY",
                  staff_rangers = "STAFF WHO ARE SPECIFICALLY IDENTIFIED AS RANGERS",
                  staff_others = "STAFF WHO ARE SPECIFICALLY IDENTIFIED AS NON RANGERS",
                  staff_others_PROBLEM  = "...5",
                  staff_undifferentiated = "NUMBERS PROVIDED AS AN UNDIFFERENTIATED TOTAL (COMBINED RANGERS AND NON RANGERS)",
                  empty_to_discard_1 = "...7",
                  staff_total = "TOTAL KNOWN STAFF (SUM B C D)",
                  area_PA_surveyed = "AREA OF PROTECTED AREAS SURVEYED IN STAFF SURVEY SQ KM",
                  area_PA_total = "AREA OF TERRESTRIAL PAS IN THE COUNTRY USED FOR CALCULATIONS (Higlighted IF DIFFERENT FROM WDPA)",
                  area_PA_WDPA_1 = "TOTAL AREA OF TERRESTRIAL PAS IN THE COUNTRY in SQ KM (WDPA )...11",
                  area_country = "TOTAL TERRESTRIAL AREA OF ENTIRE COUNTRY (WDPA)",
                  reliability = "RELIABILITY SCORE /25",
                  RANGER_MAX = "RANGER MAX (remaining area populated at 100% density)",
                  NONRANGER_MAX = "NON RANGER MAX (remaining area populated at 100% density)",
                  TOTAL_MAX = "TOTAL MAX (remaining area populated at 100% density)",
                  RANGER_MIN = "RANGER MIN (remaining area populated at 25% density)",
                  NONRANGER_MIN = "NON RANGER MIN  (remaining area populated at 25% density)",
                  TOTAL_MIN = "TOTAL MIN (remaining area populated at 25% density)",
                  empty_to_discard_2 = "...20",
                  empty_to_discard_3 = "INDICATOR DATA",
                  country_name_iso_2 = "ISO 3 LETTER",
                  country_UN_continent = "UN CONT",
                  country_UN_subcontinent = "UN SUBCONT",
                  forest_area_pct = "Forest_area_percent_total",
                  area_PA_WDPA_2 = "TOTAL AREA OF TERRESTRIAL PAS IN THE COUNTRY in SQ KM (WDPA )...26",
                  EVI = "Ecosystem Vitality Index (EVI New)",
                  SPI = "Species Protection Index (SPI.new )",
                  EPI_2020 = "EPI.new_2020",
                  GDP_capita = "GDP_Per Capita",
                  GDP_2019 = "GDP_2019",
                  GPD_growth = "GDP_growth",
                  unemployment = "Unemployment_total",
                  rural_pct = "Rural_population_percentage",
                  incomce_capita = "Adjusted net national income per capita",
                  pop_density = "Population Density",
                  IUCN_A = "PROPORTION IUCN CAT (I-IV)/(I-VI)",
                  IUCN_B = "PROPORTION IUCN CAT (I-II)/(I-VI)",
                  notes = "Notes") -> d

  ## We remove columns with no data:
  d %>%
    dplyr::select(which(!grepl(pattern = "discard", x = colnames(d)))) -> d

  ## We reformat columns if needed:
  d %>%
    dplyr::mutate(dplyr::across(.cols = where(~ class(.) == "list"),
                                .fns = ~ unlist(lapply(., function(x) as.numeric(ifelse(is.null(x), NA, as.character(x))))))) -> d

  ## We remove redundant data:
  d %>%
    dplyr::select(-.data$country_name_iso_1) %>%
    dplyr::rename(country_name_iso = .data$country_name_iso_2) -> d

  ## Return
  print(d)
  d

}

globalVariables(c(".data", "where")) ## avoid note in R CMD check
