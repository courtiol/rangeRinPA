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
  googlesheets4::gs4_auth()

  d <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1x3uo3xojtPAcmB3BDMpBVDuOaYRkYB-mfa9icBZcThc",
                                 sheet = "Data for Stats",
                                 na = c("NA", ""),
                                 col_types = "??c??????????????????????????")

  ## We rename the columns:
  d %>%
    dplyr::rename(countryname_iso = "ISO 3 Letter Code",
                  countryname_eng = "SURVEYED COUNTRY",
                  data_year_info = "YEAR OF DATA",
                  staff_rangers_others_known = "R_BOTH \nWHERE BOTH RANGER AND NON RANGER NUMBERS ARE LISTED\nNUMBERS WHO ARE SPECIFICALLY IDENTIFIED AS RANGERS",
                  staff_others_rangers_known = "NR_BOTH\n  NUMBERS WHERE BOTH RANGER AND NON RANGER NUMBERS ARE LISTED\nNUMBERS WHO ARE SPECIFICALLY IDENTIFIED AS NON RANGERS",
                  staff_rangers_others_unknown = "R_ONLY\n NUMBERS WHERE ONLY RANGERS ARE LISTED (NON RANGER NUMBERS NOT PROVIDED)\nNUMBERS WHO ARE SPECIFICALLY IDENTIFIED AS RANGERS",
                  staff_total_details_unknown = "RNR_UND\nNUMBERS PROVIDED AS AN UNDIFFERENTIATED TOTAL (COMBINED RANGERS AND NON RANGERS)",
                  staff_total = "RNRT_1\nTOTAL SURVEYED STAFF  (SUM D,E,F,G)",
                  area_PA_surveyed = "AREA OF PROTECTED AREAS SURVEYED IN STAFF SURVEY SQ KM",
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
                  GPD_growth = "GDP_growth",
                  unemployment = "Unemployment_total",
                  rural_pct = "Rural_population_percentage",
                  income = "Adjusted net national income per capita",
                  pop_density = "Population Density",
                  IUCN_1_4_prop = "PROPORTION IUCN CAT (I-IV)/(I-VI)",
                  IUCN_1_2_prop = "PROPORTION IUCN CAT (I-II)/(I-VI)",
                  notes = "Notes")

}


#' Ranger data
#'
#' This object contain the data about rangers and other staffs members working in protected areas.
#'
#' @seealso `fetch_data_rangers()` for the function used to create such a dataset
#'
#' @examples
#' data_rangers
#'
"data_rangers"
