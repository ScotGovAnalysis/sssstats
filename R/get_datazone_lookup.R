#' Creates the data zone lookup from statistics.gov.scot
#' @description Function using data.gov.scot
#'  to get the correct datazone file.
#' @param census_year A specific Census year ("2011" or "2022")in which the Scottish data zones are derived from.
#' @return A data frame.
#' @seealso
#' * [get_simd_lookup()] gets the Scottish Index of Multiple Deprivation lookup.
#' * [get_sspl_lookup()] gets the Scottish Statistics Postcode Lookup.
#' @examples
#' \dontrun{
#' datazone_2022_lookup <- get_datazone_lookup("2022")
#' datazone_2011_lookup <- get_datazone_lookup("2011")
#' }
#' @export

get_datazone_lookup <- function(census_year) {
  # Checks parameter is valid
  if (!census_year %in% c("2022", "2011")) {
    stop("Census_year not recognised for the Scottish data zone lookup.
         Only '2011' and '2022' are valid for this function.")
  }

  # sets the url
  url_2022 <- "https://data.gov.scot/dataset/2022_data_zone_lookup/resource/81b010f5-0207-4ee5-bd2d-e7821aa6a3dc/download"
  url_2011 <- "https://data.gov.scot/dataset/2011_data_zone_lookup/resource/20ad5f65-c5c2-407e-8355-7c25bf579e87/download"

  url <- dplyr::case_when(
    census_year == "2022" ~ url_2022,
    census_year == "2011" ~ url_2011,
  )

  # downloads the data
  resp <- httr2::request(url) |>
    httr2::req_user_agent("Package sssstats (https://github.com/ScotGovAnalysis/sssstats)") |>
    httr2::req_perform() |>
    httr2::resp_body_raw() |>
    readr::read_csv(show_col_types = FALSE) |>
    janitor::clean_names()
}
