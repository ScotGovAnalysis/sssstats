#' Creates the Scottish Index of Multiple Deprivation (data zone 2011) lookup
#'
#' @description Uses data.gov.scot to get the simd lookup required
#' for the Social Security Scotland official statistics publications.
#'
#' The previous data platform returned `ref_area` not `geography_code`. A column called `ref_area`
#' has been made to prevent breaking existing functions.
#'
#' @return A data frame.
#' @seealso
#' * [get_datazone_lookup()] gets the data zone lookup.
#' * [get_sspl_lookup()] gets the Scottish Statistics Postcode Lookup.
#' @examples
#' \dontrun{
#' simd_lookup <- get_simd_lookup()
#' }
#' @export

get_simd_lookup <- function() {
  # sets url for download
  url <- "https://data.gov.scot/dataset/scottish_index_of_multiple_deprivation_2020/resource/0786efac-2a01-404e-9966-e99e86a24950/download"

  # downloads the data
  resp <- httr2::request(url) |>
    httr2::req_user_agent("Package sssstats (https://github.com/ScotGovAnalysis/sssstats)") |>
    httr2::req_perform()

  # makes a dataframe from the response data
  resp |>
    httr2::resp_body_string() |>
    readr::read_csv(show_col_types = FALSE) |>

    # sets up the data how we want it
    janitor::clean_names(case = "snake") |>
    dplyr::filter(simd_domain == "SIMD") |>
    tidyr::pivot_wider(
      id_cols = geography_code,
      names_from = c(simd_domain, date_code, measurement),
      names_sep = "_",
      values_from = value
    ) |>
    janitor::clean_names(case = "snake") |>
    dplyr::select(
      geography_code,
      simd_2020_rank,
      simd_2020_quintile,
      simd_2020_decile,
      simd_2020_vigintile,
      ref_area = geography_code,
    ) |>
    dplyr::distinct(geography_code, .keep_all = TRUE)
}
