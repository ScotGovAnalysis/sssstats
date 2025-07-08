#' get_simd_lookup Creates simd lookup
#' @description downloads and cleans Scottish Index of Multiple Deprivation data from https://statistics.gov.scot
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @importFrom dplyr filter select
#' @importFrom tidyr unite pivot_wider
#' @return data frame
#' @export

get_simd_lookup <- function() {

  simd_link <- paste0("https://statistics.gov.scot/downloads/",
                      "cube-table?uri=http%3A%2F%2F",
                      "statistics.gov.scot%2Fdata%2F",
                      "scottish-index-of-multiple-deprivation")

  simd <- readr::read_csv(simd_link) |>
    janitor::clean_names() |>
    dplyr::filter(simd_domain == "SIMD") |>
    dplyr::select(feature_code, simd_domain, date_code, measurement, value) |>
    tidyr::unite(
      col = "simd_variable",
      c("simd_domain", "date_code", "measurement"),
      remove = TRUE,
      na.rm = TRUE
    ) |>
    tidyr::pivot_wider(
      names_from = simd_variable,
      values_from = value
    ) |>
    janitor::clean_names()

}
