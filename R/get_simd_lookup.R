#' Create the Scottish Index of Multiple Deprivation (data zone 2011) lookup
#' @description Uses opendatascot package to get the simd lookup required
#' for the Social Security Scotland official statistics publications.
#' @importFrom opendatascot ods_dataset
#' @importFrom tidyr unite pivot_wider
#' @importFrom janitor clean_names
#' @return A data frame.
#' @seealso
#' * [get_datazone_lookup()] gets the data zone lookup.
#' * [get_sspl_lookup()] gets the Scottish Statistics Postcode Lookup.
#' * [add_geography()] adds geography fields into the input data.
#' @examples
#' \dontrun{
#' simd_lookup <- get_simd_lookup()
#' }
#' @export

get_simd_lookup <- function() {
  opendatascot::ods_dataset("scottish-index-of-multiple-deprivation",
    simdDomain = "simd"
  ) |>
    tidyr::unite(
      col = "simd_variable",
      c("simdDomain", "refPeriod", "measureType")
    ) |>
    tidyr::pivot_wider(
      names_from = simd_variable,
      values_from = value
    ) |>
    janitor::clean_names(case = "snake")
}
