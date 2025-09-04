#' get_simd_lookup Creates simd lookup
#' @description Uses opendatascot package to get the simd lookup required
#' for the Social Security Scotland official statistics publications.
#' @importFrom opendatascot ods_dataset
#' @importFrom tidyr unite pivot_wider
#' @importFrom janitor clean_names
#' @return data frame
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
