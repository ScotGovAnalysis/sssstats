#' get_simd Creates simd lookup
#' @description Uses opendatascot package to get simd data and put it in a nice format.
#' @importFrom opendatascot ods_dataset
#' @importFrom tidyr unite pivot_wider
#' @importFrom dplyr rename mutate
#' @return dataframe
#' @export

get_simd <- function() {
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
    # mutate not rename so the function is compatible with previous code
    # but still keeps the cleaner variable names for future use
    dplyr::mutate(
      dz2011_code = refArea,
      simd2020v2_quintile = simd_2020_quintile
    )
}
