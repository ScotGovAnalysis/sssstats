#' geo_add_columns adds Geographic Columns to a Dataset
#'
#' @param input_data_formatted A data frame containing the input data. Must include:
#'   - `postcode_formatted`: A cleaned UK postcode column.
#'   - `valid_uk_postcode`: A logical column created by geo_clean_postcode.
#'
#' @param sspl_lookup A data frame containing the Scottish Statistics Postcode Lookup.
#' @param datazone_lookup A data frame for Data Zone 2022 lookup gives the la_name.
#' @param simd_lookup (Optional) A data frame for SIMD 2020 lookup.
#'
#' @details
#' The geo_ prefix indicates that the function is designed to be used after
#'  cleaning the postcode with geo_clean_postcode.
#' This function contains code that used to be in add_geography.
#' It has been removed from add_geography to allow cleaning to be carried out
#' separately to the addition of geography columns such as local authority name.
#'
#' This function enriches an input dataset with geographic information by joining
#' Scottish Statistics Postcode Lookup (SSPL) data and optional lookups for
#' Data Zones and SIMD. It also assigns descriptive labels for
#' unmatched postcodes.
#' - From `datazone_lookup` the following fields are added:
#' \itemize{
#'   \item \code{la_name ,la_code}: Local authority area
#'   \item \code{hb_code, hb_name}: Health Board
#'   \item \code{ur8_code, ur8_name}: Urban rural classification 8-fold
#'   }
#' - If `simd_lookup` is provided, SIMD quintile and decile fields are added.
#' - Scottish postcode areas are identified dynamically (excluding "CA").
#' - For unmatched postcodes, descriptive labels are assigned based on whether the
#'   postcode is Scottish, non-Scottish, or invalid.
#'
#' The three lookup files can be obtained by using functions from sssstats package
#' get_sspl_lookup();get_datazone_lookup();get_simd_lookup()
#'
#'
#' @return A data frame with additional columns:
#'   - Local authority name/code
#'   - Health board name/code
#'   - Urban/rural classification
#'   - SIMD quintile and decile (if available)
#'
#' @examples
#' \dontrun{
#' my_data_with_geography <- geo_add_columns(
#'   input_data_formatted = my_data,
#'   sspl_lookup = sspl_lookup,
#'   datazone_lookup = dz_lookup,
#'   simd_lookup = simd_lookup
#' )
#' }
#'
#' @import dplyr stringr
#' @export


geo_add_columns <- function(input_data_formatted,
                            sspl_lookup,
                            datazone_lookup,
                            simd_lookup = NULL) {

    dz_2022_lookup <- datazone_lookup |>
      dplyr::select(
        dz22_code,
        la_code,
        la_name,
        hb_code,
        hb_name,
        ur8_code,
        ur8_name
      )
    sspl_lookup<- sspl_lookup|>
      dplyr::left_join(dz_2022_lookup,
        by = c("data_zone2022code" = "dz22_code")
      )

  if (!is.null(simd_lookup)) {
    # As the current version of Scottish Index of Multiple Deprivation (SIMD) code
    # is based on the 2011 data zone code, uses the SIMD lookup to get the
    # quintile and decile values.
    simd_lookup <- simd_lookup |>
      dplyr::select(
        ref_area,
        simd_2020_quintile,
        simd_2020_decile
      )
    sspl_lookup <- sspl_lookup |>
      dplyr::left_join(simd_lookup,
        by = c("data_zone2011code" = "ref_area")
      )
  }else{
    message("No simd_lookup supplied")
  }

  # Makes a list of Scottish postcode areas, excluding "CA" as these used to be
  # Scottish before 1998.
  scottish_postcode_area <- sspl_lookup |>
    dplyr::mutate(scottish = stringr::str_extract(
      postcode_district,
      "^[A-Z]+"
    )) |>
    dplyr::distinct(scottish) |>
    dplyr::filter(scottish != "CA") |>
    dplyr::pull(scottish)

  # Adds all necessary geography fields into the input_data object
  data_with_sspl <- input_data_formatted |>
    dplyr::left_join(sspl_lookup,
      by = c("postcode_formatted" = "postcode")
    )

  # adds suitable labels to the "name" columns for postcodes that cannot be
  # matched to the Scottish Statistics Postcode Lookup
  output_data <- data_with_sspl |>
    dplyr::mutate(
      dplyr::across(
        .cols = c(la_name, hb_name, ur8_name),
        .fns = ~ dplyr::case_when(
          !is.na(.x) ~ .x,
          valid_uk_postcode &
            stringr::str_extract(postcode_formatted, "^[A-Z]+")
            %in% scottish_postcode_area ~ "Unknown - Scottish postcode",
          valid_uk_postcode ~ "Unknown - Non-Scottish postcode",
          TRUE ~ "Unknown - Other"
        )
      )
    )
}
