#' Adds geography columns from the Scottish Statistics Postcode Lookup
#'
#'@description
#' This function takes a data frame along with the \code{postcode_formatted}
#' column and adds relevant geography code fields from the Scottish Statistics
#' Postcode Lookup for all Social Security Scotland official statistics
#' publications.
#'
#' @param input_data_formatted A data frame containing the input data. Must include:
#'   - `postcode_formatted`: A cleaned UK postcode column.
#'   - `valid_uk_postcode`: A logical column created by the `clean_geo_postcode` function.
#'
#' @param sspl_lookup A data frame containing the Scottish Statistics Postcode Lookup.
#' @param datazone_lookup A data frame for the data zone lookup.
#' @param simd_lookup (Optional) A data frame for SIMD 2020 lookup.
#'
#' @details
#' It is mandatory that the `clean_geo_postcode` function is run before the `add_geo_columns` function.
#'
#' This function enriches an input dataset with relevant geographic information
#' by joining Scottish Statistics Postcode Lookup (SSPL) data. As the SSPL file
#' does not contain the local authority code-name and other geography
#' code-names, we get those code-names from the data zone 2022 lookup. From `datazone_lookup` the following fields are added:
#' \itemize{
#'   \item \code{la_name, la_code}: Local authority area
#'   \item \code{hb_code, hb_name}: Health board
#'   \item \code{ur8_code, ur8_name}: Urban/rural classification 8-fold.
#'   }
#'
#' Optionally, we also get the Scottish Index of Multiple Deprivation (SIMD)
#' 2020 columns. If `simd_lookup` is provided, the quintile and decile fields are added from the SIMD 2020 lookup. Note that the 2020 version of SIMD is based on the 2011 data zone code.
#'
#' This function also assigns descriptive labels for unmatched postcodes:
#' - 'Unknown - Scottish postcode' if its postcode area is identified as
#'    Scottish postcode
#' - 'Unknown - Non-Scottish postcode' if it meets the standard UK postcode format, but its postcode area is not identified as Scottish postcode
#' - 'Unknown - Others' if it does not meet the standard UK postcode format.
#'
#' The three lookup files can be obtained by using functions from sssstats package
#' `get_sspl_lookup()`; `get_datazone_lookup()`; `get_simd_lookup()`.
#'
#' This function is called inside the `add_geography` function.
#'
#' @return A data frame with additional columns:
#'   - Local authority name/code
#'   - Health board name/code
#'   - Urban/rural classification
#'   - (Optional) SIMD 2020 quintile and decile.
#' @seealso
#' * [clean_geo_postcode()] cleans the column containing postcodes within a data frame.
#' * [get_datazone_lookup()] gets the data zone lookup.
#' * [get_simd_lookup()] gets the Scottish Index of Multiple Deprivation lookup.
#' * [get_sspl_lookup()] gets the Scottish Statistics Postcode Lookup.
#' @examples
#' \dontrun{
#' mock_input_data <- data.frame(postcode =
#'   c("AB39 2HP",
#'     "FK2     9BB",
#'     "G20 7XN",
#'     "G27XR",
#'     "CA17 9UB",
#'     "WA15 6NL",
#'     "",
#'     NA_character_
#'     )) |>
#'     clean_geo_postcode(postcode)
#'
#' datazone_2022_lookup <- get_datazone_lookup("2022")
#' simd_lookup <- get_simd_lookup()
#' sspl_lookup <- get_sspl_lookup("filepath/to/SSPL CSV file")
#' add_geo_columns(mock_input_data, sspl_lookup, datazone_2022_lookup, simd_lookup, postcode_formatted)
#' }
#'
#' @import dplyr stringr
#' @export


add_geo_columns <- function(input_data_formatted,
                            sspl_lookup,
                            datazone_lookup,
                            simd_lookup = NULL) {

  if (!("postcode_formatted" %in% colnames(input_data_formatted))) {
    stop(paste(
      "'postcode_formatted' column does not exist within input_data_formatted.",
      "Did you run `clean_geo_postcode()` first?"
      ))
  }

  dz_2022_lookup <- datazone_lookup |>
      dplyr::select(
        "dz22_code",
        "la_code",
        "la_name",
        "hb_code",
        "hb_name",
        "ur8_code",
        "ur8_name"
      )

  sspl_lookup <- sspl_lookup |>
      dplyr::left_join(dz_2022_lookup,
        by = c("data_zone2022code" = "dz22_code")
      )

  if (!is.null(simd_lookup)) {
    simd_lookup <- simd_lookup |>
      dplyr::select(
        "ref_area",
        "simd_2020_quintile",
        "simd_2020_decile"
      )

    sspl_lookup <- sspl_lookup |>
      dplyr::left_join(simd_lookup,
        by = c("data_zone2011code" = "ref_area")
      )
  } else {
    message("The SIMD lookup is not supplied.")
  }

  # Makes a list of Scottish postcode areas, excluding "CA".
  scottish_postcode_area <- sspl_lookup |>
    dplyr::mutate(scottish = stringr::str_extract(
      .data$postcode_district,
      "^[A-Z]+"
    )) |>
    dplyr::distinct(.data$scottish) |>
    dplyr::filter(.data$scottish != "CA") |>
    dplyr::pull(.data$scottish)

  # Adds all necessary geography fields into the input_data object
  data_with_sspl <- input_data_formatted |>
    dplyr::left_join(sspl_lookup,
      by = c("postcode_formatted" = "postcode")
    )

  # adds descriptive labels for postcodes that cannot be matched to the
  # Scottish Statistics Postcode Lookup
  output_data <- data_with_sspl |>
    dplyr::mutate(
      dplyr::across(
        .cols = c("la_name", "hb_name", "ur8_name"),
        .fns = ~ dplyr::case_when(
          !is.na(.x) ~ .x,
          .data$valid_uk_postcode &
            stringr::str_extract(.data$postcode_formatted, "^[A-Z]+")
            %in% scottish_postcode_area ~ "Unknown - Scottish postcode",
          .data$valid_uk_postcode ~ "Unknown - Non-Scottish postcode",
          TRUE ~ "Unknown - Other"
        )
      )
    )

  output_data
}
