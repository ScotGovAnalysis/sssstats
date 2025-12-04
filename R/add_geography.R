#' Adds geography fields from the Scottish Statistics Postcode Lookup
#'
#' @description
#' This function takes a data frame, and run both `format_geo_postcode` and
#' `add_geo_columns` functions to add relevant geography code fields from
#' the Scottish Statistics Postcode Lookup for all Social Security
#' Scotland official statistics publications.
#'
#' @importFrom rlang enquo
#' @param input_data dataset containing the postcode column
#' @param sspl_lookup the sspl_lookup dataframe
#' @param datazone_lookup the datazone lookup dataframe
#' @param simd_lookup the simd lookup dataframe
#' @param postcode_column column name containing postcode
#'
#' @details
#' Prior to `sssstats` version 0.2.1 which both `format_geo_postcode()` and
#' `add_geo_columns()` functions were created, this function did what the
#' combination of the latter functions do.
#'
#' @return a data frame with geography fields added.
#' @seealso
#' * [format_geo_postcode()] formats the column containing postcodes within a data frame.
#' * [add_geo_columns()] add the geography fields from the Scottish Statistics Postcode Lookup.
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
#'     ))
#'
#' datazone_2022_lookup <- get_datazone_lookup("2022")
#' simd_lookup <- get_simd_lookup()
#' sspl_lookup <- get_sspl_lookup("filepath/to/SSPL CSV file")
#' add_geography(mock_input_data, sspl_lookup, datazone_2022_lookup, simd_lookup, postcode)
#' }
#'
#' @export

add_geography <- function(input_data,
                          sspl_lookup,
                          datazone_lookup,
                          simd_lookup = NULL,
                          postcode_column) {

  postcode_column <- rlang::enquo(postcode_column)

  format_geo_postcode(input_data, !!postcode_column) |>
    add_geo_columns(
      sspl_lookup,
      datazone_lookup,
      simd_lookup
    )

}
