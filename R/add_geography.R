#' Adds geography fields from the Scottish Statistics Postcode Lookup
#'
#' @description
#' Adds relevant geography code fields from the Scottish Statistics Postcode
#' Lookup for all Social Security Scotland official statistics publications. As
#' this lookup does not contain the local authority name and other geography
#' code-names required for official statistics publications, we need to join
#' the local authority names from the data zone 2022 lookup.
#'
#' To add Scottish Index of Multiple Deprivation columns, the 2011 data zone
#' code is also needed and this is obtained from the Scottish Statistics
#' Postcode Lookup file.
#'
#' After cleaning the postcodes, the geography fields are added to the
#' target data frame by joining on the postcode column.
#'
#' There are some postcodes that cannot be matched to any version of the
#' Scottish Statistics Postcode lookup, and thus those postcodes cannot be
#' assigned to the relevant geography area (e.g. a local authority area etc.)
#' Therefore, postcodes would be assigned to:
#'
#' - 'Unknown - Scottish postcode' if its postcode area is identified as
#'    Scottish postcode
#' - 'Unknown - Non-Scottish postcode' if it meets the UK standard postcode,
#'  but its postcode area is not identified as Scottish postcode
#' - 'Unknown - Others' if it does not meet the UK standard postcode.
#'
#' For further explanation on the structure of a typical UK postcode:
#'  https://www.ons.gov.uk/methodology/geography/ukgeographies/postalgeography.
#'
#' For further explanation on the structure of a typical Scottish postcode:
#'  https://www.nrscotland.gov.uk/publications/geography-postcode-information-note/.
#'
#' The three lookup files can be obtained by using functions from sssstats
#' package: `get_sspl_lookup()`; `get_datazone_lookup()`; `get_simd_lookup()`.
#'
#' @importFrom rlang enquo .data
#' @importFrom dplyr mutate select left_join filter distinct pull case_when if_else
#' @importFrom stringr str_replace_all str_replace str_extract str_detect str_to_upper str_trim
#' @importFrom tidyselect all_of
#' @param input_data dataset containing the postcode column
#' @param sspl_lookup the sspl_lookup dataframe
#' @param datazone_lookup the datazone lookup dataframe
#' @param simd_lookup the simd lookup dataframe
#' @param postcode_column column name containing postcode
#' @return a data frame with geography fields added.
#' @seealso
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

  geo_clean_postcode(input_data, !!postcode_column)|>
    geo_add_columns(sspl_lookup,
                    datazone_lookup,
                    simd_lookup)

}
