#' add_geography Adds geography fields from the Scottish Statistics Postcode Lookup
#'
#' @description
#' Selects relevant geography fields from the Scottish Statistics Postcode
#' Lookup for all Social Security Scotland official statistics publications.
#' As this lookup does not contain the local authority name, we need to join
#' the local authority names from the data zone 2022 lookup. To add Scottish
#' Index of Multiple Deprivation columns the 2011 datazones are needed and these
#' are obtained from the sspl lookup file.
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
#' The three lookup files can be obtained by using functions from sssstats package
#' get_sspl_lookup();get_datazone_lookup();get_simd_lookup()
#'
#' @importFrom rlang enquo
#' @importFrom dplyr mutate select left_join filter distinct pull case_when if_else
#' @importFrom stringr str_replace_all str_replace str_extract str_detect str_to_upper str_trim
#' @importFrom tidyselect all_of
#' @param input_data dataset containing the postcode column
#' @param sspl_lookup the sspl_lookup dataframe
#' @param datazone_lookup the datazone lookup dataframe
#' @param simd_lookup the simd lookup dataframe
#' @param postcode_column column name containing postcode

#' @return data frame with geography fields added
#' @export

add_geography <- function(input_data,
                          sspl_lookup,
                          datazone_lookup,
                          simd_lookup = NULL,
                          postcode_column) {
  postcode_column <- rlang::enquo(postcode_column)

  geo_clean_postcode(input_data, !!postcode_column) |>
    geo_add_columns(
      sspl_lookup,
      datazone_lookup,
      simd_lookup
    )
}
