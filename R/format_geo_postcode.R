#' Format the column containing postcodes
#'
#' @description
#' This function takes a data frame and a column containing postcodes, formats
#' the postcodes, validates them against a regex pattern for the standard UK
#' postcode format, and returns the original data with two additional columns:
#' \itemize{
#'   \item `postcode_formatted`: The cleaned and properly spaced postcode (or `NA` if invalid).
#'   \item `valid_uk_postcode`: A logical flag indicating whether the formatted postcode meets the standard UK postcode format or not.
#' }
#'
#' @param input_data A data frame containing the postcode column.
#' @param postcode_column The unquoted name of the column in `input_data` that contains postcodes.
#'
#' @details
#' The UK postcodes are liable to change over time and thus this function does
#' not confirm whether a given postcode does exist or not. Instead, this
#' function only assess whether the given postcode is consistent with the regex
#' pattern for the standard UK postcode format. This is not intended as a
#' comprehensive stand-alone postcode cleaning function, but provides the
#' minimum required for joining with the Scottish Statistics Postcode Lookup.
#'
#' This function:
#' \enumerate{
#'   \item Converts postcodes to uppercase and trims whitespace.
#'   \item Removes extra spaces and inserts a space before the last three characters if length >= 5.
#'   \item Validates the formatted postcode against a UK postcode regex pattern.
#' }
#' This function is called inside the `add_geography()` function.
#'
#' For further explanation on the structure of a typical UK postcode:
#'  https://www.ons.gov.uk/methodology/geography/ukgeographies/postalgeography.
#'
#' For further explanation on the structure of a typical Scottish postcode:
#'  https://www.nrscotland.gov.uk/publications/geography-postcode-information-note/.
#'
#' @importFrom rlang .data
#' @return A data frame with two additional columns: `valid_uk_postcode` and `formatted_postcode`
#' @seealso
#' * [add_geo_columns()] add the geography fields from the Scottish Statistics Postcode Lookup.
#' @examples
#'  \dontrun{library(dplyr)
#' test_data <- tibble(
#'   id = 1:4,
#'   postcode = c("AB1 0AA", "ab10ab", "INVALID", NA)
#' )
#' format_geo_postcode(test_data, postcode)
#' }
#' @export

format_geo_postcode <- function(input_data, postcode_column) {
  postcode_column <- rlang::enquo(postcode_column)

  # Regular expression for a standard UK postcode
  uk_postcode_regex <- "^([A-Z][A-HJ-Y]?\\d[A-Z\\d]? ?\\d[A-Z]{2}|GIR ?0A{2})$"

  # cleans and formats the postcode properly for the input_data object
  input_data_formatted <- input_data |>
    dplyr::mutate(
      raw_postcode = !!postcode_column,
      postcode_clean = (.data$raw_postcode) |>
        stringr::str_to_upper() |>
        stringr::str_trim() |>
        stringr::str_replace_all("\\s+", ""),
      temp_postcode_formatted = dplyr::case_when(
        is.na(.data$raw_postcode) | stringr::str_trim(.data$raw_postcode) == "" ~ NA_character_,
        nchar(.data$postcode_clean) >= 5 ~ stringr::str_replace(
          .data$postcode_clean,
          "(.+)(.{3})$", "\\1 \\2"
        ),
        TRUE ~ .data$postcode_clean
      ),
      valid_uk_postcode = dplyr::if_else(
        !is.na(.data$temp_postcode_formatted),
        stringr::str_detect(.data$temp_postcode_formatted, uk_postcode_regex),
        FALSE
      ),
      postcode_formatted = dplyr::if_else(.data$valid_uk_postcode == TRUE,
                                          .data$temp_postcode_formatted,
                                          NA_character_)
    ) |>
    dplyr::select(
      -"raw_postcode",
      -"postcode_clean",
      -"temp_postcode_formatted"
    )

  input_data_formatted
}
