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
                          simd_lookup,
                          postcode_column) {
  postcode_column <- rlang::enquo(postcode_column)

  # Regular expression for a standard UK postcode
  uk_postcode_regex <- "^([A-Z][A-HJ-Y]?\\d[A-Z\\d]? ?\\d[A-Z]{2}|GIR ?0A{2})$"

  # cleans and formats the postcode properly for the input_data object
  input_data_formatted <- input_data |>
    dplyr::mutate(
      raw_postcode = !!postcode_column,
      postcode_clean = stringr::str_replace_all(
        stringr::str_to_upper(stringr::str_trim(raw_postcode)),
        "\\s+", ""
      ),
      temp_postcode_formatted = dplyr::case_when(
        is.na(raw_postcode) | stringr::str_trim(raw_postcode) == "" ~ NA_character_,
        nchar(postcode_clean) >= 5 ~ stringr::str_replace(
          postcode_clean,
          "(.+)(.{3})$", "\\1 \\2"
        ),
        TRUE ~ postcode_clean
      ),
      valid_uk_postcode = dplyr::if_else(
        !is.na(temp_postcode_formatted),
        stringr::str_detect(temp_postcode_formatted, uk_postcode_regex),
        FALSE
      ),
      postcode_formatted = dplyr::if_else(valid_uk_postcode == TRUE,
        temp_postcode_formatted,
        NA_character_
      )
    ) |>
    dplyr::select(
      -postcode_clean,
      -temp_postcode_formatted,
      -raw_postcode
    )

  # Uses the data zone 2022 lookup to get the following code and code-names:
  # - Local authority area (la_name and la_code)
  # - Health board (hb_code and hb_name)
  # - Urban rural classification 8-fold (ur8_code and ur8_name).
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

  # As the current version of Scottish Index of Multiple Deprivation (SIMD) code
  # is based on the 2011 data zone code, uses the SIMD lookup to get the
  # quintile and decile values.
  simd_lookup <- simd_lookup |>
    dplyr::select(
      ref_area,
      simd_2020_quintile,
      simd_2020_decile
    )

  # Adds columns into the Scottish Statistics Postcode Lookup
  sspl_lookup <- sspl_lookup |>
    dplyr::left_join(dz_2022_lookup,
      by = c("data_zone2022code" = "dz22_code")
    ) |>
    dplyr::left_join(simd_lookup,
      by = c("data_zone2011code" = "ref_area")
    )

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
