geo_clean_postcode <- function(input_data, postcode_column) {
  postcode_column <- rlang::enquo(postcode_column)

  # Regular expression for a standard UK postcode
  uk_postcode_regex <- "^([A-Z][A-HJ-Y]?\\d[A-Z\\d]? ?\\d[A-Z]{2}|GIR ?0A{2})$"

  # cleans and formats the postcode properly for the input_data object
  input_data_formatted <- input_data |>
    dplyr::mutate(
      raw_postcode = !!postcode_column,
      postcode_clean = (raw_postcode) |>
        stringr::str_to_upper() |>
        stringr::str_trim() |>
        stringr::str_replace_all("\\s+", ""),
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
      -raw_postcode,
      -postcode_clean,
      -temp_postcode_formatted
    )
}
