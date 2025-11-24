geo_add_columns <- function(input_data_formatted,
                            sspl_lookup,
                            datazone_lookup = NULL,
                            simd_lookup = NULL) {

  if (!is.null(datazone_lookup)) {
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

    sspl_lookup <- sspl_lookup |>
      dplyr::left_join(dz_2022_lookup,
        by = c("data_zone2022code" = "dz22_code")
      )
  }

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
