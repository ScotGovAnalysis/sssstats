test_that("add_geography works", {
  # first collect all the lookups
  file_path <- testthat::test_path("sspl_data", "singlerecord_mock.csv")

  sspl_lookup <- get_sspl_lookup(file_path)
  datazone_lookup <- get_datazone_lookup("2022")
  simd_lookup <- get_simd_lookup()

  test_data <- data.frame(
    postcode = c(
      "AB1 0AA",
      "AB1          0AB",
      "AB10AD",
      "  AB1 0AE  ",
      "AB1 0AF",
      "CA17 9UB",
      "WA15 6NL",
      "CH41 4DS",
      "AB34 999",
      "CA6 999",
      "ABB LAH",
      "",
      NA_character_,
      "AB1 0AC"
    ),
    expected_la = c(
      "Aberdeen City",
      "Aberdeen City",
      "Aberdeen City",
      "Aberdeenshire",
      "Aberdeen City",
      "Unknown - Non-Scottish postcode",
      "Unknown - Non-Scottish postcode",
      "Unknown - Non-Scottish postcode",
      "Unknown - Other",
      "Unknown - Other",
      "Unknown - Other",
      "Unknown - Other",
      "Unknown - Other",
      "Unknown - Scottish postcode"
    ),
    expected_pc = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE
    )
  )

  postcodes_result <- add_geography(
    test_data,
    sspl_lookup,
    datazone_lookup,
    simd_lookup,
    postcode
  )

  # check if result is a dataframe
  expect_true(is.data.frame(postcodes_result))

  # check valid UK postcode validation
  expect_equal(postcodes_result$valid_uk_postcode, test_data$expected_pc)

  # check local authority area code-name
  expect_equal(postcodes_result$la_name, test_data$expected_la)

  # check that relevant code/code-name are added as expected
  key_code_names <- c("valid_uk_postcode",
                      "postcode_formatted",
                      "data_zone2022code",
                      "la_code",
                      "la_name",
                      "hb_code",
                      "hb_name",
                      "ur8_code",
                      "ur8_name",
                      "island_code")

  expect_equal(all(key_code_names %in% colnames(postcodes_result)), TRUE)
})
