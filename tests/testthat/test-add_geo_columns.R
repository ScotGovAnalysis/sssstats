test_that("add_geo_columns with the optional SIMD lookup", {
  # `get_datazone_lookup()` relies on a live SPARQL endpoint and then downloads
  # the file from its URL -- this would fail some GitHub Actions for R. So,
  # skipping CI if it comes to this.
  testthat::skip_on_ci()

  # first collect all the lookups
  file_path <- testthat::test_path("sspl_data", "singlerecord_mock.csv")

  sspl_lookup <- get_sspl_lookup(file_path)
  datazone_lookup <- get_datazone_lookup("2022")
  simd_lookup <- get_simd_lookup()

  test_data <- tibble::tibble(
    postcode_formatted = c(
      "AB1 0AA",
      "AB1 0AB",
      "AB1 0AD",
      "AB1 0AE",
      "AB1 0AF",
      "CA17 9UB",
      "WA15 6NL",
      "CH41 4DS",
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      "AB1 0AC"
    ),
    valid_uk_postcode = c(
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
    expected_dz2022 = c(
      "S01013490",
      "S01013490",
      "S01013490",
      "S01013856",
      "S01013487",
      rep(NA_character_, 9)
    ),
    expected_simd2020 = c(
      5, 5, 5, 4, 5,
      rep(NA_integer_, 9)
    )
  )

  postcode_result <- add_geo_columns(
    test_data,
    sspl_lookup,
    datazone_lookup = datazone_lookup,
    simd_lookup = simd_lookup
  )

  # check if result is a dataframe
  expect_true(is.data.frame(postcode_result))

  # check local authority area code-name
  expect_equal(postcode_result$la_name, test_data$expected_la)

  # check simd
  expect_equal(postcode_result$simd_2020_quintile, test_data$expected_simd2020)
  # check datazone
  expect_equal(postcode_result$data_zone2022code, test_data$expected_dz2022)
})

test_that("add_geo_columns works without the optional SIMD lookup", {
  # `get_datazone_lookup()` relies on a live SPARQL endpoint and then downloads
  # the file from its URL -- this would fail some GitHub Actions for R. So,
  # skipping CI if it comes to this.
  testthat::skip_on_ci()

  # first collect all the lookups
  file_path <- testthat::test_path("sspl_data", "singlerecord_mock.csv")

  sspl_lookup <- get_sspl_lookup(file_path)
  datazone_lookup <- get_datazone_lookup("2022")

  test_data2 <- tibble::tibble(
    postcode_formatted = c(
      "AB1 0AA",
      "AB1 0AB",
      "AB1 0AD",
      "AB1 0AE",
      "AB1 0AF",
      "CA17 9UB",
      "WA15 6NL",
      "CH41 4DS",
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      "AB1 0AC"
    ),
    valid_uk_postcode = c(
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
    expected_dz2022 = c(
      "S01013490",
      "S01013490",
      "S01013490",
      "S01013856",
      "S01013487",
      rep(NA_character_, 9)
    )
  )

  postcode_result2 <- add_geo_columns(
    test_data2,
    sspl_lookup,
    datazone_lookup
  )

  # check if result is a dataframe
  expect_true(is.data.frame(postcode_result))

  # check local authority area code-name
  expect_equal(postcode_result$la_name, test_data$expected_la)

  # check the message for the simd lookup being not used properly
  expect_message(add_geo_columns(
    test_data,
    sspl_lookup,
    datazone_lookup
  ), "The SIMD lookup is not supplied.")

  # check datazone
  expect_equal(postcode_result$data_zone2022code, test_data$expected_dz2022)
})
