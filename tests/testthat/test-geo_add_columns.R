test_that("geo_add_columns with optional lookups", {
  test_data <- tibble::tibble(
    postcode_formatted = c(
      "AB39 2HP", "FK2 9BB", "G20 7XN", "G2 7XR",
      "TD5 7LQ", "KA26 9SD", "EH12 7HX", "IV51 9TL",
      "DG2 0RG", "CA6 5DD", "CA17 9UB", "WA15 6NL",
      "CH41 4DS", NA, NA, NA,
      NA, NA, NA, "AB1 0AC",
      "ZE3 9XZ"
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
      FALSE,
      TRUE,
      TRUE
    ),
    expected_la = c(
      "Aberdeenshire",
      "Falkirk",
      "Glasgow City",
      "Glasgow City",
      "Scottish Borders",
      "South Ayrshire",
      "City of Edinburgh",
      "Highland",
      "Dumfries and Galloway",
      "Dumfries and Galloway",
      "Unknown - Non-Scottish postcode",
      "Unknown - Non-Scottish postcode",
      "Unknown - Non-Scottish postcode",
      "Unknown - Other",
      "Unknown - Other",
      "Unknown - Other",
      "Unknown - Other",
      "Unknown - Other",
      "Unknown - Other",
      "Unknown - Scottish postcode",
      "Unknown - Scottish postcode"
    )
  )

  # setup for the sspl file
  config <- config::get()
  folder_path <- config$sspl_folder
  file_path <- paste0(folder_path, "archive/singlerecord_2025_1.csv")

  # collect all the lookups
  sspl_lookup <- get_sspl_lookup(file_path)
  datazone_lookup <- get_datazone_lookup("2022")
  simd_lookup <- get_simd_lookup()

  postcodes_result <- geo_add_columns(
    test_data,
    sspl_lookup,
    datazone_lookup = datazone_lookup,
    simd_lookup = simd_lookup
  )

  # check if result is a dataframe
  expect_true(is.data.frame(postcodes_result))

  # check local authority area code-name
  expect_equal(postcodes_result$la_name, test_data$expected_la)

  # check simd
  expect_equal(postcodes_result$simd_2020_quintile, c(
    4, 2, 2, 3, 2, 3, 5, 3, 3,
    2, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA
  ))
  # check datazone
  expect_equal(postcodes_result$data_zone2022code, c(
    "S01013825", "S01016244",
    "S01017453", "S01017411",
    "S01019666", "S01019754",
    "S01015044", "S01017854",
    "S01015243", "S01015355",
    NA, NA,
    NA, NA,
    NA, NA,
    NA, NA,
    NA, NA,
    NA
  ))
})

test_that("geo_add_columns works without optional lookup", {
  test_data2 <- tibble::tibble(
    postcode_formatted = c(
      "AB39 2HP", "FK2 9BB", "G20 7XN", "G2 7XR",
      "TD5 7LQ", "KA26 9SD", "EH12 7HX", "IV51 9TL",
      "DG2 0RG", "CA6 5DD", "CA17 9UB", "WA15 6NL",
      "CH41 4DS", NA, NA, NA,
      NA, NA, NA, "AB1 0AC",
      "ZE3 9XZ"
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
      FALSE,
      TRUE,
      TRUE
    ),
    expected_la = c(
      "Aberdeenshire",
      "Falkirk",
      "Glasgow City",
      "Glasgow City",
      "Scottish Borders",
      "South Ayrshire",
      "City of Edinburgh",
      "Highland",
      "Dumfries and Galloway",
      "Dumfries and Galloway",
      "Unknown - Non-Scottish postcode",
      "Unknown - Non-Scottish postcode",
      "Unknown - Non-Scottish postcode",
      "Unknown - Other",
      "Unknown - Other",
      "Unknown - Other",
      "Unknown - Other",
      "Unknown - Other",
      "Unknown - Other",
      "Unknown - Scottish postcode",
      "Unknown - Scottish postcode"
    )
  )

  # setup for the sspl file
  config <- config::get()
  folder_path <- config$sspl_folder
  file_path <- paste0(folder_path, "archive/singlerecord_2025_1.csv")

  # collect all the lookups
  sspl_lookup <- get_sspl_lookup(file_path)
  datazone_lookup <- get_datazone_lookup("2022")


  postcodes_result2 <- geo_add_columns(
    test_data2,
    sspl_lookup,
    datazone_lookup
  )

  # check if result is a dataframe
  expect_true(is.data.frame(postcodes_result2))

  # check local authority area code-name
  expect_equal(postcodes_result2$la_name, test_data2$expected_la)

  # check simd
  expect_message(geo_add_columns(
    test_data2,
    sspl_lookup,
    datazone_lookup
  ), "No simd_lookup supplied")
  # check datazone
  expect_equal(postcodes_result2$data_zone2022code, c(
    "S01013825", "S01016244",
    "S01017453", "S01017411",
    "S01019666", "S01019754",
    "S01015044", "S01017854",
    "S01015243", "S01015355",
    NA, NA,
    NA, NA,
    NA, NA,
    NA, NA,
    NA, NA,
    NA
  ))
})
