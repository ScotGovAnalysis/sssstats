test_that("add_geography works", {
  test_data <- tibble::tibble(
    postcode = c(
      "AB39 2HP",
      "FK2     9BB",
      "G20 7XN",
      "G27XR",
      "TD5 7LQ",
      "    KA26 9SD",
      "  EH12   7HX  ",
      "IV519TL",
      "DG2 0RG",
      "CA6 5DD",
      "CA17 9UB",
      "WA15 6NL",
      "CH41 4DS",
      "AB34 999",
      "CA6 999",
      "IRELAND",
      "ABB LAH",
      "",
      NA_character_,
      "AB1 0AC",
      "ZE3 9XZ"
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
})
