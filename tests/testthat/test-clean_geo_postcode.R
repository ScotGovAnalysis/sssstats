test_that("geo_clean_postcode works", {
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
    expected = c(
      "AB39 2HP", "FK2 9BB", "G20 7XN", "G2 7XR",
      "TD5 7LQ", "KA26 9SD", "EH12 7HX", "IV51 9TL",
      "DG2 0RG", "CA6 5DD", "CA17 9UB", "WA15 6NL",
      "CH41 4DS", NA, NA, NA,
      NA, NA, NA, "AB1 0AC",
      "ZE3 9XZ"
    )
  )

  postcode_result <- clean_geo_postcode(test_data, postcode)

  # check output is as expected
  expect_equal(test_data$expected, postcode_result$postcode_formatted)
})
