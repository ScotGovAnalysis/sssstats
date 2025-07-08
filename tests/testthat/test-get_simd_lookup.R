test_that("lookup table for simd made", {
  simd_data <- get_simd_lookup()

  # check if result is a data frame
  expect_true(is.data.frame(simd_data))

  # check that there are exactly 6,976 data zone (2011) rows
  expect_equal(nrow(simd_data), 6976)

  # check if there exists feature code, simd_2020_quintile, and simd_2020_decile
  expect_equal(c("feature_code",
                 "simd_2020_quintile",
                 "simd_2020_decile") %in% names(simd_data),
               c(TRUE, TRUE, TRUE))
})
