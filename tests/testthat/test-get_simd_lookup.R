test_that("lookup table for simd made", {
  simd_data <- get_simd_lookup()

  # check if result is a dataframe
  expect_true(is.data.frame(simd_data))

  # check at least 5000 rows of data
  expect_gte(nrow(simd_data), 5000)
})
