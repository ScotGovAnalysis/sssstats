test_that("lookup table for simd made", {
  simd_data <- get_simd_lookup()

  # check if result is a dataframe
  expect_true(is.data.frame(simd_data))

  # check that there are 6,976 rows
  expect_equal(nrow(simd_data), 6976)
})
