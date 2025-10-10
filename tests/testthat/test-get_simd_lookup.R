test_that("lookup table for simd made", {
  # `get_simd_lookup()` relies on a live SPARQL endpoint and then downloads
  # the file from its URL -- this would fail some GitHub Actions for R. So,
  # skipping CI if it comes to this.
  testthat::skip_on_ci()

  simd_data <- get_simd_lookup()

  # check if result is a dataframe
  expect_true(is.data.frame(simd_data))

  # check that there are 6,976 rows
  expect_equal(nrow(simd_data), 6976)
})
