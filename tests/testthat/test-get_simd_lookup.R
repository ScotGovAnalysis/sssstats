test_that("lookup table for simd made", {
  # `get_simd_lookup` relies on a live SPARQL endpoint and then
  # downloads the file from its URL -- this would fail some GitHub Actions
  # for R.
  testthat::skip_on_ci()

  simd_data <- get_simd_lookup()

  # check if result is a dataframe
  expect_true(is.data.frame(simd_data))

  # check at least 5000 rows of data
  expect_gte(nrow(simd_data), 5000)
})
