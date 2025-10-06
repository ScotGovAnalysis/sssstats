test_that("get_sspl_lookup works", {
  file_path <- testthat::test_path("sspl_data", "singlerecord_mock.csv")
  sspl_result <- get_sspl_lookup(file_path)

  expect_equal(nrow(sspl_result), 10)
  expect_equal(ncol(sspl_result), 9)
})

test_that("get_sspl_lookup errors correctly", {
  file_path_bad <- paste0("wrong/filepath/singlerecord_mock.csv")

  expect_error(get_sspl_lookup(file_path_bad))
})

test_that("get_sspl_lookup returns full", {
  file_path <- testthat::test_path("sspl_data", "singlerecord_mock.csv")
  sspl_result <- get_sspl_lookup(file_path)
  sspl_result_full <- get_sspl_lookup(file_path, keep_all = TRUE)

  expect_gte(ncol(sspl_result_full), 50)
})
