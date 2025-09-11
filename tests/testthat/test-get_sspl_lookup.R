test_that("get_sspl_lookup works", {
  # This test will fail without a config file
  config <- config::get()

  folder_path <- config$sspl_folder
  file_path <- paste0(folder_path, "archive/singlerecord_2025_1.csv")
  sspl_result <- get_sspl_lookup(file_path)

  expect_equal(length(unique(sspl_result$data_zone2022code)), 7392)
  expect_equal(length(unique(sspl_result$intermediate_zone2022code)), 1334)
  expect_equal(length(unique(sspl_result$council_area2019code)), 32)
  expect_equal(ncol(sspl_result), 9)
})

test_that("get_sspl_lookup errors correctly", {
  config <- config::get()

  folder_path <- config$sspl_folder
  file_path_bad <- paste0(folder_path, "archive/singlerecord_test.csv")

  expect_error(get_sspl_lookup(file_path_bad))
})

test_that("get_sspl_lookup returns full ", {
  config <- config::get()
  folder_path <- config$sspl_folder
  file_path <- paste0(folder_path, "archive/singlerecord_2025_1.csv")
  sspl_result_full <- get_sspl_lookup(file_path, keep_all=TRUE)

  expect_equal(ncol(sspl_result_full), 50)
})
