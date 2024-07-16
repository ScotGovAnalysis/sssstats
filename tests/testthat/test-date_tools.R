test_that("convert_date() converts a date string from default input to default output", {
  expect_equal(convert_date("2024-11"), "November 2024")
})
test_that("convert_date() converts a date string from specified input to default output", {
  expect_equal(convert_date("13/12/03", input_format = "%d/%m/%y"), "December 2003")
})
test_that("convert_date() converts a date string from default input to specified output", {
  expect_equal(convert_date("2011-03", output_format = "%d/%m/%Y"), "01/03/2011")
})
test_that("convert_date() converts a date string from specified input to specified output", {
  expect_equal(convert_date("19 October 1993", input_format = "%d %B %Y", output_format = "%d/%m/%y"), "19/10/93")
})
test_that("convert_date() returns a string unchanged if not a valid date", {
  expect_equal(convert_date("Not a date"), "Not a date")
})
test_that("convert_col_date() converts a whole column of dates", {
  test_data <- create_test_data("dates") %>% dplyr::pull(dates)
  expect_equal(convert_col_date(test_data), c("November 2024", "December 2024", "January 2025"))
})
test_that("convert_col_date() converts a whole column of dates within a dataframe", {
  test_data <- create_test_data("dates")
  expect_equal(test_data %>%
                 dplyr::mutate(dates = convert_col_date(dates)),
               dplyr::tibble(dates = c("November 2024", "December 2024", "January 2025")))
})
