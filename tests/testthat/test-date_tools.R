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
test_that("financial_year() coverts a data objection into financial year", {
  test_jan_mar_data <- tibble::tibble(dates = seq(as.Date("2024-01-01"),
                                                  as.Date("2024-03-31"),
                                                  by = "day"),
                                      calculated_fy = financial_year(dates),
                                      expected_fy = "2023-2024")

  test_apr_dec_data <- tibble::tibble(dates = seq(as.Date("2024-04-01"),
                                                  as.Date("2024-12-31"),
                                                  by = "day"),
                                      calculated_fy = financial_year(dates),
                                      expected_fy = "2024-2025")

  expect_equal(test_jan_mar_data$calculated_fy, test_jan_mar_data$expected_fy)
  expect_equal(test_apr_dec_data$calculated_fy, test_apr_dec_data$expected_fy)
})

# tests for the sss calendar creation
test_that("Warning is emitted when date_to exceeds holiday range", {
  skip_if_not_installed("bizdays")
  expect_warning(
    create_sss_calendar(date_from = "2018-01-01", date_to = "2070-01-01")
  )
})

test_that("create_sss_calendar creates a valid bizdays calendar", {
  skip_if_not_installed("bizdays")

  cal <- create_sss_calendar(
    date_from = "2018-01-01",
    date_to = "2026-12-31"
  )

  expect_s3_class(cal, "Calendar")
  expect_equal(cal$name, "sss_calendar")
  expect_setequal(tolower(cal$weekdays), c("saturday", "sunday"))
  expect_true(inherits(cal$holidays, "Date"))
})

test_that("Known holiday is not a business day; normal weekday is", {
  skip_if_not_installed("bizdays")

  cal <- create_sss_calendar(
    date_from = "2018-01-01",
    date_to = "2026-12-31"
  )

  # holiday
  expect_false(bizdays::is.bizday(as.Date("2024-12-25"), cal))

  # normal weekday
  expect_true(bizdays::is.bizday(as.Date("2024-12-24"), cal))

  # Saturday
  expect_false(bizdays::is.bizday(as.Date("2023-06-03"), cal))

  # Sunday
  expect_false(bizdays::is.bizday(as.Date("2023-06-04"), cal))


})
