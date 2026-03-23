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
