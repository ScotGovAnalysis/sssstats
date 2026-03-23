test_that("calculates processing time", {
  test_df <- dplyr::tibble(
    app_date = c(
      "2024-07-01", "2024-07-01",
      "2024-07-06", "2024-07-07",
      "2024-05-23"
    ),
    app_decision_date = c(
      "2024-07-05", "2024-07-08",
      "2024-07-10", "2024-07-10",
      "2024-05-30"
    )
  ) |>
    dplyr::mutate(
      app_date = as.POSIXct(app_date, tz = "UTC"),
      app_decision_date = as.POSIXct(app_decision_date, tz = "UTC")
    )

  res_wd <- add_processing_time(test_df, app_date, app_decision_date, processing_time)

  expect_equal(res_wd[[1, 3]], 4,
    info = "monday to friday simple"
  )

  # includes a weekend
  expect_equal(res_wd[[2, 3]], 5,
    info = "includes a weekend"
  )

  expect_equal(res_wd[[3, 3]], 2,
    info = "arrives saturday"
  )

  expect_equal(res_wd[[4, 3]], 2,
    info = "arrives sunday"
  )

  expect_equal(res_wd[[5, 3]], 4,
    info = "includes holiday date"
  )
})

test_that("date column type validation works", {
  # incorrect date types
  df_bad <- data.frame(
    start = c("2024-01-01", "2024-01-05"),
    end   = as.Date(c("2024-01-10", "2024-01-12"))
  )

  # throws an error because 'start' is not date datatype
  expect_error(
    add_processing_time(df_bad, start, end, processing_time),
    regexp = "start.*must be of type Date or POSIX",
    fixed = FALSE
  )
})


test_that("input validation works", {
  not_df <- "not a data frame"

  # check is dataframe
  expect_error(
    add_processing_time(not_df, start, end, processing_time),
    regexp = "`df` must be a data frame"
  )

  # check required columns exist
  df_missing_cols <- data.frame(
    start = as.Date("2024-01-01")
  )

  expect_error(
    add_processing_time(df_missing_cols, start, end, processing_time),
    regexp = "This column does not exist in `df`"
  )

  # both missing
  df_none <- data.frame(x = 1:3)

  expect_error(
    add_processing_time(df_none, start, end, processing_time),
    regexp = "start, end"
  )
})

test_that("adjustment parameter works", {
  test_df <- dplyr::tibble(
    app_date = c(
      "2024-07-01", "2024-07-01",
      "2024-07-06", "2024-07-07",
      "2024-05-23"
    ),
    app_decision_date = c(
      "2024-07-05", "2024-07-08",
      "2024-07-10", "2024-07-10",
      "2024-05-30"
    )
  ) |>
    dplyr::mutate(
      app_date = as.POSIXct(app_date, tz = "UTC"),
      app_decision_date = as.POSIXct(app_decision_date, tz = "UTC")
    )

  # checks adjustment with 1 day
  res_wd_1 <- add_processing_time(
    test_df,
    app_date,
    app_decision_date,
    processing_time,
    1
  )

  expect_equal(res_wd_1[[1, 3]], 5,
    info = "monday to friday simple"
  )

  expect_equal(res_wd_1[[2, 3]], 6,
    info = "includes a weekend"
  )

  expect_equal(res_wd_1[[5, 3]], 5,
    info = "includes holiday date"
  )

  # checks adjustment with 2 days
  res_wd_2 <- add_processing_time(
    test_df,
    app_date,
    app_decision_date,
    processing_time,
    2
  )

  expect_equal(res_wd_2[[1, 3]], 6,
    info = "monday to friday simple"
  )

  expect_equal(res_wd_2[[2, 3]], 7,
    info = "includes a weekend"
  )

  expect_equal(res_wd_2[[5, 3]], 6,
    info = "includes holiday date"
  )
})
