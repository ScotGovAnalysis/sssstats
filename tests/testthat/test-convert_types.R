test_that("make_supp_negative() converts a [c] to -1", {
  expect_equal(make_supp_negative("[c]"), "-1")
})
test_that("make_supp_negative() ignores a numeric", {
  expect_equal(make_supp_negative(45), 45)
})
test_that("make_supp_negative() ignores NA", {
  expect_equal(make_supp_negative(NA), NA)
})
test_that("make_all_supp_negative() converts all [c] to -1", {
  test_data <- create_test_data("suppressed")
  expect_equal(make_all_supp_negative(test_data) %>%
                 dplyr::pull(suppressed),
               c("-1", "3", "-1"))
})
test_that("make_all_supp_negative() converts all [c] to -1 over whole table", {
  test_data <- create_test_data()
  expect_equal(make_all_supp_negative(test_data),
              test_data %>%
                 dplyr::mutate(suppressed = c("-1", "3", "-1")))
})
test_that("make_string_numeric() converts a string to numeric", {
  expect_equal(make_string_numeric("120"), 120)
})
test_that("make_string_numeric() ignores a numeric", {
  expect_equal(make_string_numeric(45), 45)
})
test_that("make_string_numeric() ignores NA", {
  expect_equal(make_string_numeric(NA), NA)
})
test_that("make_all_string_numeric() converts all strings to numeric", {
  test_data <- create_test_data("number_string")
  expect_equal(make_all_string_numeric(test_data) %>%
                 dplyr::pull(number_string),
               c(4, 5, 6))
})
test_that("make_all_string_numeric() converts all strings to numeric over whole table", {
  test_data <- create_test_data()
  expect_equal(test_data %>%
                 make_all_supp_negative() %>%
                 make_all_string_numeric(),
               test_data %>%
                 dplyr::mutate(number_string = c(4, 5, 6),
                               suppressed = c(-1, 3, -1)))
})
test_that("make_all_string_numeric() returns error if mixed columns present", {
  test_data <- create_test_data()
  expect_equal(suppressMessages(test_data %>%
                 make_all_string_numeric()),
               NULL)
})
test_that("make_number_percent() converts an integer to percentage", {
  expect_equal(make_number_percent(5), 0.05)
})
test_that("make_number_percent() ignores a string", {
  expect_equal(make_number_percent("This is a string"), "This is a string")
})
test_that("make_number_percent() ignores NA", {
  expect_equal(make_number_percent(NA), NA)
})
test_that("make_all_number_percent() converts all integers to percentage", {
  test_data <- create_test_data("percentage")
  expect_equal(make_all_number_percent(test_data) %>%
                 dplyr::pull(percentage),
               c(0.06, 0.09, 0.04))
})
test_that("make_all_number_percent() converts integers to percentage over whole table, for correctly titled columns", {
  test_data <- create_test_data()
  expect_equal(make_all_number_percent(test_data),
               test_data %>%
                 dplyr::mutate(percentage = c(0.06, 0.09, 0.04)))
})

