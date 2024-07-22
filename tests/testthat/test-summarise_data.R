test_that("bucket_other() aggregates any values not allowed in argument", {
  test_data <- create_test_data_2(c("place", "count"))
  expect_equal(bucket_other(data = test_data,
                            col = "place",
                            allowed_values = c("Glasgow", "Edinburgh")),
               dplyr::tibble(place = c("Other", "Glasgow", "Edinburgh", "Edinburgh", "Other", "Other", "Other", "Other", "Glasgow", "Edinburgh"),
                             count = c(4, 5, 7, 3, 6, 2, 7, 3, 4, 9)))
})
