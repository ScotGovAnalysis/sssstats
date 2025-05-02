test_that("data zone lookup works", {
  dz_2011_lookup <- get_datazone_lookup("2011")
  dz_2022_lookup <- get_datazone_lookup("2022")

  # Check that there are 6,976 rows for the data zone 2011 lookup
  # Also, check that there are 1,279 intermediate zones
  expect_equal(nrow(dz_2011_lookup), 6976)
  expect_equal(length(unique(dz_2011_lookup$iz2011_code)), 1279)

  # Check that there are 7,392 rows for the data zone 2022 lookup
  # Also, check that there are 1,334 intermediate zones
  expect_equal(nrow(dz_2022_lookup), 7392)
  expect_equal(length(unique(dz_2022_lookup$iz22_code)), 1334)
})
