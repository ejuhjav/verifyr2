test_that("XlsxFileComparator detects no differences for identical files", {
  file1 <- system.file(
    "extdata/base_files/file1_base.xlsx",
    package = "verifyr2"
  )
  file2 <- system.file(
    "extdata/base_files/file1_base.xlsx",
    package = "verifyr2"
  )

  skip_if(!file.exists(file1), "Test xlsx file not available")

  config     <- verifyr2::Config$new()
  comparator <- XlsxFileComparator$new(file1 = file1, file2 = file2)
  result     <- comparator$vrf_summary(config = config)

  expect_true(grepl("No differences", result))
})

test_that("XlsxFileComparator detects differences between files", {
  file1 <- system.file(
    "extdata/base_files/file1_base.xlsx",
    package = "verifyr2"
  )
  file2 <- system.file(
    "extdata/compare_files/file2_compare.xlsx",
    package = "verifyr2"
  )

  skip_if(!file.exists(file1), "Test xlsx file not available")
  skip_if(!file.exists(file2), "Test xlsx file not available")

  config     <- verifyr2::Config$new()
  comparator <- XlsxFileComparator$new(file1 = file1, file2 = file2)
  result     <- comparator$vrf_summary(config = config)

  expect_false(grepl("No differences", result))
})

test_that("XlsxFileComparator vrf_details returns text type", {
  file1 <- system.file(
    "extdata/base_files/file1_base.xlsx",
    package = "verifyr2"
  )
  file2 <- system.file(
    "extdata/compare_files/file2_compare.xlsx",
    package = "verifyr2"
  )

  skip_if(!file.exists(file1), "Test xlsx file not available")
  skip_if(!file.exists(file2), "Test xlsx file not available")

  config     <- verifyr2::Config$new()
  comparator <- XlsxFileComparator$new(file1 = file1, file2 = file2)
  details    <- comparator$vrf_details(config = config)

  expect_true(length(details) > 0)
  expect_equal(details[[1]]$type, "text")
})
