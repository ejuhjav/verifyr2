
base   <- "test_outputs/xlsx"
config <- Config$new(FALSE)

################################################################################
# Excel file comparison - WITHOUT omit
################################################################################

test_that(paste(
  "Returns 'No differences' for identical files."
), {
  file1 <- testthat::test_path(base, "base.xlsx")
  file2 <- testthat::test_path(base, "copy.xlsx")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(config = config)

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'Different number of lines in compared content.'",
  "for files with additional row in one sheet"
), {
  file1 <- testthat::test_path(base, "base.xlsx")
  file2 <- testthat::test_path(base, "addition_one_row.xlsx")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(config = config)

  expect_equal(result, "Different number of lines in compared content.")
})

test_that(paste(
  "Returns 'File content has changes in 2 place(s).'",
  "for files with changed cell values in two sheets"
), {
  file1 <- testthat::test_path(base, "base.xlsx")
  file2 <- testthat::test_path(base, "modified.xlsx")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(config = config)

  expect_equal(result, "File content has changes in 2 place(s).")
})

################################################################################
# Excel file comparison - WITH omit
################################################################################

test_that(paste(
  "Returns 'No differences.'",
  "for comparison with 'omit' parameter that is not present in either file"
), {
  file1 <- testthat::test_path(base, "base.xlsx")
  file2 <- testthat::test_path(base, "copy.xlsx")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(config = config, omit = "Nothing")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'No differences.'",
  "when called with 'omit' catching the changed rows in content"
), {
  file1 <- testthat::test_path(base, "base.xlsx")
  file2 <- testthat::test_path(base, "modified.xlsx")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(config = config, omit = "002")

  expect_equal(result, "No differences.")
})

################################################################################
# Excel file comparison - with readxl package missing
################################################################################

test_that(paste(
  "Returns 'Different number of lines in compared content.",
  "Xlsx details comparison disabled.' when readxl library",
  "is not available"
), {
  file1 <- testthat::test_path(base, "base.xlsx")
  file2 <- testthat::test_path(base, "addition_one_row.xlsx")

  # mock the readxl available method to return false to replicate situation
  # that readxl library is not installed.
  local_mocked_bindings(
    check_readxl_available = function() FALSE
  )

  config_local <- Config$new(FALSE)

  omit       <- "Nothing"
  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(config = config_local, omit = omit)

  expect_equal(result, paste(
    "Different number of lines in compared content.",
    "Xlsx details comparison disabled."
  ))
})
