
base   <- "test_outputs/pdf"
config <- Config$new(FALSE)

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "Returns 'File(s) not available; unable to compare.' ",
  "if both of the files do not exist"
), {
  file1 <- testthat::test_path(base, "nonexisting1.pdf")
  file2 <- testthat::test_path(base, "nonexisting2.pdf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)
  expect_length(result, 1)

  txt_result = result[[1]]
  expect_equal(txt_result$type, "text")
  expect_equal(txt_result$contents, "File(s) not available; unable to compare.")
})

test_that(paste(
  "Returns 'File(s) not available; unable to compare.' ",
  "if one file does not exist"
), {
  file1 <- testthat::test_path(base, "base.pdf")
  file2 <- testthat::test_path(base, "nonexisting.pdf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)
  expect_length(result, 1)

  txt_result = result[[1]]
  expect_equal(txt_result$type, "text")
  expect_equal(txt_result$contents, "File(s) not available; unable to compare.")
})

################################################################################
# Simple tests that a S4 object is received for details comparison
################################################################################

test_that(paste(
  "Returns S4 comparison object for two files with same content."
), {
  file1 <- testthat::test_path(base, "base.pdf")
  file2 <- testthat::test_path(base, "copy.pdf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)
  expect_length(result, 1)

  txt_result = result[[1]]
  expect_equal(txt_result$type, "text")
  expect_equal(typeof(txt_result$contents), "S4")
})

test_that(paste(
  "Returns S4 comparison object for two files with differences in content"
), {
  file1 <- testthat::test_path(base, "two_pages.pdf")
  file2 <- testthat::test_path(base, "two_pages_changes_one_row.pdf")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_details(config = config)
  expect_length(result, 1)

  txt_result = result[[1]]
  expect_equal(txt_result$type, "text")
  expect_equal(typeof(txt_result$contents), "S4")
})

################################################################################
# Text file comparison - with PDF package missing
################################################################################

test_that(paste(
  "Returns 'Pdf details comparison disabled.' when pdftools library ",
  "is not available"
), {
  file1 <- testthat::test_path(base, "two_pages.pdf")
  file2 <- testthat::test_path(base, "two_pages_addition_two_rows.pdf")

  # mock the pdftools available method to return false to replicate situation
  # that pdftools library is not installed.
  local_mocked_bindings(
    check_pdftools_available = function() FALSE
  )

  config_local <- Config$new(FALSE)
  comparator   <- create_comparator(file1, file2)
  result       <- comparator$vrf_details(config = config_local)
  expect_length(result, 1)

  txt_result = result[[1]]
  expect_equal(txt_result$type, "text")
  expect_equal(txt_result$contents, "Pdf details comparison disabled.")
})
