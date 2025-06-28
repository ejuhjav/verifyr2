
base <- "test_outputs/json"

################################################################################
# Text file comparison - WITHOUT omit
################################################################################

test_that(paste(
  "Returns 'No differences' for identical files."
), {
  file1 <- testthat::test_path(base, "base.json")
  file2 <- testthat::test_path(base, "copy.json")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'Different number of lines in compared content.'",
  "for files with additional line in the content"
), {
  file1 <- testthat::test_path(base, "base.json")
  file2 <- testthat::test_path(base, "modified.json")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "Different number of lines in compared content.")
})

################################################################################
# Text file comparison - WITH omit
################################################################################

test_that(paste(
  "Returns 'No differences.'",
  "for comparison with 'omit' parameter that is not present in either file"
), {
  file1 <- testthat::test_path(base, "base.json")
  file2 <- testthat::test_path(base, "copy.json")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(omit = "Nothing")

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'Different number of lines in compared content.'",
  "when called with 'omit' catching the changed rows in content"
), {
  file1 <- testthat::test_path(base, "base.json")
  file2 <- testthat::test_path(base, "modified.json")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(omit = "height")

  expect_equal(result, "Different number of lines in compared content.")
})

test_that(paste(
  "Returns 'File content has changes in 5 place(s).'",
  "when called with 'omit' catching the additional rows in content"
), {
  file1 <- testthat::test_path(base, "base.json")
  file2 <- testthat::test_path(base, "modified.json")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary(omit = "sex")

  expect_equal(result, "File content has changes in 5 place(s).")
})
