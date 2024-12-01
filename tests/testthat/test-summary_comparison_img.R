
base <- "test_outputs/img"

################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "Returns 'File(s) not available; unable to compare'",
  "if both files do not exist"
), {
  file1 <- testthat::test_path(base, "nonexisting1.jpg")
  file2 <- testthat::test_path(base, "nonexisting2.jpg")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "File(s) not available; unable to compare")
})

test_that(paste(
  "Returns 'File(s) not available; unable to compare'",
  "if one file does not exist"
), {
  file1 <- testthat::test_path(base, "base.jpeg")
  file2 <- testthat::test_path(base, "nonexisting.jpeg")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "File(s) not available; unable to compare")
})

################################################################################
# comparison
################################################################################

test_that(paste(
  "Returns 'No differences' for identical files"
), {
  file1 <- testthat::test_path(base, "base.png")
  file2 <- testthat::test_path(base, "copy.png")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "No differences")
})

test_that(paste(
  "Returns 'Different file sizes for compared files'"
), {
  file1 <- testthat::test_path(base, "base.jpg")
  file2 <- testthat::test_path(base, "modified1.jpg")

  comparator <- vrf_comparator(file1, file2)
  result     <- vrf_summary(comparator)

  expect_equal(result, "Different file sizes for compared files")
})
