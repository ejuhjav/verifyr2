
################################################################################
# Generic file existence checks
################################################################################

test_that(paste(
  "Returns 'File(s) not available; unable to compare.' ",
  "if both files do not exist"
), {
  file1 <- testthat::test_path("test_outputs/bin/nonexisting1.bin")
  file2 <- testthat::test_path("test_outputs/bin/nonexisting2.bin")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "File(s) not available; unable to compare.")
})

test_that(paste(
  "Returns 'File(s) not available; unable to compare.' ",
  "if one file does not exist"
), {
  file1 <- testthat::test_path("test_outputs/bin/base.bin")
  file2 <- testthat::test_path("test_outputs/bin/nonexisting.bin")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "File(s) not available; unable to compare.")
})

################################################################################
# Binary file comparison
################################################################################

test_that(paste(
  "Returns 'No differences' for identical files."
), {
  file1 <- testthat::test_path("test_outputs/bin/base.bin")
  file2 <- testthat::test_path("test_outputs/bin/copy.bin")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "No differences.")
})

test_that(paste(
  "Returns 'Different file sizes for compared files.' ",
  "for files different sizes"
), {
  file1 <- testthat::test_path("test_outputs/bin/base.bin")
  file2 <- testthat::test_path("test_outputs/bin/modified.bin")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "Different file sizes for compared files.")
})

test_that(paste(
  "Returns 'Different content in compared files' ",
  "for files with same sizes but different contents"
), {
  file1 <- testthat::test_path("test_outputs/bin/base.bin")
  file2 <- testthat::test_path("test_outputs/bin/modified_same_size.bin")

  comparator <- create_comparator(file1, file2)
  result     <- comparator$vrf_summary()

  expect_equal(result, "Different content in compared files.")
})
