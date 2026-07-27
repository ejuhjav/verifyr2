
base   <- "test_outputs/xlsx"
config <- Config$new(FALSE)

################################################################################
# Sheet / row / column annotation and header-row detection
################################################################################

test_that(paste(
  "Flattened contents annotate each cell with its column name",
  "so differences indicate sheet, row and column"
), {
  file1 <- testthat::test_path(base, "base.xlsx")

  comparator <- create_comparator(file1, file1)
  contents   <- comparator$vrf_contents(file1, config, omit = NULL)[[1]]

  # a data row carries the sheet marker, a row number and Column=Value cells
  data_rows <- grep("\\] row ", contents, value = TRUE)
  expect_true(length(data_rows) > 0)
  expect_true(any(grepl("=", data_rows)))
})

test_that(paste(
  "The default header handling treats the first row as a header"
), {
  file1 <- testthat::test_path(base, "base.xlsx")

  comparator <- create_comparator(file1, file1)
  contents   <- comparator$vrf_contents(file1, config, omit = NULL)[[1]]

  expect_false(any(grepl("no header row", contents)))
})

test_that(paste(
  "xlsx.header = 'no' treats every row as data (positional columns)"
), {
  file1 <- testthat::test_path(base, "base.xlsx")

  config_local <- Config$new(FALSE)
  config_local$set("xlsx.header", "no")

  comparator <- create_comparator(file1, file1)
  content    <- comparator$vrf_contents(file1, config_local, NULL)[[1]]

  expect_true(any(grepl("no header row", content)))
  expect_true(any(grepl("Column1=", content)))
})

test_that(paste(
  "xlsx.header = 'yes' treats first row as header"
), {
  file1 <- testthat::test_path(base, "base.xlsx")

  config_local <- Config$new(FALSE)
  config_local$set("xlsx.header", "yes")

  comparator <- create_comparator(file1, file1)
  content    <- comparator$vrf_contents(file1, config_local, NULL)[[1]]

  expect_false(any(grepl("no header row", content)))
})
