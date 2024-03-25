
################################################################################
# Successfull cases
################################################################################

test_that("Returns found array value from nesting level 1", {
  data   <- list("value1" = "value2")
  result <- get_nested(data, "value1")

  expect_equal(result, "value2")
})

test_that("Returns found array value from nesting level 2", {
  data   <- list("value1" = list("value2" = "value3"))
  result <- get_nested(data, "value1", "value2")

  expect_equal(result, "value3")
})

test_that("Returns found array value from nesting level 3", {
  data   <- list("value1" = list("value2" = list("value3" = "value4")))
  result <- get_nested(data, "value1", "value2", "value3")

  expect_equal(result, "value4")
})

################################################################################
# array path not found cases
################################################################################

test_that("Returns 'NA' when array path not found on level 1", {
  data   <- list("value1" = "value2")
  result <- get_nested(data, "valueX")

  expect_equal(result, "NA")
})

test_that("Returns 'NA' when array path not found on level 2", {
  data   <- list("value1" = list("value2" = "value3"))
  result <- get_nested(data, "value1", "valueY")

  expect_equal(result, "NA")
})

test_that("Returns 'NA' when array path not found on level 3", {
  data   <- list("value1" = list("value2" = list("value3" = "value4")))
  result <- get_nested(data, "value1", "value2", "valueY")

  expect_equal(result, "NA")
})

################################################################################
# array is null
################################################################################

test_that("Returns 'NA' when array is null", {
  data   <- NULL
  result <- get_nested(data, "valueX")

  expect_equal(result, "NA")
})
