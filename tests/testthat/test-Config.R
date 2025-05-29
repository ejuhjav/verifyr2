
base <- "test_configs"

################################################################################
# Test configuration json loading
################################################################################

test_that("Configuration json file loaded successfully", {
  path   <- testthat::test_path(base, "config1.json")
  config <- Config$new(config_path = path)

  expect_equal(config$get("rtf.mode"), "raw")
  expect_equal(config$get("details.mode"), "full")
})

test_that("Partial configuration successfully updated with defaults", {
  path   <- testthat::test_path(base, "config2.json")
  config <- Config$new(config_path = path)

  expect_equal(config$get("rtf.mode"), "raw")
  expect_equal(config$get("details.mode"), "summary")
})

################################################################################
# Test configuration saving
################################################################################

test_that("Configuration json file loaded successfully", {
  path   <- testthat::test_path("test_tmp", paste0(as.integer(Sys.time()), ".json"))
  config <- Config$new(load_config = FALSE)

  config$path <- path
  config$save()

  expect_equal(file.exists(path), TRUE)
  unlink(path)
})
