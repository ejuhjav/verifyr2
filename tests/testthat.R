# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(verifyr2)

dev <- requireNamespace("pkgload", quietly = TRUE)
dev <- dev && pkgload::is_dev_package("verifyr2")

# separate running for local development and "prod" (devtools::check())
if (dev) {
  test_dir("tests/testthat", reporter = default_reporter())
} else {
  test_check("verifyr2")
}

# Source shiny app tests manually
shiny_test_path <- "shiny_examples/app/tests/testthat"
shiny_test_dir  <- system.file(shiny_test_path, package = "verifyr2")

if (dir.exists(shiny_test_dir) && interactive()) {
  test_dir(shiny_test_dir, reporter = testthat::default_reporter())
}
