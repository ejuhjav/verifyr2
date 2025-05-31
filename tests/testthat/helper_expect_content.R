
expect_content <- function(expected, content) {
  expect_true(any(grepl(expected, content, fixed = TRUE)))
}
