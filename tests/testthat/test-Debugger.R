
################################################################################
# Test debugger simple open/close methods
################################################################################

test_that("Debugger printout", {
  debugger <- Debugger$new()

  output <- capture.output({
    debugger$open_debug("opened1")
    debugger$close_debug()
  })

  expected <- "[DEBUG] 'opened1' (execution time"
  expect_content(expected, output)
})

test_that("Debugger printout with add method", {
  debugger <- Debugger$new()

  output <- capture.output({
    debugger$open_debug("opened2")
    debugger$add_debug("additional2")
    debugger$close_debug()
  })

  expected <- "[DEBUG] 'opened2' (execution time"
  expect_content(expected, output)

  expected <- "[DEBUG]   'additional2'"
  expect_content(expected, output)
})

test_that("Debugger printout with multiple levels", {
  debugger <- Debugger$new()

  output <- capture.output({
    debugger$open_debug("opened3")
    debugger$open_debug("internal-opened4")
    debugger$open_debug("internal-opened5")
    debugger$close_debug()
    debugger$add_debug("additional3")
    debugger$close_debug()
    debugger$close_debug()
  })

  expected <- "[DEBUG] 'opened3' (execution time"
  expect_content(expected, output)

  expected <- "[DEBUG]   'internal-opened4' (execution time"
  expect_content(expected, output)

  expected <- "[DEBUG]     'internal-opened5' (execution time"
  expect_content(expected, output)

  expected <- "[DEBUG]     'additional3'"
  expect_content(expected, output)
})
