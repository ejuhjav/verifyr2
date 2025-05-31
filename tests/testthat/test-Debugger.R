
################################################################################
# Test debugger simple open/close methods
################################################################################

test_that("Debugger printout", {
  debugger <- Debugger$new()

  output <- capture.output({
    debugger$open_debug("opened1")
    debugger$close_debug()
  })

  expect_true(any(grepl("[DEBUG] 'opened1' (execution time", output, fixed = TRUE)))
})

test_that("Debugger printout with add method", {
  debugger <- Debugger$new()

  output <- capture.output({
    debugger$open_debug("opened2")
    debugger$add_debug("additional2")
    debugger$close_debug()
  })

  expect_true(any(grepl("[DEBUG] 'opened2' (execution time", output, fixed = TRUE)))
  expect_true(any(grepl("[DEBUG]   'additional2'", output, fixed = TRUE)))
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

  expect_true(any(grepl("[DEBUG] 'opened3' (execution time", output, fixed = TRUE)))
  expect_true(any(grepl("[DEBUG]   'internal-opened4' (execution time", output, fixed = TRUE)))
  expect_true(any(grepl("[DEBUG]     'internal-opened5' (execution time", output, fixed = TRUE)))
  expect_true(any(grepl("[DEBUG]     'additional3'", output, fixed = TRUE)))
})
