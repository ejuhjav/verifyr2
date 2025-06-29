library(shinytest2)

test_that("{shinytest2} recording: app_startup", {
  app <- AppDriver$new(
    variant = platform_variant(),
    name = "app_startup",
    height = 777,
    width = 1598
  )
  app$expect_screenshot()
})
