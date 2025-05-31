#' Call for shiny example where the user can test verifyr2 package functions
#'
#' \code{verifyr2::run_example} returns simple Shiny App where user can see how
#' the verifyr2 functions work
#'
#' @export
#'
#' @param debug option to override debug configuration (TRUE only) 
#'
run_example <- function(debug = FALSE) {
  appDir <- system.file("shiny_examples", "app", package = "verifyr2")

  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `verifyr2`.",
         call. = FALSE)
  }

  options(verifyr2.debug = debug)
  shiny::runApp(appDir, display.mode = "normal")
}
