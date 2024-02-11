#' Call for shiny example where the user can test verifyr2 package functions
#'
#' \code{verifyr2::run_example} returns simple Shiny App where user can see how the verifyr2 functions work
#'
#' @examples
#'
#' \code{verifyr2::run_example()}
#'
#' @export

run_example <- function() {
  appDir <- system.file("shiny_examples", "app", package = "verifyr2")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `verifyr2`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
