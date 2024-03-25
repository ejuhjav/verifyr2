#' Get value from nested array.
#'
#' \code{get_nested} Get value from the given nested array. If the given array
#' path doesn't exist, returns character 'NA'.
#'
#' @param data main level array
#' @param ...  the keys for the array path
#'
#' @return value found from the given array path or 'NA'
#'
#' @examples
#'
#' data <- list("value1" = list("value2" = list("value3" = "value4")))
#'
#' # result = "value4" (found)
#' result <- get_nested(data, "value1", "value2", "value3")
#'
#' # result = 'NA' (not found)
#' result <- get_nested(data, "value1", "value2", "value4")
#' result <- get_nested(data, "value2", "value3")
#'
#' @export

get_nested <- function(data, ...) {

  elements <- list(...)
  current  <- data

  if (0 == length(elements)) {
    return("NA")
  }

  for (element in elements) {
    if (is.null(current) || !element %in% names(current)) {
      return("NA")
    }
    current <- current[[element]]
  }

  return(current)
}
