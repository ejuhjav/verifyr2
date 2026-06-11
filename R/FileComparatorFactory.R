#' FileComparatorFactory.R
#'
#' Factory method for creating comparator instance based on the given two files.
#'
#' @param file1 first file to compare
#' @param file2 second file to compare
#'
#' @examples
#'
#' # instantiating the compared files
#' file1 <- "file1.rtf"
#' file2 <- "file2.rtf"
#'
#' # instantiating the configuration
#' config <- Config$new()
#'
#' # instantiating a new comparator instance for every comparison:
#' comparator <- verifyr2::create_comparator(file1, file2)
#'
#' # calling the summary and details comparison methods
#' comparator$vrf_summary(config = config)
#' comparator$vrf_details(config = config)
#'
#' @include BinaryFileComparator.R
#' @include TxtFileComparator.R
#'
#' @export
#'
create_comparator <- function(file1, file2) {
  if (!file.exists(file1) || !file.exists(file2)) {
    return(BinaryFileComparator$new(file1 = file1, file2 = file2))
  }

  file_extension  <- tools::toTitleCase(tools::file_ext(file1))

  if (file_extension %in% list("Jpg", "Jpeg", "Png")) {
    file_extension <- "Img"
  }

  # construct the comparator name
  comparator_name <- paste0(file_extension, "FileComparator")

  if (exists(comparator_name, envir = .GlobalEnv)) {
    class_def <- get(comparator_name, envir = .GlobalEnv)

    # return the specific class instance if the class exists
    if (inherits(class_def, "R6ClassGenerator")) {
      return(do.call(class_def$new, list(file1 = file1, file2 = file2)))
    }
  }

  # generic comparator class used based on the file contents (text/binary).
  mime_type <- mime::guess_type(file1)

  text_like <- c(
    "application/json",
    "application/xml",
    "application/javascript",
    "application/x-yaml",
    "application/sql",
    "application/x-httpd-php",
    "application/x-sh",
    "application/csv",
    "application/x-tex",
    "application/x-markdown"
  )

  # additional types that the mime::guess_type mapping doesn't handle correctly
  additional_text_types <- c(
    "Lst",
    "Sas"
  )

  txt_type <- startsWith(mime_type, "text/")
  fix_type <- mime_type %in% text_like
  add_type <- file_extension %in% additional_text_types

  if (txt_type || fix_type || add_type) {
    TxtFileComparator$new(file1 = file1, file2 = file2)
  } else {
    BinaryFileComparator$new(file1 = file1, file2 = file2)
  }
}

is_r6_class <- function(class_name) {
  obj <- try(get(class_name, envir = .GlobalEnv), silent = TRUE)
  inherits(obj, "R6ClassGenerator")
}
