#' One row list for two distinct files
#'
#' \code{list_files} List single file row based on the explicit parameter
#' files. This is a conveniency function for building same structure list for
#' direct two file comparison case.
#'
#' @param file1 character, giving the the full file path of the first file
#' @param file2 character, giving the the full file path of the second file
#'
#' @return Returns a tibble, \code{selected_files} with 2 columns \code{file1},
#'                 \code{file2}
#'
#' @examples
#'
#' path1 <- "/extdata/base_files/file2_additional_rows.rtf"
#' file1 <- paste0(fs::path_package(path1, package = "verifyr2"))
#'
#' path2 <- "/extdata/compare_files/file3_changed_rows.rtf"
#' file2 <- paste0(fs::path_package(path2, package = "verifyr2"))
#'
#' verifyr2::list_files(file1, file2)
#'
#' @export

list_files <- function(file1, file2) {

  ## do the comparison only if both of the files exist
  if (file.exists(file1) && file.exists(file2)) {

    data <- tibble::tibble(
      "file1" = file1,
      "file2" = file2
    )

    return(data)
  }

  print("one or both of the files do not exist")
}
