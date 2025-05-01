#' List files that exist in two folders
#'
#' \code{list_folder_files} List files that exist in two folders with a specific
#' name pattern
#'
#' @param folder1 character, giving the the full file path and name of the
#'                folder where first files are stored (required)
#' @param folder2 character, giving the the full file path and name of the
#'                folder where second new files are stored (required)
#' @param pattern character, limit the files to be listed to contain a
#'                specific pattern (optional)
#'
#' @return Returns a tibble, \code{selected_files} with 2 columns \code{file1},
#'                 \code{file2}
#'
#' @examples
#'
#' folder1 <- paste0(fs::path_package("/extdata/base_files/",
#'                                    package = "verifyr2"))
#'
#' folder2 <- paste0(fs::path_package("/extdata/compare_files/",
#'                                    package = "verifyr2"))
#'
#' verifyr2::list_folder_files(folder1, folder2, "base")
#'
#' @export

list_folder_files <- function(folder1, folder2, pattern = NULL) {

  ## do the comparison only if both of the folders exist
  if (file.exists(folder1) && file.exists(folder2)) {

    folder1_info <- folder_info(folder1, "file1", pattern)
    folder2_info <- folder_info(folder2, "file2", pattern)

    selected_files <- dplyr::full_join(folder1_info,
                                       folder2_info,
                                       by = "file") %>%
      dplyr::arrange(file) %>%
      dplyr::select("file1", "file2")

    return(selected_files)
  }

  print("one or both of the folders do not exist")
  return(NULL)
}

folder_info <- function(folder, column_name, pattern) {
  files  <- list.files(path = folder, pattern = pattern)
  paths  <- list.files(path = folder, pattern = pattern, full.names = TRUE)
  data   <- tibble::tibble(file = files)
  data[[column_name]] <- paths

  return(data)
}
