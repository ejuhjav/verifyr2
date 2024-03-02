#' List files that exist in two folders
#'
#' \code{list_files} List files that exist in two folders with a specific name pattern
#'
#' @param folder1 character, giving the the full file path and name of the folder where first files are stored (required)
#' @param folder2 character, giving the the full file path and name of the folder where second new files are stored (required)
#' @param pattern character, limit the files to be listed to contain a specific pattern (optional)
#'
#' @return Returns a tibble, \code{selected_files} with 3 columns \code{file}, \code{folder1_path}, \code{folder2_path}
#'
#' @examples
#'
#' folder1 <- paste0(fs::path_package("/extdata/base_files/", package = "verifyr2"))
#' folder2 <- paste0(fs::path_package("/extdata/compare_files/", package = "verifyr2"))
#' verifyr2::list_files(folder1 = folder1, folder2 = folder2, patter = "base")
#'
#' @export

list_files <- function(folder1, folder2, pattern = NULL) {

  ## do the comparison only if both of the folders exist
  if (file.exists(folder1) && file.exists(folder2)) {

    folder1_info <- folder_info(folder1, "folder1_path", pattern)
    folder2_info <- folder_info(folder2, "folder2_path", pattern)

    selected_files <- dplyr::full_join(folder1_info, folder2_info, by = "file") %>%
      dplyr::arrange(file)

    return(selected_files)
  }

  print("one or both of the folders do not exist")
}

folder_info <- function(folder, column_name, pattern) {
  folder_files  <- list.files(path = folder, pattern = pattern)
  folder_paths  <- list.files(path = folder, pattern = pattern, full.names = TRUE)
  folder_data   <- tibble::tibble(file = folder_files)
  folder_data[[column_name]] <- folder_paths

  return(folder_data)
}
