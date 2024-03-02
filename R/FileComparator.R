#' FileComparator.R
#'
#' Comparator 'base' class containing the generic comparison methods and handling for high
#' level checks (like file existence). This file contains also the \code{create_file_comparator}
#' method that is used to create the correct comparator automatically for the compared files.
#'
#' @examples
#'
#' # instantiating the compared files
#' file1 <- paste0(fs::path_package("/extdata/base_files/file1.rtf", package = "verifyr2"))
#' file2 <- paste0(fs::path_package("/extdata/compare_files/file1.rtf", package = "verifyr2"))
#' file3 <- file1
#' file4 <- file2
#' file5 <- file1
#' file6 <- file2
#'
#' # instantiating a new comparator instance for every comparison:
#' verifyr2::compare_files_summary(verifyr2::create_file_comparator(file1, file2))
#' verifyr2::compare_files_details(verifyr2::create_file_comparator(file1, file2))
#'
#' # instantiating a comparator instance and using that same for both comparison of same files
#' comparator <- verifyr2::create_file_comparator(file1, file2)
#' verifyr2::compare_files_summary(comparator)
#' verifyr2::compare_files_details(comparator)
#'
#' # insantiating an explicit comparator manually when comparing files of single specific type
#' comparator <- new("RtfFileComparator")
#' verifyr2::compare_files_summary(comparator, file1, file2)
#' verifyr2::compare_files_summary(comparator, file3, file4)
#' verifyr2::compare_files_summary(comparator, file5, file6)

setClass("FileComparator", slots = list(file1 = "ANY", file2 = "ANY"))

setMethod("initialize", signature = "FileComparator", definition = function(.Object, file1 = NULL, file2 = NULL) {
  .Object@file1 <- file1
  .Object@file2 <- file2
  .Object
})

#' Generic for comparing the file summary with the given comparator instance.
#'
#' @param comparator comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file1      first file to compare. NULL if using same values as for comparison creation
#' @param file2      second file to compare. NULL if using same values as for comparison creation
#' @param omit       all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#' @param ...        additional parameters
#'
#' @examples
#'
#' # instantiating the compared files
#' file1 <- paste0(fs::path_package("/extdata/base_files/file1.rtf", package = "verifyr2"))
#' file2 <- paste0(fs::path_package("/extdata/compare_files/file1.rtf", package = "verifyr2"))
#'
#' # invoking method directly with the comparator using the files stored in the comparator
#' # itself. Note that if the comprator was manually created without files, this will return
#' # 'no files found'.
#' comparator <- verifyr2::create_file_comparator(file1, file2)
#' verifyr2::compare_files_summary(comparator)
#'
#' # invoking method with explicitly given files when using single comparator instance
#' # (when handling set of specific file types for example).
#' comparator <- new("RtfFileComparator")
#' verifyr2::compare_files_summary(comparator, file1 = file1, file2 = file2)
#'
#' @export

setGeneric("compare_files_summary", function(comparator, file1 = NULL, file2 = NULL, omit = NULL, options = NULL, ...) standardGeneric("compare_files_summary"))

#' Generic for comparing the file details with the given comparator instance.
#'
#' @param comparator comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file1      first file to compare. NULL if using same values as for comparison creation
#' @param file2      second file to compare. NULL if using same values as for comparison creation
#' @param omit       all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#' @param ...        additional parameters
#'
#' @examples
#'
#' # instantiating the compared files
#' file1 <- paste0(fs::path_package("/extdata/base_files/file1.rtf", package = "verifyr2"))
#' file2 <- paste0(fs::path_package("/extdata/compare_files/file1.rtf", package = "verifyr2"))
#'
#' # invoking method directly with the comparator using the files stored in the comparator
#' # itself. Note that if the comprator was manually created without files, this will return
#' # 'no files found'.
#' comparator <- verifyr2::create_file_comparator(file1, file2)
#' verifyr2::compare_files_details(comparator)
#'
#' # invoking method with explicitly given files when using single comparator instance
#' # (when handling set of specific file types for example).
#' comparator <- new("RtfFileComparator")
#' verifyr2::compare_files_details(comparator, file1 = file1, file2 = file2)
#'
#' @export

setGeneric("compare_files_details", function(comparator, file1 = NULL, file2 = NULL, omit = NULL, options = NULL, ...) standardGeneric("compare_files_details"))

#' Generic for comparing the inner part for the summary query. This method can be overwritten
#' by more specialized comparator classes. This method is intended to be called only by the
#' comparator classes in the processing and shouldn't be called directly by the user.
#'
#' @param comparator comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file1      first file to compare
#' @param file2      second file to compare
#' @param omit       all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#' @param ...        additional parameters
#'
#' @export

setGeneric("compare_files_summary_inner", function(comparator, file1, file2, omit, options = NULL, ...) standardGeneric("compare_files_summary_inner"))

#' Generic for comparing the inner part for the details query. This method can be overwritten
#' by more specialized comparator classes. This method is intended to be called only by the
#' comparator classes in the processing and shouldn't be called directly by the user.
#'
#' @param comparator comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file1      first file to compare
#' @param file2      second file to compare
#' @param omit       all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#' @param ...        additional parameters

setGeneric("compare_files_details_inner", function(comparator, file1, file2, omit, options = NULL, ...) standardGeneric("compare_files_details_inner"))

#' Method for comparing the file summary with the given comparator instance.
#'
#' @param comparator comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file1      first file to compare. NULL if using same values as for comparison creation
#' @param file2      second file to compare. NULL if using same values as for comparison creation
#' @param omit       all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#' @param ...        additional parameters
#'
#' @export

setMethod("compare_files_summary", "FileComparator", function(comparator, file1 = NULL, file2 = NULL, omit = NULL, options = NULL, ...) {
  file1 <- ifelse(!is.null(file1), file1, comparator@file1)
  file2 <- ifelse(!is.null(file2), file2, comparator@file2)

  if (!file.exists(file1) || !file.exists(file2)) {
    return("File(s) not available; unable to compare")
  }

  tryCatch({
    compare_files_summary_inner(comparator, file1, file2, omit, options, ...)
  }, error = function(e) {
    return(paste0("Error reading file contents: ", conditionMessage(e)))
  })
})

#' Method for comparing the file details with the given comparator instance.
#'
#' @param comparator comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file1      first file to compare. NULL if using same values as for comparison creation
#' @param file2      second file to compare. NULL if using same values as for comparison creation
#' @param omit       all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#' @param ...        additional parameters

setMethod("compare_files_details", "FileComparator", function(comparator, file1 = NULL, file2 = NULL, omit = NULL, options = NULL, ...) {
  file1 <- ifelse(!is.null(file1), file1, comparator@file1)
  file2 <- ifelse(!is.null(file2), file2, comparator@file2)

  if (!file.exists(file1) || !file.exists(file2)) {
    return("File(s) not available; unable to compare")
  }

  tryCatch({
    compare_files_details_inner(comparator, file1, file2, omit, options, ...)
  }, error = function(e) {
    return(paste0("Error reading file contents: ", conditionMessage(e)))
  })
})

#' Factory method for creating comparator instance based on the given two files.
#'
#' @param file1 first file to compare
#' @param file2 second file to compare
#' @param ...   additional parameters
#'
#' @examples
#'
#' # instantiating the compared files
#' file1 <- paste0(fs::path_package("/extdata/base_files/file1.rtf", package = "verifyr2"))
#' file2 <- paste0(fs::path_package("/extdata/compare_files/file1.rtf", package = "verifyr2"))
#' file3 <- file1
#' file4 <- file2
#'
#' # instantiating a new comparator instance for every comparison:
#' verifyr2::compare_files_summary(verifyr2::create_file_comparator(file1, file2))
#' verifyr2::compare_files_details(verifyr2::create_file_comparator(file1, file2))
#'
#' # instantiating a comparator instance and using that same for both comparison of same files
#' comparator <- verifyr2::create_file_comparator(file1, file2)
#' verifyr2::compare_files_summary(comparator)
#' verifyr2::compare_files_details(comparator)
#'
#' # instantiating an explicit comparator manually when comparing files of single specific type
#' comparator <- new("RtfFileComparator")
#' verifyr2::compare_files_summary(comparator, file1, file2)
#' verifyr2::compare_files_summary(comparator, file3, file4)
#'
#' @export

create_file_comparator <- function(file1, file2, ...) {
  if (!file.exists(file1) || !file.exists(file2)) {
    return(new("BinaryFileComparator", file1 = file1, file2 = file2))
  }

  file_extension  <- tools::toTitleCase(tools::file_ext(file1))
  comparator_name <- paste0(file_extension, "FileComparator")

  if (isClass(comparator_name)) {
    # dedicated comparator class used.
    return(new(comparator_name, file1 = file1, file2 = file2))
  } else {
    # generic comparator class used based on the file contents (text/binary).
    # guess_type returns incorrectly application/octet-string for lst files so handle those separately
    mime_type <- mime::guess_type(file1)

    if (startsWith(mime_type, "text/") || grepl(file_extension, c("Lst"))) {
      return(new("TxtFileComparator", file1 = file1, file2 = file2))
    } else {
      return(new("BinaryFileComparator", file1 = file1, file2 = file2))
    }
  }
}
