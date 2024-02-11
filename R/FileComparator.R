#' FileComparator.R
#'
#' Comparator 'base' class containing the generic comparison methods and handling for high
#' level checks (like file existence). This file contains also the \code{create_file_comparator}
#' method that is used to create the correct comparator automatically for the compared files.
#'
#' @examples
#'
#' file(x) in these examples are files with full file path in all the examples.
#'
#' instantiating a new comparator instance for every comparison:
#' \code{verifyr2::compare_files_summary(verifyr2::create_file_comparator(file1, file2), omit = omit)}
#' \code{verifyr2::compare_files_details(verifyr2::create_file_comparator(file1, file2), omit = omit)}
#'
#' instantiating a comparator instance and using that same for both comparison of same files
#' \code{comparator <- verifyr2::create_file_comparator(file1, file2)}
#' \code{verifyr2::compare_files_summary(comparator, omit = omit)}
#' \code{verifyr2::compare_files_details(comparator, omit = omit)}
#'
#' insantiating an explicit comparator manually when comparing files of single specific type
#' \code{comparator <- new("PdfFileComparator")}
#' \code{verifyr2::compare_files_summary(comparator, file1, file2, omit = omit)}
#' \code{verifyr2::compare_files_summary(comparator, file3, file4, omit = omit)}
#' \code{verifyr2::compare_files_summary(comparator, file5, file6, omit = omit)}

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
#'
#' @examples
#'
#' # file(x) in these examples are files with full file path in all the examples.
#'
#' # invoking method directly with the comparator using the files stored in the comparator itself. Note that if the comprator
#' # was manually created without files, this will return 'no files found'.
#' \code{verifyr2::compare_files_summary(comparator, omit = omit)}
#'
#' # invoking method with explicitly given files when using single comparator instance (when handling set of specific file types for example).
#' \code{verifyr2::compare_files_summary(comparator, file1 = file1, file2 = file2,  omit = omit)}
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
#'
#' @examples
#'
#' # file(x) in these examples are files with full file path in all the examples.
#'
#' # invoking method directly with the comparator using the files stored in the comparator itself. Note that if the comprator
#' # was manually created without files, this will return 'no files found'.
#' \code{verifyr2::compare_files_details(comparator, omit = omit)}
#'
#' # invoking method with explicitly given files when using single comparator instance (when handling set of specific file types for example).
#' \code{verifyr2::compare_files_details(comparator, file1 = file1, file2 = file2,  omit = omit)}
#'
#' @export

setGeneric("compare_files_details", function(comparator, file1 = NULL, file2 = NULL, omit = NULL, options = NULL, ...) standardGeneric("compare_files_details"))

#' Generic for comparing the inner part for the summary query. This method can be overwritten by more specialized comparator classes. This
#' method is intended to be called only by the comparator classes in the processing and shouldn't be called directly by the user.
#'
#' @param comparator comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file1      first file to compare
#' @param file2      second file to compare
#' @param omit       all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#'
#' @export

setGeneric("compare_files_summary_inner", function(comparator, file1, file2, omit, options = NULL, ...) standardGeneric("compare_files_summary_inner"))

#' Generic for comparing the inner part for the details query. This method can be overwritten by more specialized comparator classes. This
#' method is intended to be called only by the comparator classes in the processing and shouldn't be called directly by the user.
#'
#' @param comparator comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file1      first file to compare
#' @param file2      second file to compare
#' @param omit       all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters

setGeneric("compare_files_details_inner", function(comparator, file1, file2, omit, options = NULL, ...) standardGeneric("compare_files_details_inner"))

#' Method for comparing the file summary with the given comparator instance.
#'
#' @param comparator comparator instance used for the comparison that is meant to be created with the factory method verifyr2::create_file_comparator.
#' @param file1      first file to compare. NULL if using same values as for comparison creation
#' @param file2      second file to compare. NULL if using same values as for comparison creation
#' @param omit       all lines containing the omit string will be excluded from the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
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
#'
#' @examples
#'
#' # file(x) in these examples are files with full file path in all the examples.
#' \code{file1 <- paste0(fs::path_package("/extdata/base_files/file1.rtf", package = "verifyr")}
#' \code{file2 <- paste0(fs::path_package("/extdata/compare_files/file1.rtf", package = "verifyr")}
#' \code{file3 <- file1}
#' \code{file4 <- file2}
#'
#' instantiating a new comparator instance for every comparison:
#' \code{verifyr2::compare_files_summary(verifyr2::create_file_comparator(file1, file2), omit = omit)}
#' \code{verifyr2::compare_files_details(verifyr2::create_file_comparator(file1, file2), omit = omit)}
#'
#' # instantiating a comparator instance and using that same for both comparison of same files
#' comparator <- verifyr2::create_file_comparator(file1, file2)
#' \code{verifyr2::compare_files_summary(comparator, omit = omit)}
#' \code{verifyr2::compare_files_details(comparator, omit = omit)}
#'
#' # instantiating an explicit comparator manually when comparing files of single specific type
#' \code{comparator <- new("RtfFileComparator")}
#' \code{verifyr2::compare_files_summary(comparator, file1, file2, omit = omit)}
#' \code{verifyr2::compare_files_summary(comparator, file3, file4, omit = omit)}
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
    mime_type <- mime::guess_type(file1)

    if (startsWith(mime_type, "text/")) {
      return(new("TxtFileComparator", file1 = file1, file2 = file2))
    } else {
      return(new("BinaryFileComparator", file1 = file1, file2 = file2))
    }
  }
}
