#' BinaryFileComparator.R
#'
#' Fallback comparator for binary files without any specific definied
#' comparator.
#'
#' @include FileComparator.R
#'
#' @examples
#'
#' # The normal way for creating a comparator would be to call the generic
#' # factory method verifyr2::vrf_comparator, but if needed, an explicit
#' # comparator can be instantiated directly as well.
#' comparator <- new("BinaryFileComparator")
#'
#' @export
#'
BinaryFileComparator <- R6Class(
  "BinaryFileComparator",
  inherit = FileComparator,
  public = list(

    #' @description
    #' Method for getting the single file contents for the comparison. This method
    #' returns the file contents in two separate vectors inside a list. The first
    #' vector is the file contents and the second one is the file contents with the
    #' rows matching the omit string excluded. This method can be overwritten by
    #' more specialized comparator classes. This method is intended to be called
    #' only by the comparator classes in the processing and shouldn not be called
    #' directly by the user.
    #'
    #' @param file    file for which to get the contents
    #' @param omit    string pattern to omit from the comparison
    #' @param options additional comparator parameters
    #'
    vrf_contents = function(file, omit, options) {
      contents <- readLines(file, warn = FALSE)
      return(self$vrf_contents_inner(contents, omit, options))
    },

    #' @description
    #' Method for getting the inner part for the file contents query. The method
    #' returns the file contents in two separate vectors inside a list. The first
    #' vector is the file contents and the second one is the file contents with the
    #' rows matching the omit string excluded. This method can be overwritten by
    #' more specialized comparator classes. This method is intended to be called
    #' only by the comparator classes in the processing and shouldn not be called
    #' directly by the user.
    #'
    #' @param contents file contents
    #' @param omit    string pattern to omit from the comparison
    #' @param options  additional comparator parameters
    #'
    vrf_contents_inner = function(contents, omit, options) {
      return(list(contents, contents))
    },

    #' @description
    #' Method for comparing the inner part for the details query. The method
    #' returns the file contents in two separate vectors inside a list. The first
    #' vector is the file contents and the second one is the file contents with the
    #' rows matching the omit string excluded. This method can be overwritten by
    #' more specialized comparator classes. This method is intended to be called
    #' only by the comparator classes in the processing and shouldn not be called
    #' directly by the user.
    #'
    #' @param omit    string pattern to omit from the comparison
    #' @param options additional comparator parameters
    #'
    vrf_summary_inner = function(omit, options) {
      file_info1 <- file.info(self$file1)
      file_info2 <- file.info(self$file2)

      if (file_info1$size != file_info2$size) {
        return("Different file sizes for compared files.")
      }

      file1_contents_list <- self$file1_contents_list
      file2_contents_list <- self$file2_contents_list

      if (is.null(file1_contents_list)) {
        file1_contents_list <- self$vrf_contents(self$file1, omit, options)
        self$file1_contents_list <- file1_contents_list
      }

      if (is.null(file2_contents_list)) {
        file2_contents_list <- self$vrf_contents(self$file2, omit, options)
        self$file2_contents_list <- file2_contents_list
      }

      file1_contents_omit <- file1_contents_list[[2]]
      file2_contents_omit <- file2_contents_list[[2]]

      if (!identical(file1_contents_omit, file2_contents_omit)) {
        return("Different content in compared files.")
      }

      return("No differences.")
    },

    #' @description
    #' Method for comparing the inner part for the details query. This method can be
    #' overwritten by more specialized comparator classes. This method is intended
    #' to be called only by the comparator classes in the processing and shouldn't
    #' be called directly by the user.
    #'
    #' @param omit    string pattern to omit from the comparison
    #' @param options additional comparator parameters
    #'
    vrf_details_inner = function(omit, options) {
      result <- list(
        type = "text",
        contents = "Binary file without applicable comparator; unable to compare details."
      )
      return(list(result))
    }
  )
)
