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
#' # factory method verifyr2::create_comparator that will automatically create
#' # the correct comparator instance based on the file types.
#'
#' file1 <- 'my_file1.bin'
#' file2 <- 'my_file2.bin'
#' comparator <- verifyr2::create_comparator(file1, file2)
#'
#' # If needed, an explicit comparator can be created as well.
#'
#' file1 <- 'my_file1.bin'
#' file2 <- 'my_file2.bin'
#' comparator <- BinaryFileComparator$new(file1, file2)
#'
#' @export
#'
BinaryFileComparator <- R6Class(
  "BinaryFileComparator",
  inherit = FileComparator,
  public = list(

    #' @description
    #' Method for getting the single file contents for the comparison. This
    #' method returns the file contents in two separate vectors inside a list.
    #' The first vector is the file contents and the second one is the file
    #' contents with the rows matching the omit string excluded. This method
    #' can be overwritten by more specialized comparator classes. This method
    #' is intended to be called only by the comparator classes in the processing
    #' and shouldn not be called directly by the user.
    #'
    #' @param file    file for which to get the contents
    #' @param omit    string pattern to omit from the comparison
    #' @param options additional comparator parameters
    #'
    vrf_contents = function(file, omit, options) {
      self$vrf_open_debug("Binary::vrf_contents", options)

      contents <- readLines(file, warn = FALSE)
      result   <- self$vrf_contents_inner(contents, omit, options)

      self$vrf_close_debug()
      return(result)
    },

    #' @description
    #' Method for getting the inner part for the file contents query. The method
    #' returns the file contents in two separate vectors inside a list. The
    #' first vector is the file contents and the second one is the file contents
    #' with the rows matching the omit string excluded. This method can be
    #' overwritten by more specialized comparator classes. This method is
    #' intended to be called only by the comparator classes in the processing
    #' and shouldn not be called directly by the user.
    #'
    #' @param contents file contents
    #' @param omit     string pattern to omit from the comparison
    #' @param options  additional comparator parameters
    #'
    vrf_contents_inner = function(contents, omit, options) {
      self$vrf_add_debug("Binary::vrf_contents_inner")
      return(list(contents, contents))
    },

    #' @description
    #' Method for comparing the inner part for the details query. The method
    #' returns the file contents in two separate vectors inside a list. The
    #' first vector is the file contents and the second one is the file contents
    #' with the rows matching the omit string excluded. This method can be
    #' overwritten by more specialized comparator classes. This method is
    #' intended to be called only by the comparator classes in the processing
    #' and shouldn not be called directly by the user.
    #'
    #' @param omit    string pattern to omit from the comparison
    #' @param options additional comparator parameters
    #'
    vrf_summary_inner = function(omit, options) {
      self$vrf_open_debug("Binary::vrf_summary_inner", options)

      file_info1 <- file.info(self$file1)
      file_info2 <- file.info(self$file2)

      if (file_info1$size != file_info2$size) {
        self$vrf_close_debug()
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
        self$vrf_close_debug()
        return("Different content in compared files.")
      }

      self$vrf_close_debug()
      return("No differences.")
    },

    #' @description
    #' Method for comparing the inner part for the details query. This method
    #' can be overwritten by more specialized comparator classes. This method is
    #' intended to be called only by the comparator classes in the processing
    #' and shouldn't be called directly by the user.
    #'
    #' @param omit    string pattern to omit from the comparison
    #' @param options additional comparator parameters
    #'
    vrf_details_inner = function(omit, options) {
      self$vrf_add_debug("Binary::vrf_details_inner")
      result <- list(
        type = "text",
        contents = "Binary file without applicable comparator."
      )
      return(list(result))
    }
  )
)
