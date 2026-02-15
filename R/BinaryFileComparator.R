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
BinaryFileComparator <- R6::R6Class(
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
    #' @param file   file for which to get the contents
    #' @param config configuration values
    #' @param omit   string pattern to omit from the comparison
    #'
    vrf_contents = function(file, config, omit) {
      self$vrf_open_debug("Binary::vrf_contents", config)

      contents <- readLines(file, warn = FALSE)
      result   <- self$vrf_contents_inner(contents, config, omit)

      self$vrf_close_debug()
      result
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
    #' @param config   configuration values
    #' @param omit     string pattern to omit from the comparison
    #'
    vrf_contents_inner = function(contents, config, omit) {
      self$vrf_add_debug("Binary::vrf_contents_inner")
      list(contents, contents)
    },

    #' @description
    #' Method for comparing the inner part for the details query. The method
    #' returns the file contents in two separate vectors inside a list. The
    #' first vector is the file contents and the second one is the file contents
    #' processed for empty spaces and omit terms if applicable. This method can
    #' be overwritten by more specialized comparator classes. This method is
    #' intended to be called only by the comparator classes in the processing
    #' and shouldn not be called directly by the user.
    #'
    #' @param config configuration values
    #' @param omit   string pattern to omit from the comparison
    #'
    vrf_summary_inner = function(config, omit) {
      self$vrf_open_debug("Binary::vrf_summary_inner", config)

      file_info1 <- file.info(self$file1)
      file_info2 <- file.info(self$file2)

      if (file_info1$size != file_info2$size) {
        self$vrf_close_debug()
        return("Different file sizes for compared files.")
      }

      file1_contents_list <- self$file1_contents_list
      file2_contents_list <- self$file2_contents_list

      if (is.null(file1_contents_list)) {
        file1_contents_list <- self$vrf_contents(self$file1, config, omit)
        self$file1_contents_list <- file1_contents_list
      }

      if (is.null(file2_contents_list)) {
        file2_contents_list <- self$vrf_contents(self$file2, config, omit)
        self$file2_contents_list <- file2_contents_list
      }

      file1_contents_processed <- file1_contents_list[[2]]
      file2_contents_processed <- file2_contents_list[[2]]

      if (!identical(file1_contents_processed, file2_contents_processed)) {
        self$vrf_close_debug()
        return("Different content in compared files.")
      }

      self$vrf_close_debug()
      "No differences."
    },

    #' @description
    #' Method for comparing the inner part for the details query. This method
    #' can be overwritten by more specialized comparator classes. This method is
    #' intended to be called only by the comparator classes in the processing
    #' and shouldn't be called directly by the user.
    #'
    #' @param config configuration values
    #' @param omit   string pattern to omit from the comparison
    #'
    vrf_details_inner = function(config, omit) {
      self$vrf_add_debug("Binary::vrf_details_inner")
      result <- list(
        type = "text",
        contents = "Binary file without applicable comparator."
      )
      list(result)
    }
  )
)
