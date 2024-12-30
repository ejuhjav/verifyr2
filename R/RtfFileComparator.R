#' RtfFileComparator.R
#'
#' Specialiced comparator for RTF file comparison.
#' This comparator contains the custom handling for handling only RTF content
#' part for the comparison.
#'
#' @import striprtf
#'
#' @include TxtWithImagesComparator.R
#'
#' @examples
#'
#' # The normal way for creating a comparator would be to call the generic
#' # factory method verifyr2::create_comparator that will automatically create
#' # the correct comparator instance based on the file types.
#'
#' file1 <- 'my_file1.rtf'
#' file2 <- 'my_file2.rtf'
#' comparator <- verifyr2::create_comparator(file1, file2)
#'
#' # If needed, an explicit comparator can be created as well.
#'
#' file1 <- 'my_file1.rtf'
#' file2 <- 'my_file2.rft'
#' comparator <- RtfFileComparator$new(file1, file2)
#'
#' @export
#'
RtfFileComparator <- R6Class(
  "RtfFileComparator",
  inherit = TxtWithImagesFileComparator,
  public = list(

    #' @description
    #' Method for getting the single file contents for the comparison. The method
    #' returns the file contents in two separate vectors inside a list. The first
    #' vector is the file contents and the second one is the file contents with the
    #' rows matching the omit string excluded. This method can be overwritten by
    #' more specialized comparator classes. This method is intended to be called
    #' only by the comparator classes in the processing and shouldn't be called
    #' directly by the user.
    #'
    #' For RtfComparator, the file contents are returned based on the mode
    #' parameter if available. "raw" meaning that the rtf file contents are
    #' returned as normal raw text contents, and "content" meaning that only rtf
    #' file content parts are returned.
    #'
    #' @param file    file for which to get the contents
    #' @param omit    string pattern to omit from the comparison
    #' @param options additional comparator parameters
    #'
    vrf_contents = function(file, omit, options) {
      # In raw mode we call directly the TxtFileComparator implementation of the vrf_contents
      if ("raw" == get_nested(options, "rtf", "mode")) {
        return(super$vrf_contents(file, omit, options))
      }

      # In content mode, we get the rtf text content
      contents <- striprtf::read_rtf(file = file)
      result   <- self$vrf_contents_inner(contents, omit, options)

      return(result)
    },

    #' @description
    #' "Abstract" method for getting the raw image hex vector array from the given
    #' source file.
    #'
    #' @param file    file for which to get the embedded image details
    #' @param options additional comparator parameters
    #'
    vrf_images = function(file, options) {
      # in raw mode no image details are extracted separately
      # @todo, move this check to the parent class
      if ("raw" == get_nested(options, "rtf", "mode")) {
        return(list())
      }

      result <- list()

      #  Read the RTF file embedded images
      rtf_content <- readLines(file, warn = FALSE)
      rtf_content <- paste(rtf_content, collapse = "\n")

      # regexp for finding the png pictures from the raw RTF content.
      base64_pattern <- "\\\\pict\\\\pngblip[^{]+([A-Za-z0-9+/=]+)"
      base64_strings <- stringr::str_extract_all(rtf_content, base64_pattern)[[1]]

      if (length(base64_strings) > 0) {
        for (index in seq_along(base64_strings)) {
          split_parts <- strsplit(base64_strings[index], " ")[[1]]
          base64_data_with_braces <- split_parts[2]
          base64_data <- strsplit(base64_data_with_braces, "}")[[1]][1]

          if (!is.na(base64_data) && nchar(base64_data) > 0) {
            raw_data <- self$hex2raw(base64_data)
            result <- c(result, list(raw_data))
          }
        }
      }
      return(result)
    }
  )
)
