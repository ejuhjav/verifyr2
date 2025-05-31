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
    #' Method for getting the single file contents for the comparison. The
    #' method returns the file contents in two separate vectors inside a list.
    #' The first vector is the file contents and the second one is the file
    #' contents with the rows matching the omit string excluded. This method
    #' can be overwritten by more specialized comparator classes. This method
    #' is intended to be called only by the comparator classes in the processing
    #' and shouldn't be called directly by the user.
    #'
    #' For RtfComparator, only the RTF file content part is returned for
    #' comparison.
    #'
    #' @param file    file for which to get the contents
    #' @param omit    string pattern to omit from the comparison
    #' @param options additional comparator parameters
    #'
    vrf_contents = function(file, omit, options) {
      self$vrf_open_debug("Rtf::vrf_contents", options)

      # Get the RTF text content
      contents <- striprtf::read_rtf(file = file)
      result   <- self$vrf_contents_inner(contents, omit, options)

      self$vrf_close_debug()
      return(result)
    },

    #' @description
    #' "Abstract" method for getting the raw image hex vector array from the
    #' given source file.
    #'
    #' @param file    file for which to get the embedded image details
    #' @param options additional comparator parameters
    #'
    vrf_images = function(file, options) {
      self$vrf_open_debug("Rtf::vrf_images", options)

      # return empty list if embedded image processing is disabled
      if ("no" == super$vrf_option_value(options, "rtf.images")) {
        self$vrf_close_debug()
        return(list())
      }

      result <- list()

      #  Read the RTF file embedded images
      rtf_content <- readLines(file, warn = FALSE)
      rtf_content <- paste(rtf_content, collapse = "\n")

      # regexp for finding the png pictures from the raw RTF content.
      pattern <- "\\\\pict\\\\pngblip[^{]+([A-Za-z0-9+/=]+)"
      matches <- stringr::str_extract_all(rtf_content, pattern)[[1]]

      if (length(matches) > 0) {
        for (index in seq_along(matches)) {
          split_parts <- strsplit(matches[index], " ")[[1]]
          base64_data_with_braces <- split_parts[2]
          base64_data <- strsplit(base64_data_with_braces, "}")[[1]][1]

          if (!is.na(base64_data) && nchar(base64_data) > 0) {
            raw_data <- self$hex2raw(base64_data)
            result <- c(result, list(raw_data))
          }
        }
      }

      self$vrf_close_debug()
      return(result)
    }
  )
)
