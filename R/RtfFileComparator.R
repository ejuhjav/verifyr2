#' RtfFileComparator.R
#'
#' Specialiced comparator for RTF file comparison.
#' This comparator contains the custom handling for handling only RTF content
#' part for the comparison.
#'
#' @include TxtFileComparator.R
#'
#' @importFrom methods callNextMethod
#' @examples
#'
#' # The normal way for creating a comparator would be to call the generic
#' # factory method verifyr2::vrf_comparator, but if needed, an explicit
#' # comparator can be instantiated directly as well.
#' comparator <- new("RtfFileComparator")
#'
#' @export
#'
RtfFileComparator <- R6Class(
  "RtfFileComparator",
  inherit = TxtFileComparator,
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

      # In content mode, we get the rtf text contents and the possible embedded images
      # 1. Read the RTF file contents
      contents <- striprtf::read_rtf(file = file)
      result   <- self$vrf_contents_inner(contents, omit, options)

      # 2. Read the RTF file embedded images
      rtf_content <- readLines(file, warn = FALSE)
      rtf_content <- paste(rtf_content, collapse = "\n")

      # regexp for finding the png pictures from the raw RTF content.
      base64_pattern <- "\\\\pict\\\\pngblip[^{]+([A-Za-z0-9+/=]+)"
      base64_strings <- stringr::str_extract_all(rtf_content, base64_pattern)[[1]]

      if (length(base64_strings) > 0) {
        split_parts <- strsplit(base64_strings[1], " ")[[1]]
        base64_data_with_braces <- split_parts[2]
        base64_data <- strsplit(base64_data_with_braces, "}")[[1]][1]

        if (!is.na(base64_data) && nchar(base64_data) > 0) {
          if (2 == length(result)) {
            result[[3]] <- list()
          }
          raw_data <- hex2raw(base64_data)
          result[[3]] <- c(result[[3]], list(raw_data))
        }
      }
      return(result)
    }
  )
)

#' Internal helper method for converting a hex string to raw vector.
#'
#' @param hex_string hexadecimal string to be converted to raw vector
#'
#' @keywords internal

hex2raw <- function(hex_string) {
  # Remove non-hex characters
  hex_string <- gsub("[^0-9a-fA-F]", "", hex_string)

  # check that the input string is a hex string
  if (nchar(hex_string) %% 2 != 0 || nchar(hex_string) < 2) {
    stop("input string isn't a hex string")
  }

  # Generate start and end indices
  start_indices <- seq(1, nchar(hex_string), 2)
  end_indices <- seq(2, nchar(hex_string), 2)

  # Extract byte-sized chunks
  bytes <- substring(hex_string, start_indices, end_indices)

  as.raw(as.hexmode(bytes))
}
