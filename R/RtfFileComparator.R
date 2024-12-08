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

setClass("RtfFileComparator",
         contains = "TxtFileComparator",
         slots = list(file1 = "ANY", file2 = "ANY"))

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
#' @param comparator comparator instance used for the comparison
#' @param file       file for which to get the contents
#' @param omit       all lines containing the omit string will be excluded from
#'                   the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#'
#' @keywords internal

setMethod("vrf_contents", "RtfFileComparator", function(comparator, file, omit, options) {

  rtf_content <- readLines(file, warn = FALSE)
  rtf_content <- paste(rtf_content, collapse = "\n")

  # regexp for finding the png pictures from the raw RTF content.
  base64_pattern <- "\\\\pict\\\\pngblip[^{]+([A-Za-z0-9+/=]+)"
  base64_strings <- stringr::str_extract_all(rtf_content, base64_pattern)[[1]]

  if (length(base64_strings) > 0) {
    # extract the encoded image string between the start and end tags
    split_parts <- strsplit(base64_strings[1], " ")[[1]]
    base64_data_with_braces <- split_parts[2]
    base64_data <- strsplit(base64_data_with_braces, "}")[[1]][1]

    if (!is.na(base64_data) && nchar(base64_data) > 0) {
      bin_data <- hex2raw(base64_data)
      image <- magick::image_read(bin_data)

      # Save the image as a PNG file - for testing, to be removed later on.
      output_file <- "output_image.png"
      magick::image_write(image, output_file)
    }
  }

  if ("raw" == get_nested(options, "rtf", "mode")) {
    return(callNextMethod(comparator, file, omit, options))
  } else {
    contents <- striprtf::read_rtf(file = file)
    return(vrf_contents_inner(comparator, contents, omit, options))
  }
})

hex2raw <- function(hex) {
  hex <- gsub("[^0-9a-fA-F]", "", hex)
  if (length(hex) == 1) {
    if (nchar(hex) < 2 || nchar(hex) %% 2 != 0) {
      stop("hex is not a valid hexadecimal representation")
    }
    hex <- strsplit(hex, character(0))[[1]]
    hex <- paste(hex[c(TRUE, FALSE)], hex[c(FALSE, TRUE)], sep = "")
  }
  if (!all(vapply(X = hex, FUN = nchar, FUN.VALUE = integer(1)) == 2)) {
    stop("hex is not a valid hexadecimal representation")
  }
  as.raw(as.hexmode(hex))
}
