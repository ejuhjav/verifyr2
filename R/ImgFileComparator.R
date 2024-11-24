#' ImgFileComparator.R
#'
#' Specialiced comparator for image file (jpg, jpef, png)  comparison.
#' This comparator contains the custom handling for handling only img content
#' part for the comparison.
#'
#' @include BinaryFileComparator.R
#'
#' @examples
#'
#' # The normal way for creating a comparator would be to call the generic
#' # factory method verifyr2::vrf_comparator, but if needed, an explicit
#' # comparator can be instantiated directly as well.
#' comparator <- new("ImgFileComparator")
#'
#' @export

setClass("ImgFileComparator",
         contains = "BinaryFileComparator",
         slots = list(file1 = "ANY", file2 = "ANY"))

#' Method for getting information whether the comparator supports both summary
#' and full details comparisons. This method can be overwritten by
#' more specialized comparator classes.
#'
#' @param comparator comparator instance used for the comparison

setMethod("vrf_supports_summary_and_full", "ImgFileComparator", function(comparator) {
  return(FALSE)
})

#' Method for comparing the inner part for the details query. This method can
#' be overwritten by more specialized comparator classes. This method is
#' intended to be called only by the comparator classes in the processing and
#' shouldn't be called directly by the user.
#'
#' @param comparator comparator instance used for the comparison
#' @param file1      first file to compare
#' @param file2      second file to compare
#' @param omit       all lines containing the omit string will be excluded from
#'                   the comparison (detaulf = NULL)
#' @param options    additional comparator parameters
#'
#' @keywords internal

setMethod("vrf_details_inner", "ImgFileComparator", function(comparator, file1, file2, omit, options) {

  image1 <- magick::image_read(file1)
  image2 <- magick::image_read(file2)

  difference <- magick::image_compare(image1, image2, metric = "AE")
  highlighted_diff <- magick::image_composite(image1, difference, operator = "atop")

  image1_raw <- magick::image_write(image1, format = "png")
  image1_base64 <- base64enc::base64encode(image1_raw)

  image2_raw <- magick::image_write(image2, format = "png")
  image2_base64 <- base64enc::base64encode(image2_raw)

  image_diff_raw <- magick::image_write(highlighted_diff, format = "png")
  image_diff_base64 <- base64enc::base64encode(image_diff_raw)

  return(list(image1 = paste0("data:image/png;base64,", image1_base64),
              image2 = paste0("data:image/png;base64,", image2_base64),
              image3 = paste0("data:image/png;base64,", image_diff_base64)))
})
