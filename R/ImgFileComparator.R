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

setClass(
  "ImgFileComparator",
  contains = "BinaryFileComparator",
  slots = list(
    file1 = "ANY",
    file2 = "ANY"
  )
)

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
  #if (is.null(comparator@image1_raw) || is.null(comparator@image2_raw)) {
    return(vrf_details_inner_from_files(comparator, file1, file2))
  #}

  #return(vrf_details_inner_from_bin(comparator, comparator@image1_raw, comparator@image2_raw))
})

#' Method for comparing the inner part for the details query with the file names
#' as the base arguments. This is a part of a group of image processing functions
#' that work with different image abstractions (file, image, raw image). These
#' methods are intended to improve the performance so that best suiting method
#' version can be used depending on what data is available from the earlier
#' method calls to the same comparator instance.
#'
#' @param comparator comparator instance used for the comparison
#' @param file1      first file to compare
#' @param file2      second file to compare
#'
#' @keywords internal

vrf_details_inner_from_files <- function(comparator, file1, file2) {
  image1 <- magick::image_read(file1)
  image2 <- magick::image_read(file2)

  return(vrf_details_inner_from_images(comparator, image1, image2))
}

#' Method for comparing the inner part for the details query with the image instances
#' as the base arguments. This is a part of a group of image processing functions
#' that work with different image abstractions (file, image, raw image). These
#' methods are intended to improve the performance so that best suiting method
#' version can be used depending on what data is available from the earlier
#' method calls to the same comparator instance.
#'
#' @param comparator comparator instance used for the comparison
#' @param image1     first image object (created with magick) to compare
#' @param image2     second image object (created with magick) to compare
#'
#' @keywords internal

vrf_details_inner_from_images <- function(comparator, image1, image2) {
  image1_raw <- magick::image_write(image1, format = "png")
  image2_raw <- magick::image_write(image2, format = "png")

  #comparator@image1_raw <- image1_raw
  #comparator@image2_raw <- image2_raw

  return(vrf_details_inner_from_bin(comparator, image1_raw, image2_raw))
}

#' Method for comparing the inner part for the details query with the raw images
#' as the base arguments. This is a part of a group of image processing functions
#' that work with different image abstractions (file, image, raw image). These
#' methods are intended to improve the performance so that best suiting method
#' version can be used depending on what data is available from the earlier
#' method calls to the same comparator instance.
#'
#' @param comparator comparator instance used for the comparison
#' @param image1_raw first image as raw binary string to compare
#' @param image2_raw second image as raw binary string to compare
#'
#' @keywords internal

vrf_details_inner_from_bin <- function(comparator, image1_raw, image2_raw) {
  image3_base64 <- NULL

  if (!identical(image1_raw, image2_raw)) {
    image1 <- magick::image_read(image1_raw)
    image2 <- magick::image_read(image2_raw)

    difference    <- magick::image_compare(image1, image2, metric = "AE")
    highlighted   <- magick::image_composite(image1, difference, operator = "atop")
    image3_raw    <- magick::image_write(highlighted, format = "png")
    image3_base64 <- paste0("data:image/png;base64,", base64enc::base64encode(image3_raw))
  }

  image1_base64 <- paste0("data:image/png;base64,", base64enc::base64encode(image1_raw))
  image2_base64 <- paste0("data:image/png;base64,", base64enc::base64encode(image2_raw))

  result <- list(
    type = "image",
    contents = list(
      image1 = image1_base64,
      image2 = image2_base64,
      image3 = image3_base64
    )
  )

  return(list(result))
}
