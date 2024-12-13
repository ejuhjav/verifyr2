#' ImgFileComparator.R
#'
#' Specialiced comparator for image file (jpg, jpef, png)  comparison.
#' This comparator contains the custom handling for handling only img content
#' part for the comparison.
#'
#' @include BinaryFileComparator.R
#'
#' @field image1_raw local property for storing the already extracted raw data for image1.
#' @field image2_raw local property for storing the already extracted raw data for image2.
#'
#' @examples
#'
#' # The normal way for creating a comparator would be to call the generic
#' # factory method verifyr2::create_comparator that will automatically create
#' # the correct comparator instance based on the file types.
#' 
#' file1 <- 'my_file1.jpg'
#' file1 <- 'my_file2.jpg'
#' comparator <- verifyr::create_comparator(file1, file2)
#'
#' # If needed, an explicit comparator can be created as well.
#'
#' file1 <- 'my_file1.png'
#' file1 <- 'my_file2.png'
#' comparator <- ImgFileComparator$new(file1, file2)
#'
#' # This comparator has also second explicit creation method that is used
#' # by the library for processing embedded image contents.
#'
#' image1_raw <- 'raw hex vector data'
#' image2_raw <- 'raw hex vector data'
#' comparator <- ImgFileComparator$new(NULL, NULL, image1_raw, image2_raw)
#'
#' @export
#'
ImgFileComparator <- R6Class(
  "ImgFileComparator",
  inherit = BinaryFileComparator,
  public = list(
    image1_raw = NULL,
    image2_raw = NULL,

    #' @description
    #' Initialize a ImgFileComparator instance
    #'
    #' @param file1      First file to compare.
    #' @param file2      Second file to compare.
    #' @param image1_raw First image in raw format to compare.
    #' @param image2_raw Second image in raw format to compare.
    #'
    initialize = function(file1 = NULL, file2 = NULL, image1_raw = NULL, image2_raw = NULL) {
      self$image1_raw <- image1_raw
      self$image2_raw <- image2_raw
      super$initialize(file1, file2)
    },

    #' @description
    #' Method for comparing the inner part for the details query. This method can
    #' be overwritten by more specialized comparator classes. This method is
    #' intended to be called only by the comparator classes in the processing and
    #' shouldn't be called directly by the user.
    #'
    #' @param omit    string pattern to omit from the comparison
    #' @param options additional comparator parameters
    #'
    vrf_details_inner = function(omit, options) {
      if (is.null(self$image1_raw) || is.null(self$image2_raw)) {
        return(self$vrf_details_inner_from_files())
      }

      return(self$vrf_details_inner_from_raw())
    },

    #' @description
    #' Internal method for comparing the earlier populated raw image contents in
    #' details and generating the difference highlight image in case differences
    #' are found.
    #*
    vrf_details_inner_from_raw = function() {
      image1_raw <- self$image1_raw
      image2_raw <- self$image2_raw
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
    },

    #' @description
    #' Method for comparing the inner part for the details query with the file names
    #' as the base arguments. This is a part of a group of image processing functions
    #' that work with different image abstractions (file, image, raw image). These
    #' methods are intended to improve the performance so that best suiting method
    #' version can be used depending on what data is available from the earlier
    #' method calls to the same comparator instance.
    #'
    #' @param file1 first file to compare
    #' @param file2 second file to compare
    #'
    vrf_details_inner_from_files = function(file1, file2) {
      image1 <- magick::image_read(self$file1)
      image2 <- magick::image_read(self$file2)

      self$image1_raw <- magick::image_write(image1, format = "png")
      self$image2_raw <- magick::image_write(image2, format = "png")

      return(self$vrf_details_inner_from_raw())
    }
  )
)
