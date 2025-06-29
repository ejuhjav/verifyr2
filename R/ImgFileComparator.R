#' ImgFileComparator.R
#'
#' Specialiced comparator for image file (jpg, jpef, png)  comparison.
#' This comparator contains the custom handling for handling only img content
#' part for the comparison.
#'
#' @import base64enc
#'
#' @include BinaryFileComparator.R
#'
#' @field image1_raw extracted raw data for image1.
#' @field image2_raw extracted raw data for image2.
#'
#' @examples
#'
#' # The normal way for creating a comparator would be to call the generic
#' # factory method verifyr2::create_comparator that will automatically create
#' # the correct comparator instance based on the file types.
#'
#' file1 <- 'my_file1.jpg'
#' file2 <- 'my_file2.jpg'
#' comparator <- verifyr2::create_comparator(file1, file2)
#'
#' # If needed, an explicit comparator can be created as well.
#'
#' file1 <- 'my_file1.png'
#' file2 <- 'my_file2.png'
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
ImgFileComparator <- R6::R6Class(
  "ImgFileComparator",
  inherit = BinaryFileComparator,
  public = list(
    image1_raw = NULL,
    image2_raw = NULL,

    #' @description
    #' Initialize a ImgFileComparator instance
    #'
    #' @param file1 First file to compare.
    #' @param file2 Second file to compare.
    #' @param raw1  First image in raw format to compare.
    #' @param raw2  Second image in raw format to compare.
    #'
    initialize = function(
      file1 = NULL,
      file2 = NULL,
      raw1  = NULL,
      raw2  = NULL
    ) {
      self$image1_raw <- raw1
      self$image2_raw <- raw2
      super$initialize(file1, file2)
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
      self$vrf_open_debug("Img::vrf_details_inner", options)

      if ("no" == super$vrf_option_value(options, "generic.images")) {
        result <- list(
          list(
            type = "text",
            contents = "Image comparison disabled; no comparison done."
          )
        )

        self$vrf_close_debug()
        return(result)
      }

      if (is.null(self$image1_raw) || is.null(self$image2_raw)) {
        result <- self$vrf_details_inner_from_files(options)

        self$vrf_close_debug()
        return(result)
      }

      result <- self$vrf_details_inner_from_raw(options)

      self$vrf_close_debug()
      return(result)
    },

    #' @description
    #' Internal method for comparing the earlier populated raw image contents in
    #' details and generating the difference highlight image in case differences
    #' are found.
    #'
    #' @param options additional comparator parameters
    #*
    vrf_details_inner_from_raw = function(options) {
      self$vrf_open_debug("Img::vrf_details_inner_from_raw", options)

      png_prefix <- "data:image/png;base64,"
      image1_raw <- self$image1_raw
      image2_raw <- self$image2_raw
      image3_base64 <- NULL

      if (!identical(image1_raw, image2_raw)) {
        image1 <- magick::image_read(image1_raw)
        image2 <- magick::image_read(image2_raw)

        diff      <- magick::image_compare(image1, image2, metric = "AE")
        highlight <- magick::image_composite(image1, diff, operator = "atop")

        image3_raw    <- magick::image_write(highlight, format = "png")
        image3_base64 <- paste0(png_prefix, base64enc::base64encode(image3_raw))
      }

      image1_base64 <- paste0(png_prefix, base64enc::base64encode(image1_raw))
      image2_base64 <- paste0(png_prefix, base64enc::base64encode(image2_raw))

      result <- list(
        type = "image",
        contents = list(
          image1 = image1_base64,
          image2 = image2_base64,
          image3 = image3_base64
        )
      )

      self$vrf_close_debug()
      return(list(result))
    },

    #' @description
    #' Method for comparing the inner part for the details query with the file
    #' names as the base arguments. This is a part of a group of image
    #' processing functions that work with different image abstractions (file,
    #' image, raw image). These methods are intended to improve the performance
    #' so that best suiting method version can be used depending on what data is
    #' available from the earlier method calls to the same comparator instance.
    #'
    #' @param options additional comparator parameters
    #'
    vrf_details_inner_from_files = function(options) {
      self$vrf_open_debug("Img::vrf_details_inner_from_files", options)

      file1_size <- file.info(self$file1)$size
      self$image1_raw <- readBin(self$file1, what = "raw", n = file1_size)

      file2_size <- file.info(self$file2)$size
      self$image2_raw <- readBin(self$file2, what = "raw", n = file2_size)

      result <- self$vrf_details_inner_from_raw(options)

      self$vrf_close_debug()
      return(result)
    },

    #' @description
    #' Inherited method for indicating whether detailed comparison is available
    #' with the current comparator. Returns an empty string if the comparator is
    #' is supported, otherwise a string that will be concatenated with the
    #' summary string.
    #'
    #' @param options additional comparator parameters
    #'
    vrf_details_supported = function(options) {
      if ("no" == super$vrf_option_value(options, "generic.images")) {
        return("Image details comparison disabled.")
      }
      return("")
    }
  )
)
