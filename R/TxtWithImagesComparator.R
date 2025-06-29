#' TxtWithImageFileComparator.R
#'
#' "Abstract"  comparator for txt based comparator classes that can additionally
#' contain embedded images. This abstraction level contains generic logic for
#' handling embedded images and storing the related data.
#'
#' @import stringr
#'
#' @include TxtFileComparator.R
#'
#' @field file1_images_raw local property for storing image1 raw data
#' @field file2_images_raw local property for storing image2 raw data
#'

# Disable cyclomatic complexity lint for the R6 class definition as lintr
# considers the whole class definition as a single function.
#
# nolint start: cyclocomp_linter
TxtWithImagesFileComparator <- R6::R6Class(
  "TxtWithImagesFileComparator",
  inherit = TxtFileComparator,
  public = list(
    file1_images_raw = NULL,
    file2_images_raw = NULL,

    #' @description
    #' Initialize a TxtWithImagesFileComparator instance
    #'
    #' @param file1 First file to compare.
    #' @param file2 Second file to compare.
    #'
    initialize = function(file1 = NULL, file2 = NULL) {
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
    vrf_summary_inner = function(omit, options) {
      self$vrf_open_debug("TxtWithImages::vrf_summary_inner" , options)

      result <- super$vrf_summary_inner(omit, options)

      if ("no" != super$vrf_option_value(options, "generic.images")) {
        file1_contents_list <- self$file1_contents_list
        file2_contents_list <- self$file2_contents_list

        if (is.null(self$file1_images_raw)) {
          self$file1_images_raw <- self$vrf_images(self$file1, options)
        }

        if (is.null(self$file2_images_raw)) {
          self$file2_images_raw <- self$vrf_images(self$file2, options)
        }

        # Generate additional summary string based on embedded image differences
        # if applicable.
        if (0 != length(self$file1_images_raw) && 0 != length(self$file2_images_raw)) {
          result_images <- "No differences in embedded images."
  
          if (length(self$file1_images_raw) != length(self$file2_images_raw)) {
            # Number of found embedded images differs between the files.
            result_images <- "Different amount of embedded images."
          } else {
            # Number of found embedded images is the same; calculate how many of
            # the embedded images has changed (based on raw file data) compared to
            # total count.
            matches <- 0
            total <- length(self$file1_images_raw)

            for (index in seq_along(self$file1_images_raw)) {
              if (identical(self$file1_images_raw[[index]], self$file2_images_raw[[index]])) {
                matches <- matches + 1
              }
            }

            if (matches != length(self$file1_images_raw)) {
              result_images <- paste0(total - matches, "/", total, " embedded images have differences.")
            }
          }
          result <- paste0(result, " ", result_images)
        }
      }

      self$vrf_close_debug()
      return(result)
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
      self$vrf_open_debug("TxtWithImages::vrf_details_inner" , options)

      result <- super$vrf_details_inner(omit, options)

      if ("no" != super$vrf_option_value(options, "generic.images")) {
        file1_contents_list <- self$file1_contents_list
        file2_contents_list <- self$file2_contents_list

        if (is.null(self$file1_images_raw)) {
          self$file1_images_raw <- self$vrf_images(self$file1, options)
        }

        if (is.null(self$file2_images_raw)) {
          self$file2_images_raw <- self$vrf_images(self$file2, options)
        }

        # Append the possible extended images into the result list if applicable.
        # List of images is included in the fileX_contents_lists if found from
        # content getter.
        if (0 != length(self$file1_images_raw) && 0 != length(self$file2_images_raw)) {

          # Only display the differences if there is the same amount of images
          # found from the compared files. Otherwise it would require additional
          # logic to decide which files should be compared with each others (which
          # is something that could be developed further with size comparisons).
          if (length(self$file1_images_raw) == length(self$file2_images_raw)) {
            for (index in seq_along(self$file1_images_raw)) {
              # Manually create a ImgFileComparator instance for every embedded
              # image and call the details comparison based on existing bin data.
              comparator <- ImgFileComparator$new(
                NULL,
                NULL,
                self$file1_images_raw[[index]],
                self$file2_images_raw[[index]]
              )
              result <- append(result, comparator$vrf_details_inner(omit, options))
            }
          }
        }
      }

      self$vrf_close_debug()
      return(result)
    },

    #' @description
    #' "Abstract" method for getting the raw image hex vector array from the
    #' given source file.
    #'
    #' @param file file for which to get the embedded image details
    #'
    vrf_images = function(file) {
      stop("vrf_images must be implemented in a subclass.")
    },

    #' @description
    #' Internal helper method for converting a hex string to raw vector.
    #'
    #' @param hex_string hexadecimal string to be converted to raw vector
    #'
    hex2raw = function(hex_string) {
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
  )
)
# nolint end: cyclocomp_linter
