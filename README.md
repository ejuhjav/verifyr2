
# verifyr2<img src="./man/figures/verifyr2.png" align="right" height="223" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/verifyr2)](https://cran.r-project.org/package=verifyr2)
![Coverage](https://github.com/ejuhjav/verifyr2/actions/workflows/check-package.yml/badge.svg)
[![Coverage Status](https://ejuhjav.github.io/verifyr2/coverage.svg)](https://ejuhjav.github.io/verifyr2/coverage/)
<!-- badges: end -->

Verifyr2 is an R package for extendable file comparison classes and methods including an
example 'shiny' app for combining the functions into a convenient application.

## Package installation

The project is available in cran and can be installed with the following command:

``` bash
> install.packages("verifyr2")
```

### Development version

The latest development version can be installed from github using different install packages
that work with github projects. One such option would be to use the 'devtools' package:

``` bash
> install.packages("devtools")
> library("devtools")
> install_github("ejuhjav/verifyr2")
```

### Extended functionality

Following additional libraries can be installed to extend the provided comparison functionality:

``` bash
> # image comparison functionality
> install.packages("magick")
>
> # pdf comparison functionality
> install.packages("pdftools")
```

## Shiny app usage

Launching the 'shiny' app requires that you have the following additional libraries installed as
documented in the suggestion list; 'shinyjs', 'shinyFiles', and 'DT'. After all the necessary
libraries are available, the included 'shiny' app can be launched with the following command:

``` bash
> verifyr2::run_example()
```

## Shiny app configuration options

### Generic options - Debugging enabled

Option to enable debugging so that the application prints diagnostic information to the output 
stream. This option is available to all users and can be enabled to include debugging details 
when reporting issues.

### Generic options - Process embedded images

Option to define whether embedded images should be processed for all supported file types. 
Processing embedded images requires additional time to detect and extract images from documents, 
so it may be advisable to disable this option when working with large files or a high volume of 
files that do not contain images.

### RTF comparison - Process embedded images

Option to define whether embedded image processing should be applied specifically to RTF files. 
Currently, this option is redundant because RTF is the only file type that supports embedded 
image processing. However, it is intended to allow configuration per file type in addition to 
the general setting.

### PDF comparison - Process PDF detailed comparison

Option to define whether detailed PDF comparison should be performed. Disabling this option is 
generally not recommended for users. However, it may be automatically disabled by the 
application if the required 'pdftools' package is not available.

### Details comparison - Mode

Option to define the default display mode for the details summary view. The available options 
are “full,” which displays the complete compared file, and “summary,” which displays only the 
detected differences.

## Creating and calling the separators manually

``` bash
> config <- Config$new()
> comparator <- verifyr2::create_comparator(file1, file2)
> comparator$vrf_summary(config = config)
> comparator$vrf_details(config = config)
```

instantiating an explicit comparator manually

``` bash
> config <- Config$new()
> comparator <- RtfFileComparator$new(file1, file2)
> comparator$vrf_summary(config = config)
> comparator$vrf_details(config = config)
```

## Optional features

Image comparison features - comparing single images as well as supported embedded images in file
contents - requires additional 'magick' library to be installed. This is defined as an optional
requirement for the 'verifyr2' package due to the additional OS level requirements of the package.
For more details on the package installation, see [magick package](https://github.com/ropensci/magick).

PDF comparison features - reading PDF file contents as text for details comparison - requires
additional 'pdftools' library to be installed. This is defined as an optional requirement for the
'verifyr2' package due to the additional OS level requirements of the package. For more details on
the package installation, see [pdftools package](https://github.com/ropensci/pdftools).

## Adding support to additional file types

The file comparison logic is implemented with a generic structure that supports easy addition of
new comparison logic for different file types. In most cases, this will simply require an implementation
of a new file comparator with the file extension name and implementing the file contents getter. The included
simplified implementation for pdf file comparison - without testing pdftools availability and excluding the
logging parts - is an example of how to do this (see source file to see the full implementation with
documentation).

When using the provided generic create_comparator function along with your custom comparator class implementation, 
note that the comparator class name prefix must match with the compared file type. 

``` bash
#' PdfFileComparator.R
#'
#' @include TxtFileComparator.R
#'
#' @export
#'
PdfFileComparator <- R6::R6Class(
  "PdfFileComparator",
  inherit = TxtFileComparator,
  public = list(
    vrf_contents = function(file, config, omit) {
      content <- pdftools::pdf_text(file)
      content <- paste(content, collapse = "")
      content <- strsplit(content, "\n")[[1]]

      self$vrf_contents_inner(content, omit, options)
    }
  )
)

```

## Credits

Verifyr2 implementation is based on the concept of the [verifyr package](https://github.com/novartis/verifyr).


