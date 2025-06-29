
# verifyr2<img src="./man/figures/verifyr2.png" align="right" height="223" />

![Coverage](https://github.com/ejuhjav/verifyr2/actions/workflows/check-package.yml/badge.svg) [![Coverage Status](https://ejuhjav.github.io/verifyr2/coverage.svg)](https://ejuhjav.github.io/verifyr2/coverage/)


Verifyr2 is an R package for extendable file comparison classes and methods including an
example shiny app for combining the functions into a convenient application.

## Package installation ##

Verifyr2 package can be installed from github using different development packages. One
such option would be to use the "devtools" package:

``` bash
> install.packages("devtools")
> library("devtools")
> install_github("ejuhjav/verifyr2")
```

Following additional libraries can be installed to extend the provided comparison functionality:

``` bash
> # image comparison functionality
> install.packages("magick")
>
> # pdf comparison functionality
> install.packages("pdftools")
```

## Shiny app usage ##

Shiny app launching requires that you have some of the additional libraries installed as documented
in the suggestion list; shinyjs, shinyFiles, DT. After all the necessary libraries are available, the
included shiny app can be launched with the following command:

``` bash
> verifyr2::run_example()
```

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

Image comparison features - comparing single images as well as supported embedded images in file contents - requires
additional magick library to be installed. This is defined as an optional requirement for the verifyr2 package due to
the additional OS level requirements of the package. For more details on the package installation, see
[magick package](https://github.com/ropensci/magick).

PDF comparison features - reading PDF file contents as text for details comparison - requires additional pdftools
library to be installed. This is defined as an optional requirement for the verifyr2 package due to
the additional OS level requirements of the package. For more details on the package installation, see
[pdftools package](https://github.com/ropensci/pdftools).

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


