
# verifyr2 - test <img src="./man/figures/verifyr2.png" align="right" height="223" />

![Coverage](https://github.com/ejuhjav/verifyr2/actions/workflows/check-package.yml/badge.svg)
![Coverage Badge](https://ejuhjav.github.io/verifyr2/coverage.svg)


Verifyr2 is an R package for extendable file comparison classes and methods including an
example shiny app for combining the functions into a convenient application.

## Package installation ##

Verifyr2 package can be installed directly from the github using different development packages. One
such option would be to use the "devtools" package:

``` bash
> install.packages("devtools")
> library("devtools")
> install_github("ejuhjav/verifyr2")
```

## Shiny app usage ##

``` bash
> verifyr2::run_example()
```

## Creating and calling the separators manually

``` bash
> comparator <- verifyr2::create_comparator(file1, file2)
> comparator$vrf_summary()
> comparator$vrf_details()
```

instantiating an explicit comparator manually

``` bash
> comparator <- RtfFileComparator$new(file1, file2)
> comparator$vrf_summary()
> comparator$vrf_details()
```

## Adding support to additional file types

The file comparison logic is implemented with a generic structure that supports easy addition of
new comparison logic for different file types. In most cases, this will simply require an implementation
of a new file comparator with the file extension name and implementing the file contents getter. The included
implementation for pdf file comparison is an example of how to do this (see source file for full documentation).

When using the provided generic create_comparator function along with your custom comparator class implementation, 
please note that the comparator class name prefix must match with the compared file type. 

``` bash
#' PdfFileComparator.R
#'
#' @import pdftools
#'
#' @include TxtFileComparator.R
#'
#' @export
#'
PdfFileComparator <- R6Class(
  "PdfFileComparator",
  inherit = TxtFileComparator,
  public = list(
    vrf_contents = function(file, omit, options) {
      content <- pdftools::pdf_text(file)
      content <- paste(content, collapse = "")
      content <- strsplit(content, "\n")[[1]]

      return(self$vrf_contents_inner(content, omit, options))
    }
  )
)

```

## Credits

Verifyr2 implementation is based on the concept of the [verifyr package](https://github.com/novartis/verifyr).


