
<div align="center">
  <img src="./man/figures/verifyr2.png" alt="Project Logo" width="200">
</div>

# verifyr2

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

instantiating a new comparator instance for every comparison:

``` bash
> verifyr2::vrf_summary(verifyr2::vrf_comparator(file1, file2), omit = omit}
> verifyr2::vrf_details(verifyr2::vrf_comparator(file1, file2), omit = omit)
```

instantiating a comparator instance and using that same for both comparison of same files

``` bash
> comparator <- verifyr2::vrf_comparator(file1, file2)
> verifyr2::vrf_summary(comparator, omit = omit)
> verifyr2::vrf_details(comparator, omit = omit)
```

instantiating an explicit comparator manually when comparing files of single specific type

``` bash
> comparator <- new("RtfFileComparator")
> verifyr2::vrf_summary(comparator, file1, file2, omit = omit)
> verifyr2::vrf_details(comparator, file3, file4, omit = omit)
```

## Adding support to additional file types

The file comparison logic is implemented with a generic structure that supports easy addition of
new comparison logic for different file types. In most cases, this will simply require an implementation
of a new file comparator with the file extension name and implementing the file contents getter. The included
implementation for pdf file comparison is an example of how to do this (see source file for full documentation).

``` bash
#' PdfFileComparator.R
#'
#' @include TxtFileComparator.R
#'
#' @export

setClass("PdfFileComparator",
         contains = "TxtFileComparator",
         slots = list(file1 = "ANY", file2 = "ANY"))

setMethod("vrf_contents", "PdfFileComparator", function(comparator, file, omit, options) {
  content <- pdftools::pdf_text(file)
  content <- paste(content, collapse = "")
  content <- strsplit(content, "\n")[[1]]

  return(vrf_contents_inner(comparator, content, omit, options))
})
```

## Credits

Verifyr2 implementation is based on the concept of the [verifyr package](https://github.com/novartis/verifyr).

