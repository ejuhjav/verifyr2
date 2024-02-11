
# verifyr2

Verifyr2 is an R package for extendable file comparison classes and methods including an
example shiny app for combining the functions into a convenient application.

## Shiny app usage ##

``` bash
> verifyr2::run_example()
```

## Creating and calling the separators manually

instantiating a new comparator instance for every comparison:

``` bash
> verifyr2::compare_files_summary(verifyr2::create_file_comparator(file1, file2), omit = omit}
> verifyr2::compare_files_details(verifyr2::create_file_comparator(file1, file2), omit = omit)
```

instantiating a comparator instance and using that same for both comparison of same files

``` bash
> comparator <- verifyr2::create_file_comparator(file1, file2)
> verifyr2::compare_files_summary(comparator, omit = omit)
> verifyr2::compare_files_details(comparator, omit = omit)
```

instantiating an explicit comparator manually when comparing files of single specific type

``` bash
> comparator <- new("RtfFileComparator")
> verifyr2::compare_files_summary(comparator, file1, file2, omit = omit)
> verifyr2::compare_files_summary(comparator, file3, file4, omit = omit)
```

## Adding support to additional file types

The file comparison logic is implemented with a generic structure that supports easy addition of
new comparison logic for different file types. In most cases, this will simply require an implementation
of a new file comparator with the file extension name and implementing the file contents getter. The included
implementation for rtf file comparison is an example of how to do this (see source file for full documentation).

``` bash
#' RtfFileComparator.R
#'
#' @include TxtFileComparator.R
#'
#' @export

setClass("RtfFileComparator", contains = "TxtFileComparator", slots = list(file1 = "ANY", file2 = "ANY"))

setMethod("compare_files_get_contents", "RtfFileComparator", function(comparator, file, omit, options, ...) {
  if (!is.null(options) &&  "rtf" %in% names(options) && "mode" %in% names(options$rtf) && "raw" == options$rtf$mode) {
    return(callNextMethod(comparator, file, omit, options, ...))
  } else {
    return(compare_files_get_contents_inner(comparator, striprtf::read_rtf(file = file), omit, options, ...))
  }
})
```

## Credits

Verifyr2 implementation is based on the concept of the [verifyr package](https://github.com/novartis/verifyr).

