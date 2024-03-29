% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FileComparator.R
\name{vrf_comparator}
\alias{vrf_comparator}
\title{Factory method for creating comparator instance based on the given two files.}
\usage{
vrf_comparator(file1, file2)
}
\arguments{
\item{file1}{first file to compare}

\item{file2}{second file to compare}
}
\description{
Factory method for creating comparator instance based on the given two files.
}
\examples{

# instantiating the compared files
file1 <- paste0(fs::path_package("/extdata/base_files/file1.rtf",
                                 package = "verifyr2"))

file2 <- paste0(fs::path_package("/extdata/compare_files/file1.rtf",
                                 package = "verifyr2"))

file3 <- file1
file4 <- file2

# instantiating a new comparator instance for every comparison:
verifyr2::vrf_summary(verifyr2::vrf_comparator(file1, file2))
verifyr2::vrf_details(verifyr2::vrf_comparator(file1, file2))

# instantiating a comparator instance and using that same for both
# comparison of same files
comparator <- verifyr2::vrf_comparator(file1, file2)
verifyr2::vrf_summary(comparator)
verifyr2::vrf_details(comparator)

# instantiating an explicit comparator manually when comparing files of
# single specific type
comparator <- new("RtfFileComparator")
verifyr2::vrf_summary(comparator, file1, file2)
verifyr2::vrf_summary(comparator, file3, file4)

}
