% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.data.table}
\alias{print.data.table}
\title{print.data.table}
\usage{
\method{print}{data.table}(d, n = NULL, ..., maxrows = 1e+06)
}
\description{
print.data.table
}
\examples{
d <- data.table(1:100)
options(datatable.print.nrow = 100)
print(d)
}
