% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{runningId}
\alias{runningId}
\title{print the running ID in the console}
\usage{
runningId(i, step = 1, N, prefix = "")
}
\arguments{
\item{i}{the running Id.}

\item{step}{how long of print step.}

\item{N}{The number of total iteration}

\item{prefix}{prefix string}
}
\description{
print the running ID in the console
}
\examples{
for (i in 1:10) {
  runningId(i, prefix = "phenofit")
}
}
