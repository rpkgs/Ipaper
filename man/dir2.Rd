% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools_file.R
\name{dir2}
\alias{dir2}
\title{dir2}
\usage{
dir2(path = ".", pattern = NULL, full.names = TRUE, ...)
}
\arguments{
\item{path}{a character vector of full path names; the default
    corresponds to the working directory, \code{\link[base]{getwd}()}.  Tilde
    expansion (see \code{\link[base]{path.expand}}) is performed.  Missing
    values will be ignored.  Elements with a marked encoding will
    be converted to the native encoding (and if that fails, considered
    non-existent).}

\item{pattern}{an optional \link[base]{regular expression}.  Only file names
    which match the regular expression will be returned.}

\item{full.names}{a logical value.  If \code{TRUE}, the directory
    path is prepended to the file names to give a relative file path.
    If \code{FALSE}, the file names (rather than paths) are returned.}

\item{...}{other parameters to \code{\link[base:list.files]{base::dir()}}}
}
\description{
dir2
}
\seealso{
\code{\link[base:list.files]{base::dir()}}
}
