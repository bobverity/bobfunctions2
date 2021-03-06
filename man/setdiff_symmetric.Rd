% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{setdiff_symmetric}
\alias{setdiff_symmetric}
\title{Find which elements are not shared between two sets}
\usage{
setdiff_symmetric(x, y)
}
\arguments{
\item{x, y}{vectors (of the same mode) containing a sequence of items.}
}
\description{
The base \code{setdiff(vec1, vec2)} function is asymmetric in
  the sense that it returns elements of \code{vec1} that are not found within
  \code{vec2}, but it does not perform the reverse comparison. In contrast,
  \code{setdiff_symmetric(vec1, vec2)} is symmetric in that it returns any
  elements that are found in one vector but not the other.
}
