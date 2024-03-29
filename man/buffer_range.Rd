% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{buffer_range}
\alias{buffer_range}
\title{Expand range to include buffer}
\usage{
buffer_range(x_min, x_max, buffer = 1.1)
}
\arguments{
\item{x_min, x_max}{minimum and maximum of starting range.}

\item{buffer}{the proportional increase in the starting range.}
}
\description{
Expand range to include buffer. The buffer distance is defined
  as a proportion of the range.
}
