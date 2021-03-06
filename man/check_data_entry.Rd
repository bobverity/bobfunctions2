% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{check_data_entry}
\alias{check_data_entry}
\title{Check numerical series for basic data entry mistakes}
\usage{
check_data_entry(x1, x2, nsmall)
}
\arguments{
\item{x1, x2}{two numeric vecors.}

\item{nsmall}{number of digits to the right of decimal point that numbers are
formatted to.}
}
\description{
Compares two numeric vectors. For numbers that differ by a
  single digit, returns the "depth" of this digit from the end of the number
  (i.e. 1.234 vs. 1.334 would have a depth of 3). Identical numbers return 0,
  and numbers that differ by more than one digit return NA.
}
\details{
A clue that a data entry mistake has occurred is if two numbers are
  identical at all digits except for a single digit. This clue is stronger if
  the digit is towards the middle of the number, as the chance of two random
  numbers being identical at many subsequent digits is small. This function
  can be used to flag these values which can then be checked by hand more
  easily.
}
