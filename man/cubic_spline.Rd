% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{cubic_spline}
\alias{cubic_spline}
\title{Get cubic spline between coordinates}
\usage{
cubic_spline(x, y, x_pred)
}
\arguments{
\item{x, y}{coordinates of points}

\item{x_pred}{x-coordinates at which to calculate cubic spline.}
}
\description{
Given a set of x,y coordinates, calculate the cubic spline that
  goes through all points. The solution is returned at a specified set of
  x-coordinates, which must be contained within the input x-coordinates. Both
  input and output x-coordinates must be increasing (i.e. the spline cannot
  double back).
}
