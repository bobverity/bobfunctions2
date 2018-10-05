
#------------------------------------------------

#' @importFrom RColorBrewer brewer.pal
#' @useDynLib bobfunctions2, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#------------------------------------------------
#' @title Dummy function
#'
#' @description Simple test function that demonstrates some of the features of
#'   the bellsandwhistles package.
#'
#' @details Takes a vector of values, returns the square.
#'
#' @param x vector of values
#'
#' @export
#' @examples
#' # Find square of first 100 values
#' dummy1(1:100)

dummy1 <- function(x = 1:5) {
  
  # print message to console
  message("running R dummy1 function")
  
  # get arguments in list form
  args <- list(x = x)
  
  # run C++ function with these arguments
  output_raw <- dummy1_cpp(args)
  
  # some optional processing of output
  message("processing output")
  ret <- output_raw$x_squared
  
  # return
  return(ret)
}

#------------------------------------------------
#' @title Expand series of colours by interpolation
#'
#' @description Expand series of colours by interpolating a given vector of raw
#'   colours. The pattern of interpolation is designed so that (n+1)th
#'   interpolation contains the nth interpolation plus one more colour, rather
#'   than being a completely different series.
#'
#' @details Takes a vector of values, returns the square.
#'
#' @param n how many colours to return
#' @param raw_cols vector of colours to interpolate
#'
#' @export

more_colours <- function(n, raw_cols = brewer.pal(10, "Paired")) {
  
  # check inputs
  assert_single_pos_int(n, zero_allowed = FALSE)
  assert_string(raw_cols)
  assert_vector(raw_cols)
  
  # generate colour palette from raw colours
  my_palette <- colorRampPalette(raw_cols)
  
  # simple case if n small
  if (n <= 2) {
    return(my_palette(3)[1:n])
  }
  
  # interpolate colours by repeatedly splitting the [0,1] interval until we have
  # enough values. n_steps is the number of times we have to do this. n_breaks
  # is the number of breaks for each step
  n_steps <- ceiling(log(n-1)/log(2))
  n_breaks <- 2^(1:n_steps) + 1
  
  # split the [0,1] interval this many times and drop duplicated values
  s <- unlist(mapply(function(x) seq(0,1,l=x), n_breaks, SIMPLIFY = FALSE))
  s <- s[!duplicated(s)]
  
  # convert s to integer index
  w <- match(s, seq(0,1,l = n_breaks[n_steps]))
  w <- w[1:n]
  
  # get final colours
  all_cols <- my_palette(n_breaks[n_steps])
  ret <- all_cols[w]
  
  return(ret)
}
