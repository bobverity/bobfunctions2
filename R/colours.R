
##########################################################################################################
# COLOUR METHODS

#------------------------------------------------
#' @title Simple plot to explore colour palettes
#'
#' @description Simple plot to explore colour palettes.
#'
#' @param x colour series
#' @param size the size of plotting circles
#'
#' @importFrom graphics text
#' @export

col_plot <- function(x, size = 8) {
  n <- length(x)
  plot(1:n, rep(0, n), col = x, pch = 20, cex = size, axes = FALSE, xlab = "", ylab = "")
  text(1:n, rep(0, n), labels = 1:n)
}

#------------------------------------------------
#' @title Expand series of colours by interpolation
#'
#' @description Expand a series of colours by interpolation to produce any 
#'   number of colours from a given series. The pattern of interpolation is 
#'   designed so that (n+1)th value contains the nth value plus one more colour,
#'   rather than being a completely different series. For example, running
#'   \code{more_colours(5)} and \code{more_colours(4)}, the first 4 colours will
#'   be shared between the two series.
#'
#' @param n how many colours to return
#' @param raw_cols vector of colours to interpolate
#'
#' @importFrom RColorBrewer brewer.pal
#' @export

more_colours <- function(n = 5, raw_cols = RColorBrewer::brewer.pal(10, "Paired")) {
  
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

##########################################################################################################
# COLOUR SETS

#------------------------------------------------
#' @title Red to blue colours
#'
#' @description Red-to-blue colours.
#'
#' @param n the number of colours
#'
#' @importFrom grDevices colorRampPalette
#' @export

col_hotcold <- function(n = 6) {
  raw_cols <- c("#D73027", "#FC8D59", "#FEE090", "#E0F3F8", "#91BFDB", "#4575B4")
  my_pal <- colorRampPalette(raw_cols)
  return(my_pal(n))
}

#------------------------------------------------
#' @title tim.colors from fields package
#'
#' @description Blue-to-red colours. Full credit to tim.colors from the fields package, from 
# which these colours derive. Copied rather than including the fields package to
# avoid dependency on another package for the sake of a single colour scheme.
#'
#' @param n the number of colours
#'
#' @export

col_tim <- function(n = 10) {
  raw_cols <- c("#00008F", "#00009F", "#0000AF", "#0000BF", 
                "#0000CF", "#0000DF", "#0000EF", "#0000FF", "#0010FF", 
                "#0020FF", "#0030FF", "#0040FF", "#0050FF", "#0060FF", 
                "#0070FF", "#0080FF", "#008FFF", "#009FFF", "#00AFFF", 
                "#00BFFF", "#00CFFF", "#00DFFF", "#00EFFF", "#00FFFF", 
                "#10FFEF", "#20FFDF", "#30FFCF", "#40FFBF", "#50FFAF", 
                "#60FF9F", "#70FF8F", "#80FF80", "#8FFF70", "#9FFF60", 
                "#AFFF50", "#BFFF40", "#CFFF30", "#DFFF20", "#EFFF10", 
                "#FFFF00", "#FFEF00", "#FFDF00", "#FFCF00", "#FFBF00", 
                "#FFAF00", "#FF9F00", "#FF8F00", "#FF8000", "#FF7000", 
                "#FF6000", "#FF5000", "#FF4000", "#FF3000", "#FF2000", 
                "#FF1000", "#FF0000", "#EF0000", "#DF0000", "#CF0000", 
                "#BF0000", "#AF0000", "#9F0000", "#8F0000", "#800000")
  my_pal <- colorRampPalette(raw_cols)
  return(my_pal(n))
}

#------------------------------------------------
#' @title Six colours taken from a wooden toy
#'
#' @description Six colours taken from a wooden toy.
#'
#' @export

col_toy <- function() {
  return(c("#BC3221FF", "#578851FF", "#4188BDFF", "#D1B13AFF", "#6A4560FF", "#CA6C2DFF"))
}
